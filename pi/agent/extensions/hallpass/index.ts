/**
 * hallpass — profile-driven sandboxing for pi's bash tool.
 *
 * Chunk 1 (this file): profile schema + state, bash replacement with per-call
 * srt wrapping, file-tool path guard, /hallpass command, status indicator.
 * See DESIGN.md for the full plan (network allowlists, subagents, tmux-mcp).
 *
 * State model:
 * - Current profile is per-session closure state (never module-level: module
 *   state may be shared across in-process sessions).
 * - Switches are persisted via pi.appendEntry("hallpass:profile", …) and
 *   restored from the active branch on session_start, so profile state
 *   survives resume and follows tree navigation.
 * - The bash override reads the current profile at call time, so a switch
 *   takes effect on the very next tool call.
 */

import process from "node:process";
import {
	createBashTool,
	createLocalBashOperations,
	type ExtensionAPI,
	type ExtensionContext,
} from "@earendil-works/pi-coding-agent";
import { assignedProfileName, discoverAgents, subagentTypeFromSessionName } from "./agents.ts";
import { explainFailure, looksLikeFsDenial, OutputTail } from "./diagnose.ts";
import { installFileToolGuard } from "./guard.ts";
import { createTmuxProfileSync } from "./tmux.ts";
import { createToolSelection, installToolBlocker } from "./tools.ts";
import {
	loadConfig,
	summarizeProfile,
	toSrtFilesystemConfig,
	type HallpassConfig,
	type Profile,
} from "./profiles.ts";
import {
	ensureNetworkFloor,
	LOG_MONITOR_ENABLED,
	setDeniedDomains,
	setNetworkFloor,
	wrapCommand,
} from "./sandbox.ts";

const ENTRY_TYPE = "hallpass:profile";

export default function hallpass(pi: ExtensionAPI) {
	// The session's cwd, refreshed at session_start: subagents with worktree isolation run
	// in a different directory than the process was started in.
	let cwd = process.cwd();
	let config: HallpassConfig = {
		defaultProfile: "",
		profiles: new Map(),
		agentProfiles: {},
		deniedDomains: [],
		warnings: ["hallpass config not loaded yet"],
	};
	let currentName: string | undefined;
	let isChild = false;
	const toolSelection = createToolSelection(pi);
	const tmuxSync = createTmuxProfileSync(pi);

	function currentProfile(): Profile | undefined {
		if (currentName) {
			const p = config.profiles.get(currentName);
			if (p) return p;
		}
		if (config.defaultProfile) return config.profiles.get(config.defaultProfile);
		return undefined;
	}

	function updateStatus(ctx: ExtensionContext): void {
		if (!ctx.hasUI) return;
		const profile = currentProfile();
		let text: string;
		if (!profile) {
			text = ctx.ui.theme.fg("muted", "hallpass: inactive");
		} else if (profile.sandbox) {
			text = ctx.ui.theme.fg("accent", `hallpass: ${profile.name}`);
		} else {
			text = ctx.ui.theme.fg("warning", `hallpass: ${profile.name} (off)`);
		}
		ctx.ui.setStatus("hallpass", text);
	}

	function switchProfile(name: string, ctx: ExtensionContext): string {
		const profile = config.profiles.get(name);
		if (!profile) {
			return `no profile "${name}" (have: ${[...config.profiles.keys()].join(", ") || "none"})`;
		}
		currentName = name;
		pi.appendEntry(ENTRY_TYPE, { profile: name });
		// Network "open" is a process-wide floor in chunk 1 (see sandbox.ts);
		// explicit switches move it. Fs policy stays exactly per-call.
		setNetworkFloor(profile.network.mode);
		toolSelection.apply(profile);
		tmuxSync.sync(profile);
		updateStatus(ctx);
		return `switched to profile "${name}" — effective on the next tool call`;
	}

	// -----------------------------------------------------------------------
	// Session lifecycle: load config, restore profile from the active branch
	// -----------------------------------------------------------------------

	pi.on("session_start", async (_event, ctx) => {
		if (ctx.cwd !== cwd) {
			cwd = ctx.cwd;
			localBash = createBashTool(cwd);
		}
		config = loadConfig(cwd);

		let restored: string | undefined;
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
				const data = entry.data as { profile?: unknown } | undefined;
				if (typeof data?.profile === "string") restored = data.profile;
			}
		}

		// Subagent child session? Its profile comes from agent frontmatter / [agentProfiles].
		const childType = subagentTypeFromSessionName(pi.getSessionName());
		const assigned = childType
			? assignedProfileName(childType, discoverAgents(cwd), config.agentProfiles)
			: undefined;
		if (childType) {
			if (assigned && !config.profiles.has(assigned)) {
				ctx.ui.notify(`hallpass: subagent "${childType}" assigned unknown profile "${assigned}"; using default`, "warning");
			}
			isChild = true;
		}
		if (assigned && config.profiles.has(assigned)) {
			currentName = assigned;
		} else if (restored && config.profiles.has(restored)) {
			currentName = restored;
		} else if (config.profiles.has(config.defaultProfile)) {
			currentName = config.defaultProfile;
		} else {
			currentName = [...config.profiles.keys()][0];
		}

		setDeniedDomains(config.deniedDomains);
		// A child must not move the process-wide network floor (it would change the parent's network).
		if (!isChild) {
			ensureNetworkFloor(currentProfile()?.network.mode);
			// Children's tool scope belongs to pi-subagents; the tool_call blocker covers them.
			toolSelection.apply(currentProfile());
		}
		// Children have their own event bus and tmux-mcp instance, so they sync too.
		tmuxSync.syncAtStart(currentProfile());
		updateStatus(ctx);
		for (const warning of config.warnings) {
			ctx.ui.notify(`hallpass: ${warning}`, "warning");
		}
	});

	// tmux-mcp's own session restore can run after ours (extension load order); re-sync once at the
	// first prompt after session start so the hallpass profile's tmux selection wins. Explicit
	// selections (/hp-profile, reload) don't arm this, so a shift+tab cycle afterwards sticks.
	pi.on("before_agent_start", () => {
		tmuxSync.syncIfPending(currentProfile());
		return undefined;
	});

	// -----------------------------------------------------------------------
	// Parent side: refuse to spawn subagents that would escape hallpass
	// -----------------------------------------------------------------------

	pi.on("tool_call", (event) => {
		if (event.toolName !== "Agent") return undefined;
		const profile = currentProfile();
		if (!profile?.sandbox) return undefined;
		const input = event.input as { subagent_type?: unknown; isolated?: unknown };
		// The Agent tool's per-call `isolated: true` strips extensions (hallpass included) from the
		// child regardless of the agent file, so it must be refused under a sandboxed profile too.
		if (input.isolated === true) {
			return {
				block: true,
				reason:
					`hallpass: spawning an agent with isolated=true would run it without hallpass (unsandboxed) ` +
					`while profile "${profile.name}" is active. Retry without isolated, or ask the user to switch ` +
					`to an unsandboxed profile.`,
			};
		}
		const type = input.subagent_type;
		if (typeof type !== "string") return undefined;
		const info = discoverAgents(cwd).get(type.toLowerCase());
		if (info && !info.hallpassLoads) {
			return {
				block: true,
				reason:
					`hallpass: agent "${info.name}" is configured so hallpass does not load in its session ` +
					`(extensions disabled/isolated/excluded), which would run it unsandboxed while profile ` +
					`"${profile.name}" is active. Ask the user to change the agent or switch to an unsandboxed profile.`,
			};
		}
		return undefined;
	});

	// -----------------------------------------------------------------------
	// Interactive /hp command family
	// -----------------------------------------------------------------------

	function requireInteractiveUI(ctx: ExtensionContext): boolean {
		if (ctx.mode === "tui") return true;
		ctx.ui.notify("This hallpass command requires interactive TUI mode.", "warning");
		return false;
	}

	function profileChoices(): Array<{ label: string; profile: Profile }> {
		return [...config.profiles.values()].map((profile) => {
			const active = profile.name === currentName ? "● active" : "";
			const description = profile.description ? ` — ${profile.description}` : "";
			return { label: `${profile.name}${active ? `  ${active}` : ""}${description}`, profile };
		});
	}

	async function chooseProfile(ctx: ExtensionContext, title = "Select hallpass profile"): Promise<void> {
		if (!requireInteractiveUI(ctx)) return;
		const items = profileChoices();
		if (items.length === 0) {
			ctx.ui.notify("No profiles defined in hallpass.toml", "warning");
			return;
		}
		const selected = await ctx.ui.select(title, items.map((item) => item.label));
		if (!selected) return;
		const item = items.find((candidate) => candidate.label === selected);
		if (!item) return;
		const message = switchProfile(item.profile.name, ctx);
		ctx.ui.notify(`hallpass: ${message}`, "info");
	}

	async function showProfileDetails(ctx: ExtensionContext, profile = currentProfile()): Promise<void> {
		if (!profile) {
			ctx.ui.notify("hallpass: no active profile (bash unsandboxed)", "warning");
			return;
		}
		ctx.ui.notify(summarizeProfile(profile), "info");
	}

	pi.registerCommand("hp", {
		description: "Hallpass profile dashboard",
		handler: async (_args, ctx) => {
			if (!requireInteractiveUI(ctx)) return;
			const selected = await ctx.ui.select("Hallpass", [
				`Status${currentName ? ` — ${currentName}` : " — inactive"}`,
				"Switch profile…",
				"Reload configuration",
			]);
			if (!selected) return;
			if (selected.startsWith("Status")) {
				await showProfileDetails(ctx);
			} else if (selected === "Switch profile…") {
				await chooseProfile(ctx);
			} else if (selected === "Reload configuration") {
				reloadConfiguration(ctx);
			}
		},
	});

	pi.registerCommand("hp-status", {
		description: "Show the active hallpass profile",
		handler: async (_args, ctx) => {
			await showProfileDetails(ctx);
		},
	});

	pi.registerCommand("hp-profile", {
		description: "Select the active hallpass profile",
		handler: async (args, ctx) => {
			if (!requireInteractiveUI(ctx)) return;
			const requested = args.trim();
			if (requested) {
				const profile = config.profiles.get(requested);
				if (!profile) {
					ctx.ui.notify(`Unknown profile "${requested}". Run /hp-profile to pick one.`, "error");
					return;
				}
				const message = switchProfile(requested, ctx);
				ctx.ui.notify(`hallpass: ${message}`, "info");
				return;
			}
			await chooseProfile(ctx);
		},
	});

	pi.registerCommand("hp-reload", {
		description: "Reload hallpass.toml configuration",
		handler: async (_args, ctx) => {
			reloadConfiguration(ctx);
		},
	});

	function reloadConfiguration(ctx: ExtensionContext): void {
		const previousName = currentName;
		config = loadConfig(cwd);
		if (!currentName || !config.profiles.has(currentName)) {
			currentName = config.profiles.has(config.defaultProfile)
				? config.defaultProfile
				: [...config.profiles.keys()][0];
		}
		setDeniedDomains(config.deniedDomains);
		const profile = currentProfile();
		if (profile) {
			setNetworkFloor(profile.network.mode);
			if (!isChild) toolSelection.apply(profile);
			tmuxSync.sync(profile);
			if (currentName !== previousName) pi.appendEntry(ENTRY_TYPE, { profile: currentName });
		}
		updateStatus(ctx);
		ctx.ui.notify(
			`Reloaded ${config.profiles.size} profile(s)${currentName ? `; active: ${currentName}` : "; no active profile"}`,
			config.warnings.length ? "warning" : "info",
		);
		for (const warning of config.warnings) ctx.ui.notify(`hallpass: ${warning}`, "warning");
	}

	// -----------------------------------------------------------------------
	// bash tool override: per-call srt wrap (Gondolin pattern)
	// -----------------------------------------------------------------------

	let localBash = createBashTool(cwd);

	pi.registerTool({
		...localBash,
		async execute(id, params, signal, onUpdate) {
			const profile = currentProfile();
			if (!profile || !profile.sandbox) {
				return localBash.execute(id, params, signal, onUpdate);
			}
			let wrapped: Awaited<ReturnType<typeof wrapCommand>>;
			try {
				wrapped = await wrapCommand(
					params.command,
					toSrtFilesystemConfig(profile, cwd),
					profile.network,
					signal,
					id,
				);
			} catch (err) {
				const message = err instanceof Error ? err.message : String(err);
				throw new Error(
					`hallpass: failed to sandbox command (profile "${profile.name}"): ${message}\n` +
						`Fix the sandbox setup, or ask the user to switch profiles (/hp-profile).`,
				);
			}
			try {
				const result = await localBash.execute(id, { ...params, command: wrapped.command }, signal, onUpdate);
				if (result.isError) {
					const first = result.content[0];
					const output = first?.type === "text" ? first.text : "";
					// Only the optional log monitor delivers fs denials, asynchronously; wait briefly for it.
					const violations = await wrapped.violations(LOG_MONITOR_ENABLED && looksLikeFsDenial(output) ? 300 : 0);
					const extra = explainFailure(profile, output, violations);
					if (extra && first?.type === "text") {
						result.content[0] = { ...first, text: `${first.text}\n\n${extra}` };
					}
				}
				return result;
			} finally {
				wrapped.cleanup();
			}
		},
	});

	// -----------------------------------------------------------------------
	// File-tool path guard
	// -----------------------------------------------------------------------

	installFileToolGuard(pi, currentProfile, () => cwd);
	installToolBlocker(pi, currentProfile);

	// -----------------------------------------------------------------------
	// User `!` / `!!` shell (opt-in per profile)
	// -----------------------------------------------------------------------

	pi.on("user_bash", async () => {
		const profile = currentProfile();
		if (!profile?.sandbox || !profile.sandboxUserBash) return undefined;
		const local = createLocalBashOperations();
		return {
			operations: {
				exec: async (command, commandCwd, options) => {
					const wrapped = await wrapCommand(
						command,
						toSrtFilesystemConfig(profile, commandCwd),
						profile.network,
						options.signal,
						`user-${crypto.randomUUID()}`,
					);
					const tail = new OutputTail();
					try {
						const result = await local.exec(wrapped.command, commandCwd, {
							...options,
							onData: (data) => {
								tail.push(data);
								options.onData(data);
							},
						});
						if (result.exitCode !== 0) {
							const output = tail.toString();
							const violations = await wrapped.violations(
								LOG_MONITOR_ENABLED && looksLikeFsDenial(output) ? 300 : 0,
							);
							const extra = explainFailure(profile, output, violations);
							if (extra) options.onData(Buffer.from(`\n${extra}\n`));
						}
						return result;
					} finally {
						wrapped.cleanup();
					}
				},
			},
		};
	});
}
