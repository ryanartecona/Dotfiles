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
import { explainFailure, looksLikeFsDenial, OutputTail } from "./diagnose.ts";
import { installFileToolGuard } from "./guard.ts";
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
	const cwd = process.cwd();
	let config: HallpassConfig = {
		defaultProfile: "",
		profiles: new Map(),
		agentProfiles: {},
		deniedDomains: [],
		warnings: ["hallpass config not loaded yet"],
	};
	let currentName: string | undefined;

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
		updateStatus(ctx);
		return `switched to profile "${name}" — effective on the next tool call`;
	}

	// -----------------------------------------------------------------------
	// Session lifecycle: load config, restore profile from the active branch
	// -----------------------------------------------------------------------

	pi.on("session_start", async (_event, ctx) => {
		config = loadConfig(cwd);

		let restored: string | undefined;
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
				const data = entry.data as { profile?: unknown } | undefined;
				if (typeof data?.profile === "string") restored = data.profile;
			}
		}

		if (restored && config.profiles.has(restored)) {
			currentName = restored;
		} else if (config.profiles.has(config.defaultProfile)) {
			currentName = config.defaultProfile;
		} else {
			currentName = [...config.profiles.keys()][0];
		}

		setDeniedDomains(config.deniedDomains);
		ensureNetworkFloor(currentProfile()?.network.mode);
		updateStatus(ctx);
		for (const warning of config.warnings) {
			ctx.ui.notify(`hallpass: ${warning}`, "warning");
		}
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

	const localBash = createBashTool(cwd);

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

	installFileToolGuard(pi, currentProfile, cwd);

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
