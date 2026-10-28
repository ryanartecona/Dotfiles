/**
 * hallpass — composable profile-driven sandboxing for pi's bash and file tools.
 *
 * Configured profiles are resolved through their inheritance chain, then an
 * initially-empty, in-memory session overlay is applied as the final child.
 * Every enforcement surface reads that effective profile at call time. Base
 * profile switches remain branch-persistent; session overrides never persist.
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
import {
	isEmptyProfilePatch,
	loadConfig,
	parseSessionPatch,
	resolveEffectiveProfile,
	toSrtConfig,
	type HallpassConfig,
	type Profile,
	type ProfilePatch,
} from "./profiles.ts";
import { registerReflectionTools } from "./reflection.ts";
import {
	ensureNetworkFloor,
	LOG_MONITOR_ENABLED,
	setDeniedDomains,
	setNetworkFloor,
	wrapCommand,
} from "./sandbox.ts";
import { formatHallpassStatus, formatSessionEditPreview, type HallpassStatus } from "./status.ts";
import { createTmuxProfileSync } from "./tmux.ts";
import { createToolSelection, installToolBlocker } from "./tools.ts";

const ENTRY_TYPE = "hallpass:profile";

function emptySessionPatch(): ProfilePatch {
	return { filesystem: {}, network: {} };
}

export default function hallpass(pi: ExtensionAPI) {
	// Session cwd is refreshed at session_start: worktree-isolated children differ from process.cwd().
	let cwd = process.cwd();
	let config: HallpassConfig = {
		defaultProfile: "",
		profiles: new Map(),
		cycleProfiles: [],
		agentProfiles: {},
		deniedDomains: [],
		warnings: ["hallpass config not loaded yet"],
	};
	let currentName: string | undefined;
	let isChild = false;
	let sessionPatch = emptySessionPatch();
	let sessionToml = "";
	let sessionRevision = 0;
	const toolSelection = createToolSelection(pi);
	const tmuxSync = createTmuxProfileSync(pi);

	function currentBaseProfile(): Profile | undefined {
		if (currentName) {
			const profile = config.profiles.get(currentName);
			if (profile) return profile;
		}
		if (config.defaultProfile) return config.profiles.get(config.defaultProfile);
		return undefined;
	}

	/** The single policy source used by bash, file guards, tools, tmux, UI, and reflection. */
	function currentProfile(): Profile | undefined {
		const base = currentBaseProfile();
		return base ? resolveEffectiveProfile(base, sessionPatch) : undefined;
	}

	function getStatus(): HallpassStatus {
		const base = currentBaseProfile();
		return {
			revision: sessionRevision,
			baseProfile: base?.name,
			activeProfile: base?.name,
			inheritanceChain: base?.inheritanceChain ?? [],
			sessionOverrides: sessionToml,
			sessionOverridesIgnored: Boolean(base?.invalid && sessionToml),
			deniedDomains: [...config.deniedDomains],
			warnings: [...config.warnings],
			isChild,
			effectiveProfile: base ? resolveEffectiveProfile(base, sessionPatch) : undefined,
		};
	}

	function updateStatus(ctx: ExtensionContext): void {
		if (!ctx.hasUI) return;
		const profile = currentProfile();
		let text: string;
		if (!profile) {
			text = ctx.ui.theme.fg("muted", "hallpass: inactive");
		} else {
			const suffix = profile.invalid || isEmptyProfilePatch(sessionPatch) ? "" : "+session";
			text = profile.sandbox
				? ctx.ui.theme.fg("accent", `hallpass: ${profile.name}${suffix}`)
				: ctx.ui.theme.fg("warning", `hallpass: ${profile.name}${suffix} (off)`);
		}
		ctx.ui.setStatus("hallpass", text);
	}

	/** Apply all side effects after a base switch, config reload, or session edit. */
	function applyEffectiveProfile(ctx: ExtensionContext): void {
		const profile = currentProfile();
		if (!isChild) {
			if (profile) setNetworkFloor(profile.network.mode);
			// Children's tool scope belongs to pi-subagents; tool_call remains the backstop.
			toolSelection.apply(profile);
		}
		tmuxSync.sync(profile);
		updateStatus(ctx);
	}

	function switchProfile(name: string, ctx: ExtensionContext): string {
		const profile = config.profiles.get(name);
		if (!profile) {
			return `no profile "${name}" (have: ${[...config.profiles.keys()].join(", ") || "none"})`;
		}
		currentName = name;
		pi.appendEntry(ENTRY_TYPE, { profile: name });
		sessionRevision++;
		applyEffectiveProfile(ctx);
		const overlay = isEmptyProfilePatch(sessionPatch) ? "" : " (session overrides retained)";
		return `switched to profile "${name}"${overlay} — effective on the next tool call`;
	}

	function cycleProfile(step: 1 | -1, ctx: ExtensionContext): void {
		const names = config.cycleProfiles;
		if (names.length === 0) {
			ctx.ui.notify("hallpass: cycleProfiles is empty or has no valid profiles", "warning");
			return;
		}
		let current = names[0];
		if (currentName && config.profiles.has(currentName)) current = currentName;
		else if (config.profiles.has(config.defaultProfile)) current = config.defaultProfile;
		const index = names.indexOf(current ?? "");
		const nextIndex = index < 0
			? (step === 1 ? 0 : names.length - 1)
			: (index + step + names.length) % names.length;
		const next = names[nextIndex];
		if (next === undefined) return;
		ctx.ui.notify(`hallpass: ${switchProfile(next, ctx)}`, "info");
	}

	pi.registerShortcut("alt+n", {
		description: "Cycle hallpass profile forward",
		handler: (ctx) => cycleProfile(1, ctx),
	});
	pi.registerShortcut("alt+p", {
		description: "Cycle hallpass profile backward",
		handler: (ctx) => cycleProfile(-1, ctx),
	});

	// -----------------------------------------------------------------------
	// Session lifecycle: load config, restore base profile, reset ephemeral state
	// -----------------------------------------------------------------------

	pi.on("session_start", async (_event, ctx) => {
		if (ctx.cwd !== cwd) {
			cwd = ctx.cwd;
			localBash = createBashTool(cwd);
		}
		config = loadConfig(cwd);
		sessionPatch = emptySessionPatch();
		sessionToml = "";
		sessionRevision++;
		isChild = false;

		let restored: string | undefined;
		for (const entry of ctx.sessionManager.getBranch()) {
			if (entry.type === "custom" && entry.customType === ENTRY_TYPE) {
				const data = entry.data as { profile?: unknown } | undefined;
				if (typeof data?.profile === "string") restored = data.profile;
			}
		}

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
		if (assigned && config.profiles.has(assigned)) currentName = assigned;
		else if (restored && config.profiles.has(restored)) currentName = restored;
		else if (config.profiles.has(config.defaultProfile)) currentName = config.defaultProfile;
		else currentName = [...config.profiles.keys()][0];

		setDeniedDomains(config.deniedDomains);
		if (!isChild) {
			ensureNetworkFloor(currentProfile()?.network.mode);
			toolSelection.apply(currentProfile());
		}
		tmuxSync.syncAtStart(currentProfile());
		updateStatus(ctx);
		for (const warning of config.warnings) ctx.ui.notify(`hallpass: ${warning}`, "warning");
	});

	// Keep tmux selection and model-facing profile guidance current at each run.
	pi.on("before_agent_start", (event) => {
		tmuxSync.syncIfPending(currentProfile());
		const profile = currentProfile();
		if (!profile) {
			delete event.systemPromptOptions.sections.hallpass;
			return undefined;
		}
		const lines = [
			`Active hallpass profile: ${profile.name}${profile.description ? ` — ${profile.description}` : ""}`,
			"Hallpass filesystem, network, and tool restrictions are enforced policy. Do not retry denials or attempt to bypass them.",
			"Use hallpass_status to inspect the effective policy and session revision. If access is needed, explain why and use hallpass_session_edit to request user-approved session-only changes, or ask the user to switch profiles.",
		];
		if (profile.agentUsage) lines.push(`Profile agentUsage instructions:\n${profile.agentUsage}`);
		event.systemPromptOptions.sections.hallpass = lines.join("\n");
		return undefined;
	});

	// -----------------------------------------------------------------------
	// Parent side: refuse to spawn subagents that would escape hallpass
	// -----------------------------------------------------------------------

	pi.on("tool_call", (event) => {
		if (event.toolName !== "Agent") return undefined;
		const profile = currentProfile();
		if (!profile?.sandbox) return undefined;
		if (profile.invalid) {
			return {
				block: true,
				reason: `hallpass profile "${profile.name}" is invalid and fail-closed. Fix hallpass.toml and run /hp-reload before spawning agents.`,
			};
		}
		const input = event.input as { subagent_type?: unknown; isolated?: unknown };
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
	// Ephemeral session overlays and reflection tools
	// -----------------------------------------------------------------------

	function normalizeSessionToml(patch: ProfilePatch, toml: string): string {
		return isEmptyProfilePatch(patch) ? "" : toml.trim();
	}

	function assertRevision(expectedRevision: number): void {
		if (expectedRevision !== sessionRevision) {
			throw new Error(`stale session revision ${expectedRevision}; current revision is ${sessionRevision}. Call hallpass_status again.`);
		}
	}

	function previewSessionEdit(toml: string, expectedRevision: number): string {
		assertRevision(expectedRevision);
		const base = currentBaseProfile();
		if (!base) throw new Error("no active configured profile to extend");
		if (base.invalid) throw new Error(`profile "${base.name}" is invalid and fail-closed; fix hallpass.toml and /hp-reload first`);
		const patch = parseSessionPatch(toml);
		return formatSessionEditPreview(
			resolveEffectiveProfile(base, sessionPatch),
			resolveEffectiveProfile(base, patch),
			normalizeSessionToml(patch, toml),
		);
	}

	function setSessionConfig(toml: string, ctx: ExtensionContext): string {
		const base = currentBaseProfile();
		if (!base) throw new Error("no active configured profile to extend");
		if (base.invalid) throw new Error(`profile "${base.name}" is invalid and fail-closed; fix hallpass.toml and /hp-reload first`);
		const patch = parseSessionPatch(toml);
		sessionPatch = patch;
		sessionToml = normalizeSessionToml(patch, toml);
		sessionRevision++;
		const action = sessionToml
			? `updated session overrides for profile "${base.name}"`
			: `cleared session overrides for profile "${base.name}"`;
		try {
			applyEffectiveProfile(ctx);
			return `${action} — effective on the next tool call`;
		} catch (err) {
			// The closure-backed policy is already committed even if a secondary integration fails.
			return `${action}, but tool/tmux/UI synchronization failed: ${err instanceof Error ? err.message : String(err)}. ` +
				`The new filesystem/network/tool policy is still in memory; fix the integration or restart hallpass.`;
		}
	}

	function applySessionEdit(toml: string, expectedRevision: number, ctx: ExtensionContext): string {
		assertRevision(expectedRevision);
		return setSessionConfig(toml, ctx);
	}

	registerReflectionTools(pi, { getStatus, previewSessionEdit, applySessionEdit });

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
		ctx.ui.notify(`hallpass: ${switchProfile(item.profile.name, ctx)}`, "info");
	}

	function showProfileDetails(ctx: ExtensionContext): void {
		ctx.ui.notify(formatHallpassStatus(getStatus()), "info");
	}

	pi.registerCommand("hp", {
		description: "Hallpass profile dashboard",
		handler: async (_args, ctx) => {
			if (!requireInteractiveUI(ctx)) return;
			const selected = await ctx.ui.select("Hallpass", [
				`Status${currentName ? ` — ${currentName}` : " — inactive"}`,
				"Switch profile…",
				"Session overrides…",
				"Reload configuration",
			]);
			if (!selected) return;
			if (selected.startsWith("Status")) showProfileDetails(ctx);
			else if (selected === "Switch profile…") await chooseProfile(ctx);
			else if (selected === "Session overrides…") await editSessionOverrides(ctx);
			else if (selected === "Reload configuration") reloadConfiguration(ctx);
		},
	});

	pi.registerCommand("hp-status", {
		description: "Show the effective hallpass profile and session overrides",
		handler: async (_args, ctx) => showProfileDetails(ctx),
	});

	pi.registerCommand("hp-profile", {
		description: "Select the active hallpass profile",
		handler: async (args, ctx) => {
			if (!requireInteractiveUI(ctx)) return;
			const requested = args.trim();
			if (requested) {
				if (!config.profiles.has(requested)) {
					ctx.ui.notify(`Unknown profile "${requested}". Run /hp-profile to pick one.`, "error");
					return;
				}
				ctx.ui.notify(`hallpass: ${switchProfile(requested, ctx)}`, "info");
				return;
			}
			await chooseProfile(ctx);
		},
	});

	async function editSessionOverrides(ctx: ExtensionContext): Promise<void> {
		if (!requireInteractiveUI(ctx)) return;
		const prefill = sessionToml || [
			"# Ephemeral overrides layered over the active profile; never written to hallpass.toml.",
			"# This is the COMPLETE session overlay. Uncomment/edit examples and combine fields as needed.",
			"# Raw lists replace inherited lists; all-signed lists add/remove exact entries; [] clears.",
			"#",
			"# Example: allow implementation writes in the project and restore editing tools",
			"# filesystem.allowWrite = [\"+$CWD\"]",
			"# tools = [\"+write\", \"+edit\"]",
			"#",
			"# Example: permit only GitHub network access (setting the mode is required)",
			"# network.mode = \"allowlist\"",
			"# network.allowedDomains = [\"+github.com\", \"+*.githubusercontent.com\"]",
			"#",
			"# Example: remove an inherited write root (or use raw [] to clear all inherited roots)",
			"# filesystem.allowWrite = [\"-/tmp\"]",
			"#",
			"# Example: replace agent-facing instructions for this session",
			"# agentUsage = \"Implement the approved plan; keep changes inside $CWD.\"",
			"#",
			"# Save comments-only/empty input to clear. See /skill:hallpass for the full schema.",
			"",
		].join("\n");
		const edited = await ctx.ui.editor("Hallpass session overrides (TOML)", prefill);
		if (edited === undefined) return;
		try {
			ctx.ui.notify(`hallpass: ${setSessionConfig(edited, ctx)}`, "info");
		} catch (err) {
			ctx.ui.notify(`hallpass session config not applied: ${err instanceof Error ? err.message : String(err)}`, "error");
		}
	}

	pi.registerCommand("hp-session", {
		description: "Edit ephemeral session overrides (TOML), or /hp-session clear",
		handler: async (args, ctx) => {
			const requested = args.trim();
			if (!requested) {
				await editSessionOverrides(ctx);
				return;
			}
			try {
				ctx.ui.notify(`hallpass: ${setSessionConfig(requested.toLowerCase() === "clear" ? "" : args, ctx)}`, "info");
			} catch (err) {
				ctx.ui.notify(`hallpass session config not applied: ${err instanceof Error ? err.message : String(err)}`, "error");
			}
		},
	});

	pi.registerCommand("hp-reload", {
		description: "Reload hallpass.toml configuration (session overrides are retained)",
		handler: async (_args, ctx) => reloadConfiguration(ctx),
	});

	function reloadConfiguration(ctx: ExtensionContext): void {
		const previousName = currentName;
		config = loadConfig(cwd);
		if (!currentName || !config.profiles.has(currentName)) {
			currentName = config.profiles.has(config.defaultProfile)
				? config.defaultProfile
				: [...config.profiles.keys()][0];
		}
		sessionRevision++;
		setDeniedDomains(config.deniedDomains);
		applyEffectiveProfile(ctx);
		if (currentName && currentName !== previousName) pi.appendEntry(ENTRY_TYPE, { profile: currentName });
		ctx.ui.notify(
			`Reloaded ${config.profiles.size} profile(s)${currentName ? `; active: ${currentName}` : "; no active profile"}; session overrides retained`,
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
			if (!profile || !profile.sandbox) return localBash.execute(id, params, signal, onUpdate);
			let wrapped: Awaited<ReturnType<typeof wrapCommand>>;
			try {
				wrapped = await wrapCommand(
					params.command,
					toSrtConfig(profile, cwd),
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
					const violations = await wrapped.violations(LOG_MONITOR_ENABLED && looksLikeFsDenial(output) ? 300 : 0);
					const extra = explainFailure(profile, output, violations);
					if (extra && first?.type === "text") result.content[0] = { ...first, text: `${first.text}\n\n${extra}` };
				}
				return result;
			} finally {
				wrapped.cleanup();
			}
		},
	});

	// File-tool path guard and tool-call backstop both read the effective profile.
	installFileToolGuard(pi, currentProfile, () => cwd);
	installToolBlocker(pi, currentProfile);

	// User `!` / `!!` shell (opt-in per effective profile).
	pi.on("user_bash", async () => {
		const profile = currentProfile();
		if (!profile?.sandbox || !profile.sandboxUserBash) return undefined;
		const local = createLocalBashOperations();
		return {
			operations: {
				exec: async (command, commandCwd, options) => {
					const wrapped = await wrapCommand(
						command,
						toSrtConfig(profile, commandCwd),
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
