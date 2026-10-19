/**
 * hallpass profile schema: TOML config loading, validation, and compilation
 * to sandbox-runtime (srt) configs.
 *
 * Config files (later files override profiles of the same name):
 *   ~/.pi/agent/hallpass.toml   (user)
 *   .pi/hallpass.toml           (project, cwd-relative)
 */

import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { isAbsolute, join, resolve } from "node:path";
import { parse as parseToml } from "smol-toml";
import type { SandboxRuntimeConfig } from "@anthropic-ai/sandbox-runtime";

export interface FilesystemProfile {
	denyRead: string[];
	allowRead: string[];
	allowWrite: string[];
	denyWrite: string[];
}

export type NetworkMode = "open" | "offline" | "allowlist";

export interface NetworkProfile {
	mode: NetworkMode;
	allowedDomains: string[];
	deniedDomains: string[];
}

export interface Profile {
	name: string;
	description?: string;
	/** false = bash commands run unsandboxed (escape hatch). File-tool guard still applies. */
	sandbox: boolean;
	filesystem: FilesystemProfile;
	network: NetworkProfile;
	/** Reserved (chunk 4): pi tool-selection delta, e.g. ["-write", "+grep"]. */
	tools?: string[];
	/** Reserved (chunk 4): tmux-mcp fine-grain policy profile name. */
	tmuxProfile?: string;
	/** Also wrap user-typed `!`/`!!` commands in this profile's sandbox. */
	sandboxUserBash: boolean;
}

export interface HallpassConfig {
	defaultProfile: string;
	profiles: Map<string, Profile>;
	/** Reserved (chunk 3): subagent type -> profile name. */
	agentProfiles: Record<string, string>;
	warnings: string[];
}

const CONFIG_FILE = "hallpass.toml";

function configPaths(cwd: string): string[] {
	return [join(homedir(), ".pi", "agent", CONFIG_FILE), join(cwd, ".pi", CONFIG_FILE)];
}

function asStringArray(value: unknown, label: string, warnings: string[]): string[] {
	if (value === undefined) return [];
	if (!Array.isArray(value) || value.some((v) => typeof v !== "string")) {
		warnings.push(`${label}: expected an array of strings, ignoring`);
		return [];
	}
	return value as string[];
}

function parseProfile(name: string, raw: unknown, warnings: string[]): Profile | undefined {
	if (typeof raw !== "object" || raw === null || Array.isArray(raw)) {
		warnings.push(`profile "${name}": expected a table, ignoring`);
		return undefined;
	}
	const table = raw as Record<string, unknown>;
	const fs = (table.filesystem ?? {}) as Record<string, unknown>;
	const net = (table.network ?? {}) as Record<string, unknown>;

	const modeRaw = net.mode ?? "open";
	if (modeRaw !== "open" && modeRaw !== "offline" && modeRaw !== "allowlist") {
		warnings.push(`profile "${name}": network.mode must be open|offline|allowlist, defaulting to "open"`);
	}
	const mode: NetworkMode =
		modeRaw === "offline" || modeRaw === "allowlist" ? modeRaw : "open";

	if (mode === "allowlist" && !Array.isArray(net.allowedDomains)) {
		warnings.push(`profile "${name}": network.mode is "allowlist" but network.allowedDomains is missing/invalid`);
	}

	return {
		name,
		description: typeof table.description === "string" ? table.description : undefined,
		sandbox: table.sandbox !== false,
		filesystem: {
			denyRead: asStringArray(fs.denyRead, `profile "${name}" filesystem.denyRead`, warnings),
			allowRead: asStringArray(fs.allowRead, `profile "${name}" filesystem.allowRead`, warnings),
			allowWrite: asStringArray(fs.allowWrite, `profile "${name}" filesystem.allowWrite`, warnings),
			denyWrite: asStringArray(fs.denyWrite, `profile "${name}" filesystem.denyWrite`, warnings),
		},
		network: {
			mode,
			allowedDomains: asStringArray(net.allowedDomains, `profile "${name}" network.allowedDomains`, warnings),
			deniedDomains: asStringArray(net.deniedDomains, `profile "${name}" network.deniedDomains`, warnings),
		},
		tools: Array.isArray(table.tools) ? asStringArray(table.tools, `profile "${name}" tools`, warnings) : undefined,
		tmuxProfile: typeof table.tmuxProfile === "string" ? table.tmuxProfile : undefined,
		sandboxUserBash: table.sandboxUserBash === true,
	};
}

/** Load and merge user + project hallpass.toml. Never throws; problems become warnings. */
export function loadConfig(cwd: string): HallpassConfig {
	const warnings: string[] = [];
	const profiles = new Map<string, Profile>();
	let defaultProfile = "";
	let agentProfiles: Record<string, string> = {};

	for (const path of configPaths(cwd)) {
		if (!existsSync(path)) continue;
		let doc: Record<string, unknown>;
		try {
			doc = parseToml(readFileSync(path, "utf8")) as Record<string, unknown>;
		} catch (err) {
			warnings.push(`${path}: TOML parse error: ${err instanceof Error ? err.message : String(err)}`);
			continue;
		}
		if (typeof doc.default === "string") defaultProfile = doc.default;
		const rawProfiles = doc.profiles;
		if (typeof rawProfiles === "object" && rawProfiles !== null) {
			for (const [name, raw] of Object.entries(rawProfiles as Record<string, unknown>)) {
				const profile = parseProfile(name, raw, warnings);
				if (profile) profiles.set(name, profile);
			}
		}
		const rawAgentProfiles = doc.agentProfiles;
		if (typeof rawAgentProfiles === "object" && rawAgentProfiles !== null) {
			for (const [agent, profileName] of Object.entries(rawAgentProfiles as Record<string, unknown>)) {
				if (typeof profileName === "string") agentProfiles[agent] = profileName;
			}
		}
	}

	if (profiles.size === 0) {
		warnings.push("no profiles defined; hallpass is inactive (bash runs unsandboxed)");
	}
	if (defaultProfile && !profiles.has(defaultProfile)) {
		warnings.push(`default profile "${defaultProfile}" is not defined`);
	}
	return { defaultProfile, profiles, agentProfiles, warnings };
}

// ---------------------------------------------------------------------------
// Path expansion
// ---------------------------------------------------------------------------

const GLOB_CHARS = /[*?[\]{}]/;

export function hasGlobChars(p: string): boolean {
	return GLOB_CHARS.test(p);
}

/** Expand $CWD, ~, and $ENV_VARS; resolve relative paths against cwd. Globs are kept as-is. */
export function expandPath(entry: string, cwd: string): string {
	let p = entry;
	if (p === "~" || p.startsWith("~/")) p = homedir() + p.slice(1);
	p = p.replaceAll("$CWD", cwd);
	p = p.replace(/\$\{(\w+)\}|\$(\w+)/g, (_m, braced, bare) => process.env[braced ?? bare] ?? "");
	if (!hasGlobChars(p) && !isAbsolute(p)) p = resolve(cwd, p);
	return p;
}

function expandAll(entries: string[], cwd: string): string[] {
	return entries.map((e) => expandPath(e, cwd));
}

// ---------------------------------------------------------------------------
// Compilation to srt config
// ---------------------------------------------------------------------------

/**
 * Compile a profile's filesystem policy to a per-call srt config (the
 * `customConfig` argument of wrapWithSandbox). srt merges per-field with the
 * base config given at initialize(); hallpass's base fs config is empty, so
 * every fs restriction must be present here. Fs policy is baked at wrap time,
 * so it is always exactly per-call.
 *
 * Network is deliberately NOT included: in srt 0.0.80, customConfig.network
 * never reaches the proxy filter — per-call network policy goes through
 * registerCommandNetworkLists + the ask callback instead (see sandbox.ts).
 * The profile's `network.deniedDomains` is parsed but not yet enforced
 * (per-call denies need either a global updateConfig or callback support;
 * chunk 2 decision).
 */
export function toSrtFilesystemConfig(profile: Profile, cwd: string): Partial<SandboxRuntimeConfig> {
	return {
		filesystem: {
			denyRead: expandAll(profile.filesystem.denyRead, cwd),
			allowRead: expandAll(profile.filesystem.allowRead, cwd),
			allowWrite: expandAll(profile.filesystem.allowWrite, cwd),
			denyWrite: expandAll(profile.filesystem.denyWrite, cwd),
		},
	};
}

/** Short human-readable summary for /hallpass status. */
export function summarizeProfile(profile: Profile): string {
	const lines = [
		`profile: ${profile.name}${profile.description ? ` — ${profile.description}` : ""}`,
		`sandbox: ${profile.sandbox ? "on" : "OFF (bash unsandboxed)"}`,
		`network: ${profile.network.mode}${profile.network.mode === "allowlist" ? ` (${profile.network.allowedDomains.join(", ") || "none"})` : ""}${profile.network.deniedDomains.length ? `; denied: ${profile.network.deniedDomains.join(", ")} (not yet enforced)` : ""}`,
		`write: ${profile.filesystem.allowWrite.length ? profile.filesystem.allowWrite.join(", ") : "(nothing writable outside srt defaults)"}`,
	];
	if (profile.filesystem.denyWrite.length) lines.push(`deny-write: ${profile.filesystem.denyWrite.join(", ")}`);
	if (profile.filesystem.denyRead.length) lines.push(`deny-read: ${profile.filesystem.denyRead.join(", ")}`);
	if (profile.tmuxProfile) lines.push(`tmux-mcp profile: ${profile.tmuxProfile} (reserved, not yet enforced)`);
	if (profile.tools) lines.push(`tools: ${profile.tools.join(" ")} (reserved, not yet applied)`);
	return lines.join("\n");
}
