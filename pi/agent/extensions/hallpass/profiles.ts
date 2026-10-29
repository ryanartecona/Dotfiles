/**
 * hallpass profile schema: TOML config loading, inheritance, session overlays,
 * validation, and compilation to sandbox-runtime (srt) configs.
 *
 * Config files (later files replace same-named raw profile definitions):
 *   ~/.pi/agent/hallpass.toml   (user)
 *   .pi/hallpass.toml           (project, cwd-relative)
 */

import { existsSync, readFileSync, realpathSync } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, isAbsolute, join, resolve } from "node:path";
import { parse as parseToml } from "smol-toml";
import type { SandboxRuntimeConfig } from "@anthropic-ai/sandbox-runtime";
import { mergeToolDeltas, parseToolDeltas } from "./tools.ts";

export interface FilesystemProfile {
	denyRead: string[];
	allowRead: string[];
	allowWrite: string[];
	denyWrite: string[];
}

export interface FilesystemProfilePatch {
	denyRead?: string[];
	allowRead?: string[];
	allowWrite?: string[];
	denyWrite?: string[];
}

export type NetworkMode = "open" | "offline" | "allowlist";

export interface NetworkProfile {
	mode: NetworkMode;
	allowedDomains: string[];
	/** macOS path allowlist passed to srt at wrap time; ignored by srt on Linux. */
	allowUnixSockets: string[];
}

export interface NetworkProfilePatch {
	mode?: NetworkMode;
	allowedDomains?: string[];
	allowUnixSockets?: string[];
}

/** A not-yet-resolved profile. Undefined fields inherit; empty lists replace/clear. */
export interface ProfilePatch {
	extends?: string;
	description?: string;
	agentUsage?: string;
	sandbox?: boolean;
	filesystem: FilesystemProfilePatch;
	network: NetworkProfilePatch;
	tools?: string[];
	tmuxProfiles?: string[];
	sandboxUserBash?: boolean;
}

export interface Profile {
	name: string;
	/** True for the built-in lockdown used when this profile's config is invalid. */
	invalid?: boolean;
	/** Root-to-leaf configured inheritance chain. Session overlays do not add an entry. */
	inheritanceChain: string[];
	description?: string;
	/** Agent-facing instructions reflected into the system prompt and status tool. */
	agentUsage?: string;
	/** false = bash commands run unsandboxed (escape hatch). File-tool guard still applies. */
	sandbox: boolean;
	filesystem: FilesystemProfile;
	network: NetworkProfile;
	/** Resolved pi tool-selection operations, e.g. ["-write", "+grep"]. */
	tools?: string[];
	/** tmux-mcp fine-grain policy profile names. */
	tmuxProfiles?: string[];
	/** Also wrap user-typed `!`/`!!` commands in this profile's sandbox. */
	sandboxUserBash: boolean;
}

export interface HallpassConfig {
	defaultProfile: string;
	profiles: Map<string, Profile>;
	/** Ordered hotkey cycle. Empty disables cycling; omitted config defaults to all profiles. */
	cycleProfiles: string[];
	/** Subagent type -> profile name. */
	agentProfiles: Record<string, string>;
	/** Process-global always-deny domains, including in open mode. */
	deniedDomains: string[];
	warnings: string[];
}

const CONFIG_FILE = "hallpass.toml";

function configPaths(cwd: string): string[] {
	return [join(homedir(), ".pi", "agent", CONFIG_FILE), join(cwd, ".pi", CONFIG_FILE)];
}

function isTable(value: unknown): value is Record<string, unknown> {
	return typeof value === "object" && value !== null && !Array.isArray(value);
}

function asStringArray(value: unknown, label: string, warnings: string[]): string[] {
	if (value === undefined) return [];
	if (!Array.isArray(value) || value.some((v) => typeof v !== "string")) {
		warnings.push(`${label}: expected an array of strings, ignoring`);
		return [];
	}
	return value as string[];
}

/**
 * Parse a composable list. Empty/raw lists replace the inherited value; a list
 * whose entries are all +/- signed is a delta. Mixed forms are rejected.
 */
function parseComposableList(value: unknown, label: string, warnings: string[]): string[] | undefined {
	if (value === undefined) return undefined;
	if (!Array.isArray(value) || value.some((v) => typeof v !== "string")) {
		warnings.push(`${label}: expected an array of strings`);
		return undefined;
	}
	const entries = value as string[];
	if (entries.some((entry) => entry.length === 0 || entry === "+" || entry === "-")) {
		warnings.push(`${label}: entries and +/- operands must not be empty`);
		return undefined;
	}
	const signed = entries.filter((entry) => /^[+-]/.test(entry)).length;
	if (signed > 0 && signed !== entries.length) {
		warnings.push(`${label}: use either a raw replacement list or an all-signed +/- delta list, not both`);
		return undefined;
	}
	return entries;
}

/** `tmuxProfile` may be one name or a composable list of names. */
function parseTmuxProfiles(value: unknown, label: string, warnings: string[]): string[] | undefined {
	if (value === undefined) return undefined;
	if (typeof value === "string") return value ? [value] : [];
	return parseComposableList(value, label, warnings);
}

const PROFILE_KEYS = new Set([
	"extends",
	"description",
	"agentUsage",
	"sandbox",
	"filesystem",
	"network",
	"tools",
	"tmuxProfile",
	"sandboxUserBash",
]);
const FILESYSTEM_KEYS = new Set(["denyRead", "allowRead", "allowWrite", "denyWrite"]);
const NETWORK_KEYS = new Set(["mode", "allowedDomains", "allowUnixSockets", "deniedDomains"]);

function rejectUnknownKeys(
	table: Record<string, unknown>,
	allowed: Set<string>,
	label: string,
	warnings: string[],
): void {
	for (const key of Object.keys(table)) {
		if (!allowed.has(key)) warnings.push(`${label}: unknown field "${key}"`);
	}
}

/** Parse one raw profile or a session overlay. Any validation warning invalidates it. */
function parseProfilePatch(
	name: string,
	raw: unknown,
	warnings: string[],
	session: boolean,
): ProfilePatch | undefined {
	const label = session ? "hallpass session config" : `profile "${name}"`;
	if (!isTable(raw)) {
		warnings.push(`${label}: expected a table`);
		return undefined;
	}
	const warningStart = warnings.length;
	if (session) rejectUnknownKeys(raw, PROFILE_KEYS, label, warnings);
	if (session && raw.extends !== undefined) {
		warnings.push(`${label}: extends is implicit (the active profile) and cannot be set`);
	}

	const patch: ProfilePatch = { filesystem: {}, network: {} };
	if (typeof raw.extends === "string" && raw.extends) patch.extends = raw.extends;
	else if (raw.extends !== undefined && !session) warnings.push(`${label}: extends must be a non-empty string`);
	if (typeof raw.description === "string") patch.description = raw.description;
	else if (raw.description !== undefined) warnings.push(`${label}: description must be a string`);
	if (typeof raw.agentUsage === "string") patch.agentUsage = raw.agentUsage;
	else if (raw.agentUsage !== undefined) warnings.push(`${label}: agentUsage must be a string`);
	if (typeof raw.sandbox === "boolean") patch.sandbox = raw.sandbox;
	else if (raw.sandbox !== undefined) warnings.push(`${label}: sandbox must be a boolean`);
	if (typeof raw.sandboxUserBash === "boolean") patch.sandboxUserBash = raw.sandboxUserBash;
	else if (raw.sandboxUserBash !== undefined) warnings.push(`${label}: sandboxUserBash must be a boolean`);

	const fs = raw.filesystem ?? {};
	if (!isTable(fs)) {
		warnings.push(`${label}: filesystem must be a table`);
	} else {
		if (session) rejectUnknownKeys(fs, FILESYSTEM_KEYS, `${label}.filesystem`, warnings);
		for (const key of FILESYSTEM_KEYS) {
			const parsed = parseComposableList(fs[key], `${label}.filesystem.${key}`, warnings);
			if (parsed !== undefined) patch.filesystem[key as keyof FilesystemProfilePatch] = parsed;
		}
	}

	const net = raw.network ?? {};
	if (!isTable(net)) {
		warnings.push(`${label}: network must be a table`);
	} else {
		if (session) rejectUnknownKeys(net, NETWORK_KEYS, `${label}.network`, warnings);
		if (net.deniedDomains !== undefined) {
			warnings.push(
				`${label}: network.deniedDomains is not supported per profile or session; ` +
					`move it to the top-level [network] deniedDomains in hallpass.toml`,
			);
		}
		if (net.mode !== undefined && net.mode !== "open" && net.mode !== "offline" && net.mode !== "allowlist") {
			warnings.push(`${label}: network.mode must be open|offline|allowlist`);
		} else if (net.mode !== undefined) {
			patch.network.mode = net.mode;
		}
		const allowedDomains = parseComposableList(net.allowedDomains, `${label}.network.allowedDomains`, warnings);
		if (allowedDomains !== undefined) patch.network.allowedDomains = allowedDomains;
		const allowUnixSockets = parseComposableList(net.allowUnixSockets, `${label}.network.allowUnixSockets`, warnings);
		if (allowUnixSockets !== undefined) patch.network.allowUnixSockets = allowUnixSockets;
	}

	patch.tools = parseToolDeltas(raw.tools, `${label} tools`, warnings);
	patch.tmuxProfiles = parseTmuxProfiles(raw.tmuxProfile, `${label} tmuxProfile`, warnings);
	return warnings.length === warningStart ? patch : undefined;
}

/** Parse and validate a complete desired in-memory session overlay. Throws on invalid input. */
export function parseSessionPatch(toml: string): ProfilePatch {
	let doc: unknown;
	try {
		doc = parseToml(toml);
	} catch (err) {
		throw new Error(`TOML parse error: ${err instanceof Error ? err.message : String(err)}`);
	}
	const warnings: string[] = [];
	const patch = parseProfilePatch("session", doc, warnings, true);
	if (!patch) throw new Error(warnings.join("\n") || "invalid session config");
	return patch;
}

export function isEmptyProfilePatch(patch: ProfilePatch): boolean {
	return (
		patch.extends === undefined &&
		patch.description === undefined &&
		patch.agentUsage === undefined &&
		patch.sandbox === undefined &&
		patch.sandboxUserBash === undefined &&
		patch.tools === undefined &&
		patch.tmuxProfiles === undefined &&
		Object.values(patch.filesystem).every((value) => value === undefined) &&
		Object.values(patch.network).every((value) => value === undefined)
	);
}

/** Apply a generic list replacement/delta without mutating either input. */
function mergeList(parent: string[] | undefined, child: string[] | undefined): string[] | undefined {
	if (child === undefined) return parent === undefined ? undefined : [...parent];
	if (child.length === 0 || !/^[+-]/.test(child[0] ?? "")) return [...child];
	const result = [...(parent ?? [])];
	for (const delta of child) {
		const value = delta.slice(1);
		if (delta.startsWith("+")) {
			if (!result.includes(value)) result.push(value);
		} else {
			for (let i = result.length - 1; i >= 0; i--) {
				if (result[i] === value) result.splice(i, 1);
			}
		}
	}
	return result;
}

function defaultProfile(name: string): Profile {
	return {
		name,
		inheritanceChain: [name],
		sandbox: true,
		filesystem: { denyRead: [], allowRead: [], allowWrite: [], denyWrite: [] },
		network: { mode: "open", allowedDomains: [], allowUnixSockets: [] },
		sandboxUserBash: false,
	};
}

/** Keep an invalid selected/default/assigned profile present, but make it unusable for work. */
function failClosedProfile(name: string): Profile {
	return {
		name,
		invalid: true,
		inheritanceChain: [name],
		description: "invalid configuration; fail-closed lockdown",
		agentUsage:
			"This hallpass profile is invalid and has been replaced by a fail-closed lockdown. " +
			"Do not attempt workarounds. Ask the user to fix hallpass.toml and run /hp-reload.",
		sandbox: true,
		filesystem: { denyRead: ["/"], allowRead: [], allowWrite: [], denyWrite: ["/"] },
		network: { mode: "offline", allowedDomains: [], allowUnixSockets: [] },
		tools: ["-bash", "-write", "-edit", "-Agent"],
		sandboxUserBash: true,
	};
}

function applyPatch(base: Profile, patch: ProfilePatch, name: string): Profile {
	return {
		name,
		invalid: base.invalid,
		inheritanceChain: [...base.inheritanceChain],
		description: patch.description !== undefined ? patch.description : base.description,
		agentUsage: patch.agentUsage !== undefined ? patch.agentUsage : base.agentUsage,
		sandbox: patch.sandbox !== undefined ? patch.sandbox : base.sandbox,
		filesystem: {
			denyRead: mergeList(base.filesystem.denyRead, patch.filesystem.denyRead) ?? [],
			allowRead: mergeList(base.filesystem.allowRead, patch.filesystem.allowRead) ?? [],
			allowWrite: mergeList(base.filesystem.allowWrite, patch.filesystem.allowWrite) ?? [],
			denyWrite: mergeList(base.filesystem.denyWrite, patch.filesystem.denyWrite) ?? [],
		},
		network: {
			mode: patch.network.mode ?? base.network.mode,
			allowedDomains: mergeList(base.network.allowedDomains, patch.network.allowedDomains) ?? [],
			allowUnixSockets: mergeList(base.network.allowUnixSockets, patch.network.allowUnixSockets) ?? [],
		},
		tools: mergeToolDeltas(base.tools, patch.tools),
		tmuxProfiles: mergeList(base.tmuxProfiles, patch.tmuxProfiles),
		sandboxUserBash: patch.sandboxUserBash !== undefined ? patch.sandboxUserBash : base.sandboxUserBash,
	};
}

/** Layer the ephemeral overlay over a valid profile. Invalid lockdowns cannot be relaxed by overlays. */
export function resolveEffectiveProfile(base: Profile, session: ProfilePatch): Profile {
	if (base.invalid) return base;
	return applyPatch(base, session, base.name);
}

/** Resolve a raw profile map, recording unknown parents, cycles, and invalid descendants. */
function resolveProfiles(
	raw: Map<string, ProfilePatch>,
	warnings: string[],
	invalidNames: Set<string>,
): Map<string, Profile> {
	const resolved = new Map<string, Profile>();
	const failed = new Set<string>();
	const visiting: string[] = [];

	function resolveOne(name: string): Profile | undefined {
		const cached = resolved.get(name);
		if (cached) return cached;
		if (failed.has(name)) return undefined;
		const patch = raw.get(name);
		if (!patch) return undefined;
		const cycleStart = visiting.indexOf(name);
		if (cycleStart >= 0) {
			warnings.push(`profile inheritance cycle: ${[...visiting.slice(cycleStart), name].join(" -> ")}`);
			failed.add(name);
			return undefined;
		}

		visiting.push(name);
		let base: Profile | undefined;
		if (patch.extends) {
			if (!raw.has(patch.extends)) {
				warnings.push(`profile "${name}": unknown parent profile "${patch.extends}"`);
				failed.add(name);
			} else {
				base = resolveOne(patch.extends);
				if (!base) {
					warnings.push(`profile "${name}": parent profile "${patch.extends}" is invalid`);
					failed.add(name);
				}
			}
		} else {
			base = defaultProfile(name);
		}
		visiting.pop();
		if (!base) return undefined;

		const profile = applyPatch(base, patch, name);
		profile.inheritanceChain = patch.extends ? [...base.inheritanceChain, name] : [name];
		resolved.set(name, profile);
		return profile;
	}

	for (const name of raw.keys()) resolveOne(name);
	for (const name of failed) invalidNames.add(name);
	// DFS resolves parents first, but profile/cycle order remains declaration order.
	return new Map([...raw.keys()].flatMap((name) => {
		const profile = resolved.get(name);
		return profile ? [[name, profile] as const] : [];
	}));
}

/** Load and resolve user + project hallpass.toml. Never throws; problems become warnings. */
export function loadConfig(cwd: string, paths = configPaths(cwd)): HallpassConfig {
	const warnings: string[] = [];
	const rawProfiles = new Map<string, ProfilePatch>();
	const invalidNames = new Set<string>();
	let defaultProfileName = "";
	let configuredCycle: string[] | undefined;
	const agentProfiles: Record<string, string> = {};
	const deniedDomains = new Set<string>();

	for (const path of paths) {
		if (!existsSync(path)) continue;
		let doc: Record<string, unknown>;
		try {
			doc = parseToml(readFileSync(path, "utf8")) as Record<string, unknown>;
		} catch (err) {
			warnings.push(`${path}: TOML parse error: ${err instanceof Error ? err.message : String(err)}`);
			continue;
		}
		if (typeof doc.default === "string") defaultProfileName = doc.default;
		if (doc.cycleProfiles !== undefined) {
			if (Array.isArray(doc.cycleProfiles) && doc.cycleProfiles.every((name) => typeof name === "string")) {
				configuredCycle = doc.cycleProfiles as string[];
			} else {
				warnings.push(`${path} cycleProfiles: expected an array of profile names; cycling disabled`);
				configuredCycle = [];
			}
		}
		const rawNetwork = doc.network;
		if (isTable(rawNetwork)) {
			// Union across user + project files: a project can add denies, never remove the user's.
			for (const entry of asStringArray(
				rawNetwork.deniedDomains,
				`${path} [network] deniedDomains`,
				warnings,
			)) {
				deniedDomains.add(entry);
			}
		}
		const rawProfileTable = doc.profiles;
		if (isTable(rawProfileTable)) {
			for (const [name, raw] of Object.entries(rawProfileTable)) {
				const profile = parseProfilePatch(name, raw, warnings, false);
				if (profile) {
					rawProfiles.set(name, profile);
					invalidNames.delete(name);
				} else {
					// A later invalid definition replaces an earlier valid one; never silently use the looser old policy.
					rawProfiles.delete(name);
					invalidNames.add(name);
				}
			}
		}
		const rawAgentProfiles = doc.agentProfiles;
		if (isTable(rawAgentProfiles)) {
			for (const [agent, profileName] of Object.entries(rawAgentProfiles)) {
				if (typeof profileName === "string") agentProfiles[agent] = profileName;
			}
		}
	}

	const profiles = resolveProfiles(rawProfiles, warnings, invalidNames);
	for (const name of invalidNames) {
		if (!profiles.has(name)) profiles.set(name, failClosedProfile(name));
	}
	if (profiles.size === 0) {
		warnings.push("no profiles defined; hallpass is inactive (bash runs unsandboxed)");
	}
	if (defaultProfileName && !profiles.has(defaultProfileName)) {
		warnings.push(`default profile "${defaultProfileName}" is not defined or is invalid`);
	}
	for (const [agent, profileName] of Object.entries(agentProfiles)) {
		if (!profiles.has(profileName)) warnings.push(`agentProfiles["${agent}"] references unknown profile "${profileName}"`);
	}

	let cycleProfiles: string[];
	if (configuredCycle === undefined) {
		cycleProfiles = [...profiles.values()].filter((profile) => !profile.invalid).map((profile) => profile.name);
	} else {
		const seen = new Set<string>();
		cycleProfiles = [];
		for (const name of configuredCycle) {
			if (seen.has(name)) continue;
			seen.add(name);
			const profile = profiles.get(name);
			if (profile && !profile.invalid) cycleProfiles.push(name);
			else warnings.push(`cycleProfiles references unknown or invalid profile "${name}"`);
		}
	}

	return {
		defaultProfile: defaultProfileName,
		profiles,
		cycleProfiles,
		agentProfiles,
		deniedDomains: [...deniedDomains],
		warnings,
	};
}

// ---------------------------------------------------------------------------
// Path expansion
// ---------------------------------------------------------------------------

const GLOB_CHARS = /[*?[\]{}]/;

export function hasGlobChars(p: string): boolean {
	return GLOB_CHARS.test(p);
}

/** Resolve symlinks in the longest existing ancestor, re-appending the non-existent tail. */
export function realpathBestEffort(p: string): string {
	let current = p;
	const tail: string[] = [];
	for (;;) {
		try {
			const real = realpathSync(current);
			return tail.length > 0 ? join(real, ...tail.reverse()) : real;
		} catch {
			const parent = dirname(current);
			if (parent === current) return p;
			tail.push(basename(current));
			current = parent;
		}
	}
}

/** Expand $CWD, ~, and $ENV_VARS; resolve relative paths and symlinks. Globs stay as-is. */
export function expandPath(entry: string, cwd: string): string {
	let p = entry;
	if (p === "~" || p.startsWith("~/")) p = homedir() + p.slice(1);
	p = p.replaceAll("$CWD", cwd);
	p = p.replace(/\$\{(\w+)\}|\$(\w+)/g, (_m, braced, bare) => process.env[braced ?? bare] ?? "");
	if (hasGlobChars(p)) return p;
	if (!isAbsolute(p)) p = resolve(cwd, p);
	return realpathBestEffort(p);
}

function expandAll(entries: string[], cwd: string): string[] {
	return entries.map((e) => expandPath(e, cwd));
}

/**
 * Compile per-call srt config. Domain policy remains in sandbox.ts because srt's
 * proxy ignores customConfig.network.allowedDomains. allowUnixSockets is also
 * handled there: srt 0.0.80 reads it from global config at wrap time.
 */
export function toSrtConfig(profile: Profile, cwd: string): Partial<SandboxRuntimeConfig> {
	return {
		filesystem: {
			denyRead: expandAll(profile.filesystem.denyRead, cwd),
			allowRead: expandAll(profile.filesystem.allowRead, cwd),
			allowWrite: expandAll(profile.filesystem.allowWrite, cwd),
			denyWrite: expandAll(profile.filesystem.denyWrite, cwd),
		},
		// SAFETY: Partial<SandboxRuntimeConfig> is shallow; srt accepts a partial
		// nested network object and hallpass deliberately omits proxy domain fields.
		network: {
			allowUnixSockets: expandAll(profile.network.allowUnixSockets, cwd),
		} as SandboxRuntimeConfig["network"],
	};
}

/** Short human-readable summary shared by status UIs, tools, and approval diffs. */
export function summarizeProfile(profile: Profile): string {
	const lines = [
		`profile: ${profile.name}${profile.description ? ` — ${profile.description}` : ""}${profile.invalid ? " [INVALID; FAIL-CLOSED]" : ""}`,
		`sandbox: ${profile.sandbox ? "on" : "OFF (bash unsandboxed)"}`,
		`network: ${profile.network.mode}${profile.network.mode === "allowlist" ? ` (${profile.network.allowedDomains.join(", ") || "none"})` : ""}`,
		`unix sockets: ${profile.network.allowUnixSockets.length ? profile.network.allowUnixSockets.join(", ") : "(none allowed)"}`,
		`write: ${profile.filesystem.allowWrite.length ? profile.filesystem.allowWrite.join(", ") : "(nothing writable outside srt defaults)"}`,
	];
	if (profile.filesystem.allowRead.length) lines.push(`allow-read: ${profile.filesystem.allowRead.join(", ")}`);
	if (profile.filesystem.denyWrite.length) lines.push(`deny-write: ${profile.filesystem.denyWrite.join(", ")}`);
	if (profile.filesystem.denyRead.length) lines.push(`deny-read: ${profile.filesystem.denyRead.join(", ")}`);
	if (profile.tmuxProfiles?.length) lines.push(`tmux-mcp profile: ${profile.tmuxProfiles.join(", ")}`);
	if (profile.tools?.length) lines.push(`tools: ${profile.tools.join(" ")}`);
	lines.push(`sandbox user bash: ${profile.sandboxUserBash ? "on" : "off"}`);
	if (profile.agentUsage) lines.push(`agentUsage:\n${profile.agentUsage}`);
	return lines.join("\n");
}
