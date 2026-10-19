/**
 * Subagent integration helpers (no fork of pi-subagents).
 *
 * pi-subagents parses agent `.md` frontmatter itself and silently drops keys
 * it does not know, so hallpass reads the same files independently to find
 * its own `hallpass_profile:` key and to decide whether hallpass will even load in the
 * child. Discovery mirrors pi-subagents' `loadCustomAgents` (global, then
 * workspace `.agents/agents`, then project `.pi/agents`; later overrides
 * earlier). Package-shipped agent dirs are NOT scanned (known gap); such
 * agents fall through to the `[agentProfiles]` map.
 *
 * Child identity: pi-subagents names each child session
 * `{agentName}#{agentId[:8]}` before binding extensions, so it is readable at
 * `session_start`. All identity logic lives in subagentTypeFromSessionName so
 * an upstream change (env var, exported marker) slots in here.
 */

import { existsSync, readdirSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { basename, join } from "node:path";

export interface AgentInfo {
	name: string;
	path: string;
	/** Frontmatter `hallpass_profile:` (hallpass-owned key; snake_case like pi-subagents' own keys). */
	profile?: string;
	/**
	 * Whether hallpass's handlers and tools will be bound in this agent's
	 * session. False when frontmatter disables extensions, is `isolated`,
	 * excludes hallpass, or allowlists extensions without hallpass.
	 */
	hallpassLoads: boolean;
}

type FrontmatterValue = string | string[];

function normalizeKey(key: string): string {
	return key
		.replace(/([A-Z])/g, "_$1")
		.replace(/-/g, "_")
		.toLowerCase();
}

function unquote(value: string): string {
	const v = value.trim();
	if (v.length >= 2 && (v.startsWith('"') || v.startsWith("'")) && v.endsWith(v[0] ?? "")) {
		return v.slice(1, -1);
	}
	return v;
}

/** Flat top-level `key: value` frontmatter, with `[a, b]` and `- item` list forms. */
export function parseFrontmatter(text: string): Record<string, FrontmatterValue> | undefined {
	const lines = text.split(/\r?\n/);
	if (lines[0]?.trim() !== "---") return undefined;
	const end = lines.findIndex((line, index) => index > 0 && line.trim() === "---");
	if (end < 0) return undefined;

	const result: Record<string, FrontmatterValue> = {};
	let listKey: string | undefined;
	for (const line of lines.slice(1, end)) {
		const item = /^\s+-\s+(.*)$/.exec(line);
		if (item && listKey) {
			const existing = result[listKey];
			if (Array.isArray(existing)) existing.push(unquote(item[1] ?? ""));
			continue;
		}
		const kv = /^([A-Za-z_][\w-]*):\s*(.*)$/.exec(line);
		if (!kv) {
			listKey = undefined;
			continue;
		}
		const key = normalizeKey(kv[1] ?? "");
		const raw = (kv[2] ?? "").trim();
		if (raw === "") {
			// Possibly a block list; becomes [] and fills from following "- item" lines.
			result[key] = [];
			listKey = key;
		} else if (raw.startsWith("[") && raw.endsWith("]")) {
			result[key] = raw
				.slice(1, -1)
				.split(",")
				.map((part) => unquote(part))
				.filter((part) => part !== "");
			listKey = undefined;
		} else {
			result[key] = unquote(raw.replace(/\s+#.*$/, ""));
			listKey = undefined;
		}
	}
	return result;
}

function scalar(value: FrontmatterValue | undefined): string | undefined {
	return typeof value === "string" ? value : undefined;
}

function names(value: FrontmatterValue | undefined): string[] {
	const items = Array.isArray(value) ? value : (value ?? "").split(",");
	return items.map((item) => item.trim().toLowerCase()).filter((item) => item !== "");
}

/** Decide from frontmatter whether hallpass is bound in the child session. */
export function hallpassLoadsFor(fm: Record<string, FrontmatterValue>): boolean {
	if (scalar(fm.isolated)?.toLowerCase() === "true") return false;
	if (names(fm.exclude_extensions).includes("hallpass")) return false;

	const spec = fm.extensions ?? fm.inherit_extensions;
	if (spec === undefined) return true;
	if (typeof spec === "string") {
		const lowered = spec.trim().toLowerCase();
		if (lowered === "false") return false;
		if (lowered === "" || lowered === "true") return true;
		return names(spec).includes("hallpass");
	}
	// An empty list came from a bare `extensions:` key with no items: treat as unspecified.
	return spec.length === 0 ? true : names(spec).includes("hallpass");
}

function agentDirs(cwd: string): string[] {
	return [join(homedir(), ".pi", "agent", "agents"), join(cwd, ".agents", "agents"), join(cwd, ".pi", "agents")];
}

/** Discover agent definitions; map keys are lowercased names (pi-subagents resolves types case-insensitively). */
export function discoverAgents(cwd: string): Map<string, AgentInfo> {
	const agents = new Map<string, AgentInfo>();
	for (const dir of agentDirs(cwd)) {
		if (!existsSync(dir)) continue;
		let files: string[];
		try {
			files = readdirSync(dir).filter((file) => file.endsWith(".md"));
		} catch {
			continue;
		}
		for (const file of files) {
			const path = join(dir, file);
			let fm: Record<string, FrontmatterValue> | undefined;
			try {
				fm = parseFrontmatter(readFileSync(path, "utf8"));
			} catch {
				continue;
			}
			if (!fm) continue;
			const name = scalar(fm.name) ?? basename(file, ".md");
			agents.set(name.toLowerCase(), {
				name,
				path,
				profile: scalar(fm.hallpass_profile),
				hallpassLoads: hallpassLoadsFor(fm),
			});
		}
	}
	return agents;
}

/** The agent type encoded in a pi-subagents child session name, or undefined for a non-child session. */
export function subagentTypeFromSessionName(sessionName: string | undefined): string | undefined {
	return /^(.+)#[0-9a-f]{8}$/.exec(sessionName ?? "")?.[1];
}

/**
 * Profile name for a subagent type: its own frontmatter `hallpass_profile:`, then the
 * `[agentProfiles]` entry for the type, then `[agentProfiles]["*"]`, else
 * undefined (caller falls back to the default profile).
 */
export function assignedProfileName(
	type: string,
	agents: Map<string, AgentInfo>,
	agentProfiles: Record<string, string>,
): string | undefined {
	const lowered = type.toLowerCase();
	const fromFile = agents.get(lowered)?.profile;
	if (fromFile) return fromFile;
	for (const [key, value] of Object.entries(agentProfiles)) {
		if (key.toLowerCase() === lowered) return value;
	}
	return agentProfiles["*"];
}
