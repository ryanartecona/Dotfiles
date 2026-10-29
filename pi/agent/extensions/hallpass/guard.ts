/**
 * hallpass file-tool guard.
 *
 * srt only sandboxes the bash tool; pi's file tools (read/write/edit/grep/
 * find/ls) perform their own fs access. This guard enforces the active
 * profile's filesystem policy on their structured `path` inputs via the
 * tool_call hook.
 *
 * Limitations (documented in DESIGN.md):
 * - Prefix matching in JS, not kernel enforcement. Targets and rules resolve
 *   symlinks in their longest existing ancestor, but filesystem races remain.
 * - Glob entries (containing * ? [ ] { }) are enforced for bash by srt but
 *   skipped here — matching them faithfully would duplicate srt's glob
 *   semantics. Keep profile paths literal where file-tool coverage matters.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { isAbsolute, resolve } from "node:path";
import { expandPath, hasGlobChars, realpathBestEffort, type Profile } from "./profiles.ts";

const READ_TOOLS = new Set(["read", "grep", "find", "ls"]);
const WRITE_TOOLS = new Set(["write", "edit"]);

function normalize(p: string): string {
	return p.length > 1 && p.endsWith("/") ? p.slice(0, -1) : p;
}

/** Subpath semantics: p equals root or lives under it. */
function isUnder(p: string, root: string): boolean {
	const r = normalize(root);
	return p === r || p.startsWith(r + "/");
}

/** Literal (non-glob) entries, expanded and absolutized. */
function literalEntries(entries: string[], cwd: string): string[] {
	const result: string[] = [];
	for (const entry of entries) {
		if (!hasGlobChars(entry)) result.push(normalize(expandPath(entry, cwd)));
	}
	return result;
}

function underAny(p: string, roots: string[]): boolean {
	return roots.some((root) => isUnder(p, root));
}

export function installFileToolGuard(
	pi: ExtensionAPI,
	getProfile: () => Profile | undefined,
	getCwd: () => string,
): void {
	pi.on("tool_call", (event) => {
		const profile = getProfile();
		if (!profile) return undefined;
		const cwd = getCwd();

		const isRead = READ_TOOLS.has(event.toolName);
		const isWrite = WRITE_TOOLS.has(event.toolName);
		if (!isRead && !isWrite) return undefined;

		const input = event.input as { path?: string };
		const rawPath = typeof input.path === "string" && input.path ? input.path : ".";
		const absoluteTarget = isAbsolute(rawPath) ? rawPath : resolve(cwd, rawPath);
		// Profile entries are canonicalized by expandPath. Canonicalize the tool
		// target too, or an allowed symlink (e.g. ~/.pi/agent/settings.json) can
		// never match its real path. Best-effort ancestors also cover new files.
		const target = normalize(realpathBestEffort(normalize(absoluteTarget)));

		if (isRead) {
			const denied = literalEntries(profile.filesystem.denyRead, cwd);
			const reallowed = literalEntries(profile.filesystem.allowRead, cwd);
			if (underAny(target, denied) && !underAny(target, reallowed)) {
				return {
					block: true,
					reason: `hallpass profile "${profile.name}" denies reading ${target} (denyRead)`,
				};
			}
		}

		if (isWrite) {
			const deniedWrite = literalEntries(profile.filesystem.denyWrite, cwd);
			if (underAny(target, deniedWrite)) {
				return {
					block: true,
					reason: `hallpass profile "${profile.name}" denies writing ${target} (denyWrite)`,
				};
			}
			const allowWrite = literalEntries(profile.filesystem.allowWrite, cwd);
			if (allowWrite.length > 0 && !underAny(target, allowWrite)) {
				return {
					block: true,
					reason:
						`hallpass profile "${profile.name}" allows writes only under: ` +
						`${profile.filesystem.allowWrite.join(", ")} (got ${target})`,
				};
			}
		}

		return undefined;
	});
}
