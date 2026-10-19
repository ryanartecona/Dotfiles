/**
 * hallpass file-tool guard.
 *
 * srt only sandboxes the bash tool; pi's file tools (read/write/edit/grep/
 * find/ls) perform their own fs access. This guard enforces the active
 * profile's filesystem policy on their structured `path` inputs via the
 * tool_call hook.
 *
 * Limitations (documented in DESIGN.md):
 * - Prefix matching in JS, not kernel enforcement: symlink escapes are not
 *   detected (srt's own enforcement for bash happens at the sandbox layer).
 * - Glob entries (containing * ? [ ] { }) are enforced for bash by srt but
 *   skipped here — matching them faithfully would duplicate srt's glob
 *   semantics. Keep profile paths literal where file-tool coverage matters.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { isAbsolute, resolve } from "node:path";
import { expandPath, hasGlobChars, type Profile } from "./profiles.ts";

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
	cwd: string,
): void {
	pi.on("tool_call", (event) => {
		const profile = getProfile();
		if (!profile) return undefined;

		const isRead = READ_TOOLS.has(event.toolName);
		const isWrite = WRITE_TOOLS.has(event.toolName);
		if (!isRead && !isWrite) return undefined;

		const input = event.input as { path?: string };
		const rawPath = typeof input.path === "string" && input.path ? input.path : ".";
		const target = isAbsolute(rawPath) ? normalize(rawPath) : normalize(resolve(cwd, rawPath));

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
