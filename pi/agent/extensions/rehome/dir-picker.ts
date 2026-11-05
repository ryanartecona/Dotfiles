/**
 * Interactive directory picker for /rehome, modeled on pi's own
 * SessionSelectorComponent: a query line with type-to-filter, a navigable
 * list, Enter to descend, Esc to cancel.
 *
 * The fs helpers (splitPathPrefix, listSubdirs, buildPickerEntries) use only
 * node builtins so test/dir-picker.test.mjs can exercise them directly with
 * node's type stripping. The component glue uses pi-tui (virtual module,
 * only importable inside pi).
 */

import { readdirSync, realpathSync, statSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, isAbsolute, join, resolve } from "node:path";

export interface PickerEntry {
	/** Display label, e.g. "src/" or "./" */
	label: string;
	/** Absolute directory this entry points at. */
	path: string;
	/** "select" chooses the directory as the result; "enter" descends into it. */
	action: "select" | "enter";
	/** Pinned entries stay on top and are not filtered out by the query. */
	pinned: boolean;
}

/** Expand a leading ~ like pi does (~ and ~/ only). Shared with index.ts. */
export function expandTilde(p: string): string {
	if (p === "~") return homedir();
	if (p.startsWith("~/")) return join(homedir(), p.slice(2));
	return p;
}

/**
 * Split a path prefix into the directory part to list (kept verbatim for
 * completion values) and the partial name being typed.
 * "src/ut" -> ["src/", "ut"]; "src/" -> ["src/", ""]; "ut" -> ["", "ut"].
 */
export function splitPathPrefix(prefix: string): [dirPart: string, partial: string] {
	const slash = prefix.lastIndexOf("/");
	if (slash < 0) return ["", prefix];
	return [prefix.slice(0, slash + 1), prefix.slice(slash + 1)];
}

/** Resolve a possibly relative, tilde-containing directory part against cwd. */
export function resolveDirPart(dirPart: string, cwd: string): string {
	const expanded = expandTilde(dirPart);
	const abs = isAbsolute(expanded) ? expanded : resolve(cwd, expanded);
	try {
		return realpathSync.native(abs);
	} catch {
		return abs;
	}
}


/** List visible subdirectory names of a directory. Returns undefined on error. */
export function listSubdirs(absDir: string, includeHidden: boolean): string[] | undefined {
	try {
		return readdirSync(absDir, { withFileTypes: true })
			.filter((d) => {
				if (d.isDirectory()) return true;
				// Dirent.isDirectory() is false for symlinks; follow them so
				// symlinked directories (e.g. dotfile setups) still appear.
				if (!d.isSymbolicLink()) return false;
				try {
					return statSync(join(absDir, d.name)).isDirectory();
				} catch {
					return false;
				}
			})
			.map((d) => d.name)
			.filter((name) => includeHidden || !name.startsWith("."))
			.sort((a, b) => a.localeCompare(b));
	} catch {
		return undefined;
	}
}

/**
 * Build the picker's entry list: pinned [./ (select), ../, ~/] followed by
 * the current directory's subdirectories.
 *
 * With an empty query, all pinned entries and all subdirectories are shown.
 * With a non-empty query, everything — pinned entries included — is filtered
 * through `filter`, and an exact subdirectory-name match is bumped to the
 * top (so Enter or "/" descends into it immediately).
 * `filter` is injected so the pure logic stays testable without pi-tui.
 */
export function buildPickerEntries(
	currentDir: string,
	query: string,
	filter: <T>(items: T[], query: string, getText: (item: T) => string) => T[],
): { entries: PickerEntry[]; error: boolean } {
	const pinned: PickerEntry[] = [
		{ label: "./", path: currentDir, action: "select", pinned: true },
		{ label: "../", path: dirname(currentDir), action: "enter", pinned: true },
		{ label: "~/", path: homedir(), action: "enter", pinned: true },
	];
	const names = listSubdirs(currentDir, query.startsWith("."));
	if (names === undefined) {
		return { entries: query ? filter(pinned, query, (e) => e.label) : pinned, error: true };
	}
	const subs: PickerEntry[] = names.map((name) => ({
		label: `${name}/`,
		path: join(currentDir, name),
		action: "enter" as const,
		pinned: false,
	}));
	if (!query) {
		return { entries: [...pinned, ...subs], error: false };
	}
	const filteredPinned = filter(pinned, query, (e) => e.label);
	const filteredSubs = filter(subs, query, (e) => e.label);
	// Exact name match first, so it is selected by default.
	const q = query.toLowerCase();
	filteredSubs.sort((a, b) => Number(isExactDirMatch(b, q)) - Number(isExactDirMatch(a, q)));
	return { entries: [...filteredPinned, ...filteredSubs], error: false };
}

function isExactDirMatch(entry: PickerEntry, lowerQuery: string): boolean {
	return entry.label.slice(0, -1).toLowerCase() === lowerQuery;
}

/**
 * Find the descendable entry whose directory name exactly matches the query.
 * Backs the "/" keystroke: typing "ra/" descends into "ra" like Enter would.
 */
export function findExactDirEntry(entries: PickerEntry[], query: string): PickerEntry | undefined {
	if (!query) return undefined;
	const q = query.toLowerCase();
	return entries.find((e) => e.action === "enter" && isExactDirMatch(e, q));
}

/**
 * Directory completions for /rehome arguments: subdirectories matching the
 * typed prefix, plus the --fork/--move flags for dash prefixes. Values carry
 * a trailing slash so completion continues into subdirectories.
 * `filter` is injected for testability.
 */
export function completeDirectories(
	prefix: string,
	cwd: string,
	filter: <T>(items: T[], query: string, getText: (item: T) => string) => T[],
): { value: string; label: string }[] {
	if (prefix.startsWith("-")) {
		const flags = ["--fork", "--move"];
		return filter(flags, prefix, (f) => f).map((f) => ({ value: f, label: f }));
	}
	// Split before expanding so completion values preserve the user's ~.
	const [dirPart, partial] = splitPathPrefix(prefix);
	const baseDir = dirPart ? resolveDirPart(dirPart, cwd) : cwd;
	const names = listSubdirs(baseDir, partial.startsWith("."));
	if (names === undefined) return [];
	const filtered = partial ? filter(names, partial, (n) => n) : names;
	return filtered.slice(0, 20).map((name) => ({
		value: `${dirPart}${name}/`,
		label: `${name}/`,
	}));
}
