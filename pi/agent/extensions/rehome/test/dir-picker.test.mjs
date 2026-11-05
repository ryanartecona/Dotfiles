#!/usr/bin/env node
// Unit tests for the pure directory-picker helpers (no pi runtime needed).
// Run: node test/dir-picker.test.mjs   (node >= 23 strips TS types natively)
import { mkdirSync, rmSync, symlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import {
	buildPickerEntries,
	completeDirectories,
	expandTilde,
	findExactDirEntry,
	listSubdirs,
	resolveDirPart,
	splitPathPrefix,
} from "../dir-picker.ts";
import { realpathSync } from "node:fs";

/** realpath for expectations (macOS /tmp is a symlink). */
function realpath(p) {
	try {
		return realpathSync.native(p);
	} catch {
		return p;
	}
}

let failures = 0;
function eq(name, actual, expected) {
	const a = JSON.stringify(actual);
	const e = JSON.stringify(expected);
	if (a === e) console.log(`  ok: ${name}`);
	else {
		failures++;
		console.log(`  FAIL: ${name}\n    expected: ${e}\n    actual:   ${a}`);
	}
}

/** Simple substring filter standing in for pi-tui's fuzzyFilter (which is case-insensitive). */
const contains = (items, query, getText) =>
	items.filter((i) => getText(i).toLowerCase().includes(query.toLowerCase()));

// Fixture tree
const ROOT = join("/tmp", "rehome-picker-test");
rmSync(ROOT, { recursive: true, force: true });
mkdirSync(join(ROOT, "alpha", "nested"), { recursive: true });
mkdirSync(join(ROOT, "beta"), { recursive: true });
mkdirSync(join(ROOT, ".hidden"), { recursive: true });
mkdirSync(join(ROOT, "home", "code"), { recursive: true });
writeFileSync(join(ROOT, "a-file.txt"), "not a directory");
symlinkSync(join(ROOT, "alpha"), join(ROOT, "alpha-link"));
// os.homedir() prefers $HOME on POSIX: point it at the fixture for determinism.
process.env.HOME = realpath(join(ROOT, "home"));
const HOME = homedir();

console.log("== splitPathPrefix ==");
eq("no slash", splitPathPrefix("ut"), ["", "ut"]);
eq("with dir", splitPathPrefix("src/ut"), ["src/", "ut"]);
eq("trailing slash", splitPathPrefix("src/"), ["src/", ""]);
eq("absolute", splitPathPrefix("/tmp/re"), ["/tmp/", "re"]);
eq("empty", splitPathPrefix(""), ["", ""]);

console.log("== expandTilde ==");
eq("bare tilde", expandTilde("~"), HOME);
eq("tilde path", expandTilde("~/code"), join(HOME, "code"));
eq("no tilde", expandTilde("/tmp/x"), "/tmp/x");
eq("tilde not leading", expandTilde("/x/~"), "/x/~");

console.log("== resolveDirPart ==");
eq("absolute", resolveDirPart("/tmp", "/anywhere"), realpath("/tmp"));
eq("relative", resolveDirPart("src/", "/tmp/base"), "/tmp/base/src");
eq("tilde", resolveDirPart("~/", "/anywhere"), realpath(HOME));

console.log("== listSubdirs ==");
eq("dirs only, no hidden", listSubdirs(ROOT, false), ["alpha", "alpha-link", "beta", "home"]);
eq("hidden included", listSubdirs(ROOT, true), [".hidden", "alpha", "alpha-link", "beta", "home"]);
eq("missing dir", listSubdirs(join(ROOT, "nope"), false), undefined);

console.log("== buildPickerEntries ==");
{
	const { entries, error } = buildPickerEntries(ROOT, "", contains);
	eq("no error", error, false);
	eq("pinned first", entries.slice(0, 3).map((e) => e.label), ["./", "../", "~/"]);
	eq("select action on ./ only", entries.map((e) => e.action), ["select", "enter", "enter", "enter", "enter", "enter", "enter"]);
	eq("subdirs", entries.slice(3).map((e) => e.label), ["alpha/", "alpha-link/", "beta/", "home/"]);
	const { entries: filtered } = buildPickerEntries(ROOT, "lph", contains);
	eq("non-matching query filters pinned out too", filtered.map((e) => e.label), ["alpha/", "alpha-link/"]);
	const { entries: exact } = buildPickerEntries(ROOT, "alpha", contains);
	eq("exact match bumped to top", exact.map((e) => e.label), ["alpha/", "alpha-link/"]);
	const { entries: exactCase } = buildPickerEntries(ROOT, "BETA", contains);
	eq("exact match is case-insensitive", exactCase.map((e) => e.label), ["beta/"]);
	const { entries: hidden } = buildPickerEntries(ROOT, ".", contains);
	eq("matching pinned survive filtering", hidden.map((e) => e.label), ["./", "../", ".hidden/"]);
	const { entries: missing, error: missingErr } = buildPickerEntries(join(ROOT, "nope"), "", contains);
	eq("unreadable keeps pinned", missing.map((e) => e.label), ["./", "../", "~/"]);
	eq("unreadable flags error", missingErr, true);
	const { entries: missingFiltered } = buildPickerEntries(join(ROOT, "nope"), "zz", contains);
	eq("unreadable + query filters pinned", missingFiltered.map((e) => e.label), []);
}

console.log("== completeDirectories ==");
{
	eq("cwd subdirs", completeDirectories("", ROOT, contains).map((c) => c.value), ["alpha/", "alpha-link/", "beta/", "home/"]);
	eq("relative prefix", completeDirectories("al", ROOT, contains).map((c) => c.value), ["alpha/", "alpha-link/"]);
	eq("into subdir", completeDirectories("alpha/", ROOT, contains).map((c) => c.value), ["alpha/nested/"]);
	eq("absolute prefix", completeDirectories(`${ROOT}/be`, ROOT, contains).map((c) => c.value), [`${ROOT}/beta/`]);
	eq("flags", completeDirectories("--f", ROOT, contains).map((c) => c.value), ["--fork"]);
	eq("missing base dir", completeDirectories("nope/x", ROOT, contains), []);
	eq("file not offered", completeDirectories("a-file", ROOT, contains), []);
	eq("tilde completion", completeDirectories("~/co", ROOT, contains).map((c) => c.value), ["~/code/"]);
	eq("symlinked dir offered", completeDirectories("alpha-l", ROOT, contains).map((c) => c.value), ["alpha-link/"]);
}

console.log("== findExactDirEntry ==");
{
	const { entries } = buildPickerEntries(ROOT, "alpha", contains);
	eq("exact subdir found", findExactDirEntry(entries, "alpha")?.path, join(ROOT, "alpha"));
	eq("exact is case-insensitive", findExactDirEntry(entries, "ALPHA")?.path, join(ROOT, "alpha"));
	const { entries: tildeEntries } = buildPickerEntries(ROOT, "~", contains);
	eq("pinned ~/ found", findExactDirEntry(tildeEntries, "~")?.label, "~/");
	const { entries: dotEntries } = buildPickerEntries(ROOT, ".", contains);
	eq("./ is not descendable", findExactDirEntry(dotEntries, "."), undefined);
	eq("empty query", findExactDirEntry(entries, ""), undefined);
	eq("no match", findExactDirEntry(entries, "zzz"), undefined);
}

console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);
