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

/** Simple substring filter standing in for pi-tui's fuzzyFilter. */
const contains = (items, query, getText) => items.filter((i) => getText(i).includes(query));

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
	eq("query filters subdirs", filtered.map((e) => e.label), ["./", "../", "~/", "alpha/", "alpha-link/"]);
	const { entries: hidden } = buildPickerEntries(ROOT, ".", contains);
	eq("dot query reveals hidden", hidden.map((e) => e.label), ["./", "../", "~/", ".hidden/"]);
	const { entries: missing, error: missingErr } = buildPickerEntries(join(ROOT, "nope"), "", contains);
	eq("unreadable keeps pinned", missing.map((e) => e.label), ["./", "../", "~/"]);
	eq("unreadable flags error", missingErr, true);
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

console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);
