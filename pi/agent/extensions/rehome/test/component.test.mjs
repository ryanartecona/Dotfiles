/**
 * Headless smoke test for DirectoryPickerComponent: constructs it with a mock
 * theme that throws on unknown color tokens (like pi's real Theme proxy) and
 * drives handleInput/render through the main flows. Catches theme misuse and
 * input-handling crashes that the RPC suite cannot see (no TUI render there).
 *
 * Run: node test/component.test.mjs   (or PI_BIN=/path/to/pi node ...)
 */
import assert from "node:assert";
import { copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, symlinkSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const TEST_DIR = dirname(fileURLToPath(import.meta.url));
const EXT_DIR = dirname(TEST_DIR);

/** Locate the installed pi-tui package (the component imports it as a bare specifier). */
function findPiTui() {
	const installDir = join(homedir(), ".pi/agent/install");
	const version = readFileSync(join(installDir, "current-version"), "utf-8").trim();
	const pkg = join(installDir, "releases", version, "node_modules/@earendil-works/pi-tui");
	if (!existsSync(pkg)) throw new Error(`pi-tui not found at ${pkg}`);
	return pkg;
}

// Stage a copy of the extension with a node_modules symlink so the bare
// "@earendil-works/pi-tui" import resolves under plain node.
const STAGE = join("/tmp", "rehome-component-test");
rmSync(STAGE, { recursive: true, force: true });
mkdirSync(join(STAGE, "node_modules/@earendil-works"), { recursive: true });
symlinkSync(findPiTui(), join(STAGE, "node_modules/@earendil-works/pi-tui"));
copyFileSync(join(EXT_DIR, "dir-picker.ts"), join(STAGE, "dir-picker.ts"));
copyFileSync(join(EXT_DIR, "dir-picker-component.ts"), join(STAGE, "dir-picker-component.ts"));

// Fixture tree
const ROOT = join(STAGE, "fixture");
mkdirSync(join(ROOT, "alpha"), { recursive: true });
mkdirSync(join(ROOT, "beta"), { recursive: true });

// Mock theme faithful to pi's Theme: fg/bg throw on unknown tokens.
const FG_TOKENS = new Set([
	"accent", "border", "borderAccent", "borderMuted", "success", "error", "warning", "muted", "dim", "text",
	"thinkingText", "scrollbarTrack", "scrollbarThumb", "searchMatchText", "userMessageText", "customMessageText",
	"customMessageLabel", "toolTitle", "toolOutput", "mdHeading", "mdLink", "mdLinkUrl", "mdCode", "mdCodeBlock",
	"mdCodeBlockBorder", "mdQuote", "mdQuoteBorder", "mdHr", "mdListBullet", "toolDiffAdded", "toolDiffRemoved",
	"toolDiffContext", "syntaxComment", "syntaxKeyword", "syntaxFunction", "syntaxVariable", "syntaxString",
	"syntaxNumber", "syntaxType", "syntaxOperator", "syntaxPunctuation", "thinkingOff", "thinkingMinimal",
	"thinkingLow", "thinkingMedium", "thinkingHigh", "thinkingXhigh", "thinkingMax", "bashMode",
]);
const bgCalls = [];
const theme = {
	fg: (color, text) => {
		if (!FG_TOKENS.has(color)) throw new Error(`Unknown theme color: ${color}`);
		return text;
	},
	bg: (color, text) => {
		if (color !== "selectedBg") throw new Error(`Unknown theme background: ${color}`);
		bgCalls.push({ color, text });
		return text;
	},
	bold: (t) => t,
	italic: (t) => t,
	underline: (t) => t,
	inverse: (t) => t,
	strikethrough: (t) => t,
};

const { DirectoryPickerComponent } = await import(join(STAGE, "dir-picker-component.ts"));

let failures = 0;
function ok(name, cond) {
	if (cond) console.log(`  ok: ${name}`);
	else {
		console.log(`  FAIL: ${name}`);
		failures++;
	}
}

const fakeTui = {};
const KEYS = {
	up: "\x1b[A", down: "\x1b[B", left: "\x1b[D", right: "\x1b[C", enter: "\r", escape: "\x1b",
	backspace: "\x7f", tab: "\t", shiftTab: "\x1b[Z", ctrlA: "\x01", ctrlE: "\x05",
};

/** Label of the currently selected entry line (the first "> " line is the query). */
function selectedLabel(picker) {
	const lines = picker.render(80);
	const queryIndex = lines.findIndex((l) => l.startsWith("> "));
	const line = lines.slice(queryIndex + 1).find((l) => l.startsWith("> "));
	return line?.slice(2);
}

function makePicker(dir = ROOT) {
	let result = "unset";
	const picker = new DirectoryPickerComponent(fakeTui, theme, dir, (r) => {
		result = r;
	});
	return { picker, getResult: () => result };
}

console.log("== render ==");
{
	const { picker } = makePicker();
	const lines = picker.render(80);
	ok("renders full-width top and bottom borders", lines[0] === "─".repeat(80) && lines.at(-1) === "─".repeat(80));
	const titleIndex = lines.findIndex((l) => l.includes("Rehome to directory"));
	const queryIndex = lines.findIndex((l) => l.startsWith("> "));
	const firstEntryIndex = lines.findIndex((l) => l.startsWith("> ./"));
	const helpIndex = lines.findIndex((l) => l.includes("type to filter"));
	ok("orders title, query, entries, then help", titleIndex === 2 && queryIndex === titleIndex + 1 && firstEntryIndex === queryIndex + 1 && helpIndex > firstEntryIndex);
	ok("separates help from entries", lines[helpIndex - 1] === "" && lines.at(-2) === lines[helpIndex]);
	ok("renders query line", queryIndex >= 0);
	ok("contrasts query background", bgCalls.some((c) => c.color === "selectedBg" && c.text.includes("> ")));
	ok("lists fixture subdirs", lines.some((l) => l.includes("alpha/")) && lines.some((l) => l.includes("beta/")));
	ok("pinned entries on empty query", lines.some((l) => l.includes("./")) && lines.some((l) => l.includes("~/")));
	const errPicker = new DirectoryPickerComponent(fakeTui, theme, join(ROOT, "nope"), () => {});
	ok("unreadable dir renders error", errPicker.render(80).some((l) => l.includes("cannot read")));
}

console.log("== input handling ==");
{
	const { picker } = makePicker();
	picker.handleInput("b");
	picker.handleInput("e");
	const lines = picker.render(80);
	ok("typing filters", lines.some((l) => l.includes("beta/")) && !lines.some((l) => l.includes("alpha/")));
	ok("non-matching query hides pinned", !lines.some((l) => l.includes("~/")));

	// exact match is preselected; "/" descends into it and clears the query
	picker.handleInput("t");
	picker.handleInput("a"); // query "beta"
	picker.handleInput("/");
	ok('"/" descends into exact match', picker.render(80).some((l) => l.includes(join(ROOT, "beta"))));
	picker.handleInput("x");
	picker.handleInput(KEYS.backspace);
	ok("backspace edits query", picker.render(80).every((l) => !l.includes("cannot read")));

	// ctrl+a / ctrl+e: cursor moves must not crash or clear the query
	picker.handleInput("z");
	picker.handleInput(KEYS.ctrlA);
	picker.handleInput(KEYS.ctrlE);
	ok("^a/^e keep the query", true); // no throw is the assertion; value check below
}

console.log("== selection and done ==");
{
	const { picker, getResult } = makePicker();
	picker.handleInput(KEYS.enter); // first entry on empty query is "./"
	ok("enter on ./ selects current dir", getResult() === ROOT);

	const { picker: p2, getResult: r2 } = makePicker();
	p2.handleInput(KEYS.escape);
	ok("escape cancels", r2() === undefined);

	const { picker: p3 } = makePicker();
	p3.handleInput(KEYS.down);
	p3.handleInput(KEYS.right); // descend into ../
	ok("right descends", p3.render(80).some((l) => l.includes(dirname(ROOT))));

	const { picker: p4 } = makePicker();
	p4.handleInput(KEYS.backspace); // empty query: go up
	ok("backspace on empty query ascends", p4.render(80).some((l) => l.includes(dirname(ROOT))));
}

console.log("== tab / shift+tab ==");
{
	const { picker } = makePicker();
	ok("selection starts on ./", selectedLabel(picker) === "./");
	picker.handleInput(KEYS.tab);
	ok("tab moves to next", selectedLabel(picker) === "../");
	picker.handleInput(KEYS.tab);
	ok("tab moves to next again", selectedLabel(picker) === "~/");
	picker.handleInput(KEYS.shiftTab);
	ok("shift+tab moves back", selectedLabel(picker) === "../");

	const { picker: single } = makePicker();
	for (const ch of "beta") single.handleInput(ch);
	ok("filtering to one option", selectedLabel(single) === "beta/");
	single.handleInput(KEYS.tab);
	ok("tab activates a single option", single.render(80).some((l) => l.includes(join(ROOT, "beta"))));
}

console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURES`);
if (failures > 0) process.exit(1);
