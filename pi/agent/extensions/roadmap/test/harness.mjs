// Behavior harness for the roadmap extension: loads ../index.ts with
// pi's bundled jiti and drives the event handlers with fake events.
// Run: node test/harness.mjs
// jiti is resolved from the pi installation; override with PI_JITI
// (path to jiti's package.json) when the nix store path has changed.
import { createRequire } from "node:module";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const EXTENSION = fileURLToPath(new URL("../index.ts", import.meta.url));
const JITI =
  process.env.PI_JITI ??
  "/nix/store/qmradqpzvmk1697yzp1dny70gb9rqp2g-pi-1.4.0/lib/pi/node_modules/jiti/package.json";
const FIXTURE_DIR = fs.mkdtempSync(path.join(os.tmpdir(), "roadmap-ext-test-"));
process.on("exit", () => fs.rmSync(FIXTURE_DIR, { recursive: true, force: true }));

// Fixture: a Shape-violating roadmap (code block at lines 7-9).
fs.mkdirSync(path.join(FIXTURE_DIR, "ai/roadmaps"), { recursive: true });
fs.writeFileSync(path.join(FIXTURE_DIR, "ai/roadmaps/x.md"), `# X

## Plan

Goal: do the thing.

\`\`\`rust
fn parse() {}
\`\`\`

### 1. First stage

Some intent text here.
`);

const require = createRequire(JITI);
const createJiti = require("jiti");
const jiti = createJiti(import.meta.url);
const mod = await jiti.import(EXTENSION);
const factory = mod.default ?? mod;

const handlers = {};
const pi = { on: (event, handler) => { handlers[event] = handler; return () => {}; } };
factory(pi);

const scratch = FIXTURE_DIR;
const ctx = {
  cwd: scratch,
  hasUI: false,
  sessionManager: { getBranch: () => branch },
  ui: { notify: (msg, level) => console.log(`NOTIFY[${level}]: ${msg}`) },
};
let branch = [];

let failures = 0;
function check(name, cond, extra = "") {
  console.log(`${cond ? "PASS" : "FAIL"}  ${name}${cond ? "" : "  " + extra}`);
  if (!cond) failures++;
}

function writeResult(filePath) {
  return {
    type: "tool_result", toolCallId: "t" + Math.random(), toolName: "write",
    input: { path: filePath, content: "x" },
    content: [{ type: "text", text: "File written" }], isError: false, details: undefined,
  };
}
function editResult(filePath, patch) {
  return {
    type: "tool_result", toolCallId: "t" + Math.random(), toolName: "edit",
    input: { path: filePath, edits: [] },
    content: [{ type: "text", text: "Edited" }], isError: false,
    details: { patch, diff: "", firstChangedLine: 1 },
  };
}
function settle() {
  return handlers.agent_before_settle(
    { type: "agent_before_settle", entries: [], continue: false, context: {}, outcome: "completed" }, ctx);
}

// --- Hook 1: lint on write --------------------------------------------
// write a Shape-violating roadmap -> reason appended, scoped to whole file
let r = await handlers.tool_result(writeResult("ai/roadmaps/x.md"), ctx);
check("write: bad Shape roadmap gets lint reason",
  r && r.content.some((c) => c.text?.includes("broke the Shape rung bar")),
  JSON.stringify(r));

// the same write, as an edit whose patch only touches the stage intent
// (lines 13-15, away from the code block at 7-9) -> no finding
const patchClean = "@@ -12,3 +12,4 @@\n ### 1. First stage\n \n Tags exist.\n+More intent.\n";
r = await handlers.tool_result(editResult("ai/roadmaps/x.md", patchClean), ctx);
check("edit: patch outside the violation is clean", r === undefined, JSON.stringify(r));

// an edit whose patch covers the code block -> finding
const patchDirty = "@@ -6,4 +6,5 @@\n Goal: tag everything.\n \n ```rust\n fn parse() {}\n+x\n";
r = await handlers.tool_result(editResult("ai/roadmaps/x.md", patchDirty), ctx);
check("edit: patch over the code block gets lint reason",
  r && r.content.some((c) => c.text?.includes("broke the Shape rung bar")), JSON.stringify(r));

// writes outside ai/roadmaps are untouched
r = await handlers.tool_result(writeResult("src/main.rs"), ctx);
check("write: non-roadmap file untouched", r === undefined, JSON.stringify(r));

// --- Hook 2: journal on settle -----------------------------------------
// State so far (handler order above): x.md write(1, rm), x.md edit(2, rm),
// x.md edit(3, rm), main.rs write(4, code). Engaged via the roadmap paths.
let s = await settle();
check("settle: engaged + code write after roadmap write -> journal prompt",
  s && s.continue === true &&
  s.entries[0]?.customType === "roadmap-journal" &&
  s.entries[0]?.content === "Run the /skill:roadmap-journal skill.",
  JSON.stringify(s));

// second settle right after: watermark blocks the loop
s = await settle();
check("settle: own continuation does not re-prompt", s === undefined, JSON.stringify(s));

// a user message that edits nothing -> still no prompt
s = await settle();
check("settle: no new writes -> no prompt", s === undefined, JSON.stringify(s));

// a roadmap write (the journal entry) also settles the debt
await handlers.tool_result(editResult("ai/roadmaps/x.md", "@@ -15,1 +15,2 @@\n Tags exist.\n+\n"), ctx);
s = await settle();
check("settle: roadmap write after code write -> no prompt", s === undefined, JSON.stringify(s));

// new code work re-arms the prompt
await handlers.tool_result(writeResult("src/other.rs"), ctx);
s = await settle();
check("settle: new code write re-arms journal prompt", s && s.continue === true, JSON.stringify(s));

// --- rebuild from branch ----------------------------------------------
branch = [
  { type: "message", id: "1", parentId: null, timestamp: "t",
    message: { role: "assistant", content: [
      { type: "toolCall", id: "a", name: "read", arguments: { path: "ai/roadmaps/x.md" } },
    ] } },
  { type: "message", id: "2", parentId: "1", timestamp: "t",
    message: { role: "assistant", content: [
      { type: "toolCall", id: "b", name: "write", arguments: { path: "src/main.rs", content: "" } },
    ] } },
  { type: "custom_message", id: "3", parentId: "2", timestamp: "t",
    customType: "roadmap-journal", content: "Run the /skill:roadmap-journal skill.", display: true },
];
handlers.session_start({ type: "session_start" }, ctx);
s = await settle();
check("rebuild: prompt entry after last code write -> no re-prompt", s === undefined, JSON.stringify(s));

// skill command in a user message counts as engagement
branch = [
  { type: "message", id: "1", parentId: null, timestamp: "t",
    message: { role: "user", content: "/skill:roadmap-stage" } },
  { type: "message", id: "2", parentId: "1", timestamp: "t",
    message: { role: "assistant", content: [
      { type: "toolCall", id: "b", name: "write", arguments: { path: "src/main.rs", content: "" } },
    ] } },
];
handlers.session_start({ type: "session_start" }, ctx);
s = await settle();
check("rebuild: /skill command engages, code write prompts", s && s.continue === true, JSON.stringify(s));

// aborted runs never prompt
branch = [];
handlers.session_start({ type: "session_start" }, ctx);
await handlers.tool_result(writeResult("ai/roadmaps/x.md"), ctx);
await handlers.tool_result(writeResult("src/z.rs"), ctx);
s = await handlers.agent_before_settle(
  { type: "agent_before_settle", entries: [], continue: false, context: {}, outcome: "aborted" }, ctx);
check("settle: aborted outcome -> no prompt", s === undefined, JSON.stringify(s));

console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURES`);
process.exit(failures === 0 ? 0 : 1);
