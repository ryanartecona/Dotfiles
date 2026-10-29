#!/usr/bin/env node
// RPC-driven integration test for the rehome extension.
// Spawns `pi --mode rpc` against fixture sessions and asserts on disk state.
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { readdirSync } from "node:fs";
import { join } from "node:path";

const ROOT = realpathSync("/tmp/rehome-test");
const AGENT_DIR = join(ROOT, "agent");
const SESSION_DIR = join(ROOT, "sessions");
const EXT = new URL("../index.ts", import.meta.url).pathname;
const PI = process.env.PI_BIN ?? "pi";

const FORK_LABEL = "Fork: rehome a copy here, keep the old session file";
const MOVE_LABEL = "Move: rehome a copy here, delete the old session file";
const KEEP_LABEL = "Keep the session's original cwd";

let failures = 0;
function check(name, cond, extra = "") {
  if (cond) console.log(`  ok: ${name}`);
  else { failures++; console.log(`  FAIL: ${name} ${extra}`); }
}

function makeSession(path, cwd, id, text) {
  const ts = new Date().toISOString();
  const lines = [
    JSON.stringify({ type: "session", version: 3, id, timestamp: ts, cwd }),
    JSON.stringify({
      type: "message", id: "aaaaaaaa", parentId: null, timestamp: ts,
      message: { role: "user", content: text, timestamp: Date.now() },
    }),
  ];
  writeFileSync(path, lines.join("\n") + "\n");
}

function sessionDirFor(cwd) {
  const real = realpathSync(cwd);
  return join(AGENT_DIR, "sessions", `--${real.replace(/^\//, "").replace(/[/\\:]/g, "-")}--`);
}

function norm(p) {
  if (typeof p !== "string") return p;
  try { return realpathSync(p); } catch { return p; }
}

class Rpc {
  constructor(cwd, sessionPath) {
    this.lines = [];
    this.waiters = [];
    this.reqId = 0;
    this.consumed = new Set();
    const env = {
      ...process.env,
      PI_CODING_AGENT_DIR: AGENT_DIR,
      PI_CODING_AGENT_SESSION_DIR: SESSION_DIR,
    };
    this.proc = spawn(PI, ["--mode", "rpc", "--session", sessionPath, "--extension", EXT], { cwd, env });
    this.proc.stderr.on("data", (d) => process.stderr.write(`[pi-stderr] ${d}`));
    let buf = "";
    this.proc.stdout.on("data", (d) => {
      buf += d.toString();
      let idx;
      while ((idx = buf.indexOf("\n")) >= 0) {
        const line = buf.slice(0, idx).trim();
        buf = buf.slice(idx + 1);
        if (!line) continue;
        let msg;
        try { msg = JSON.parse(line); } catch { console.log(`[pi-out] ${line}`); continue; }
        this.dispatch(msg);
      }
    });
    this.proc.on("exit", (code) => {
      console.log(`[pi exited: ${code}]`);
      this.dispatch({ type: "__exit__", code });
    });
  }
  dispatch(msg) {
    this.lines.push(msg);
    this.waiters = this.waiters.filter((w) => {
      if (w.pred(msg)) { w.resolve(msg); return false; }
      return true;
    });
  }
  waitFor(pred, timeoutMs = 20000, what = "condition") {
    const existing = this.lines.find(pred);
    if (existing) return Promise.resolve(existing);
    return new Promise((resolve, reject) => {
      const t = setTimeout(() => reject(new Error(`timeout waiting for ${what}`)), timeoutMs);
      this.waiters.push({ pred, resolve: (m) => { clearTimeout(t); resolve(m); } });
    });
  }
  send(obj) {
    this.proc.stdin.write(JSON.stringify(obj) + "\n");
  }
  async command(type, extra = {}) {
    const id = `req-${++this.reqId}`;
    this.send({ id, type, ...extra });
    return this.waitFor((m) => m.type === "response" && m.id === id, 20000, `response to ${type}`);
  }
  /** Respond to the next unconsumed extension_ui_request select with the given option. */
  async expectSelectAndRespond(option, timeoutMs = 20000) {
    const req = await this.waitFor(
      (m) => m.type === "extension_ui_request" && m.method === "select" && !this.consumed.has(m.id),
      timeoutMs, "select request");
    this.consumed.add(req.id);
    console.log(`  [select] "${req.title?.split("\n")[0]}" options=${JSON.stringify(req.options)} -> ${option}`);
    this.send({ type: "extension_ui_response", id: req.id, value: option });
    return req;
  }
  async expectNotify(substr, timeoutMs = 20000) {
    const req = await this.waitFor(
      (m) => m.type === "extension_ui_request" && m.method === "notify" && !this.consumed.has(m.id) &&
        String(m.message ?? m.text ?? "").includes(substr),
      timeoutMs, `notify containing "${substr}"`);
    this.consumed.add(req.id);
    return req;
  }
  kill() { try { this.proc.kill("SIGTERM"); } catch {} }
}

function readJsonl(path) {
  return readFileSync(path, "utf8").trim().split("\n").map((l) => JSON.parse(l));
}

async function settle(ms = 800) { await new Promise((r) => setTimeout(r, ms)); }

// ---------------------------------------------------------------------------

async function scenario1() {
  console.log("\n== Scenario 1: startup mismatch prompt, choose MOVE ==");
  const dirA = join(ROOT, "dirA"), dirB = join(ROOT, "dirB");
  const sA = join(SESSION_DIR, "s1_a.jsonl");
  makeSession(sA, dirA, "sess-1a", "hello from scenario 1");
  const rpc = new Rpc(dirB, sA);
  try {
    await rpc.expectSelectAndRespond(MOVE_LABEL);
    await rpc.expectNotify("Rehomed to");
    await settle();
    const stats = await rpc.command("get_session_stats");
    const newFile = stats.data?.sessionFile;
    console.log(`  sessionFile: ${newFile}`);
    check("session file moved into dirB session dir", newFile?.startsWith(sessionDirFor(dirB)));
    check("old file deleted (move)", !existsSync(sA));
    const entries = readJsonl(newFile);
    check("header cwd is dirB", norm(entries[0].cwd) === norm(dirB), JSON.stringify(entries[0]));
    check("parentSession backlink", entries[0].parentSession === sA);
    check("history copied (user message)", entries.some((e) => e.message?.content === "hello from scenario 1"));
    const marker = entries.find((e) => e.type === "custom_message" && e.customType === "rehome");
    check("rehome marker present in new file", !!marker);
    check("marker details", norm(marker?.details?.from) === norm(dirA) && norm(marker?.details?.to) === norm(dirB) && marker?.details?.mode === "move");
  } finally { rpc.kill(); }
}

async function scenario2() {
  console.log("\n== Scenario 2: /rehome command (flag + select + same-dir no-op) ==");
  const dirA = join(ROOT, "dirA"), dirB = join(ROOT, "dirB"), dirC = join(ROOT, "dirC");
  const sA = join(SESSION_DIR, "s2_a.jsonl");
  makeSession(sA, dirA, "sess-2a", "hello from scenario 2");
  const rpc = new Rpc(dirB, sA);
  try {
    await rpc.expectSelectAndRespond(KEEP_LABEL); // dismiss startup prompt, stay on dirA
    await settle();
    // 2a: /rehome with explicit path + --fork
    await rpc.send({ type: "prompt", message: `/rehome ${dirC} --fork` });
    await rpc.expectNotify("Rehomed to");
    await settle();
    let stats = await rpc.command("get_session_stats");
    const fileC = stats.data?.sessionFile;
    check("2a: session now in dirC dir", fileC?.startsWith(sessionDirFor(dirC)), fileC);
    check("2a: original file kept (fork)", existsSync(sA));
    const entriesC = readJsonl(fileC);
    check("2a: header cwd is dirC", norm(entriesC[0].cwd) === norm(dirC));
    check("2a: marker appended to new file", entriesC.some((e) => e.customType === "rehome"));
    const entriesA = readJsonl(sA);
    check("2a: original file left untouched (no marker)", !entriesA.some((e) => e.customType === "rehome"));
    // 2b: /rehome with no args -> already in process.cwd() (dirC after chdir)
    await rpc.send({ type: "prompt", message: "/rehome" });
    await rpc.expectNotify("already lives");
    // 2c: /rehome with path, choose MOVE in the select
    await rpc.send({ type: "prompt", message: `/rehome ${dirB}` });
    await rpc.expectSelectAndRespond(MOVE_LABEL);
    await rpc.expectNotify("Rehomed to");
    await settle();
    stats = await rpc.command("get_session_stats");
    const fileB = stats.data?.sessionFile;
    check("2c: session now in dirB dir", fileB?.startsWith(sessionDirFor(dirB)), fileB);
    check("2c: dirC file deleted (move)", !existsSync(fileC));
    // 2d: /rehome ~ expands the tilde to the home directory
    await rpc.send({ type: "prompt", message: "/rehome ~ --fork" });
    await rpc.expectNotify("Rehomed to");
    await settle();
    stats = await rpc.command("get_session_stats");
    const fileHome = stats.data?.sessionFile;
    check("2d: '~' expanded to home dir", fileHome?.startsWith(sessionDirFor(homedir())), fileHome);
  } finally { rpc.kill(); }
}

async function scenario3() {
  console.log("\n== Scenario 3: /resume (switch_session) intercept ==");
  const dirB = join(ROOT, "dirB"), dirX = join(ROOT, "dirX"), dirY = join(ROOT, "dirY");
  mkdirSync(dirX, { recursive: true }); mkdirSync(dirY, { recursive: true });
  const sB = join(SESSION_DIR, "s3_b.jsonl");
  const sX = join(SESSION_DIR, "s3_x.jsonl");
  const sY = join(SESSION_DIR, "s3_y.jsonl");
  makeSession(sB, dirB, "sess-3b", "home session");
  makeSession(sX, dirX, "sess-3x", "foreign session X");
  makeSession(sY, dirY, "sess-3y", "foreign session Y");
  const rpc = new Rpc(dirB, sB);
  try {
    await settle(1500); // no startup prompt expected (cwd matches)
    // 3a: switch to foreign session, choose KEEP (native behavior)
    const p1 = rpc.command("switch_session", { sessionPath: sX });
    await rpc.expectSelectAndRespond(KEEP_LABEL);
    const r1 = await p1;
    check("3a: native switch not cancelled", r1.data?.cancelled === false, JSON.stringify(r1));
    await settle();
    let stats = await rpc.command("get_session_stats");
    check("3a: now on foreign file unchanged", stats.data?.sessionFile === sX, stats.data?.sessionFile);
    check("3a: header cwd still dirX (tied)", norm(readJsonl(sX)[0].cwd) === norm(dirX));
    // 3b: switch to another foreign session, choose MOVE
    const p2 = rpc.command("switch_session", { sessionPath: sY });
    await rpc.expectSelectAndRespond(MOVE_LABEL);
    const r2 = await p2;
    check("3b: pi's native switch cancelled by rehome", r2.data?.cancelled === true, JSON.stringify(r2));
    await rpc.expectNotify("Rehomed to");
    await settle();
    stats = await rpc.command("get_session_stats");
    const newFile = stats.data?.sessionFile;
    // ambient process cwd is still dirB (keep-choice did not chdir)
    check("3b: session rehomed into dirB dir", newFile?.startsWith(sessionDirFor(dirB)), newFile);
    check("3b: original foreign file deleted (move)", !existsSync(sY));
    const entries = readJsonl(newFile);
    check("3b: header cwd is dirB", norm(entries[0].cwd) === norm(dirB));
    check("3b: history copied", entries.some((e) => e.message?.content === "foreign session Y"));
    check("3b: marker appended post-switch", entries.some((e) => e.customType === "rehome" && norm(e.details?.from) === norm(dirY)));
  } finally { rpc.kill(); }
}

async function main() {
  rmSync(ROOT, { recursive: true, force: true });
  for (const d of ["dirA", "dirB", "dirC", "dirX", "dirY", "agent", "sessions"]) {
    mkdirSync(join(ROOT, d), { recursive: true });
  }
  await scenario1();
  await scenario2();
  await scenario3();
  console.log(failures === 0 ? "\nALL PASS" : `\n${failures} FAILURES`);
  process.exit(failures === 0 ? 0 : 1);
}

main().catch((e) => { console.error(e); process.exit(1); });
