#!/usr/bin/env node
// Verify other extensions see the rehomed cwd (ctx.cwd and process.cwd()) after a rehome.
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const ROOT = realpathSync("/tmp/rehome-test");
const AGENT_DIR = join(ROOT, "agent");
const SESSION_DIR = join(ROOT, "sessions");
const EXT = new URL("../index.ts", import.meta.url).pathname;
const PROBE = new URL("./cwd-probe.ts", import.meta.url).pathname;
const PI = process.env.PI_BIN ?? "pi";
const KEEP_LABEL = "Keep the session's original cwd";

rmSync(ROOT, { recursive: true, force: true });
for (const d of ["dirA", "dirB", "agent", "sessions"]) mkdirSync(join(ROOT, d), { recursive: true });

const dirA = join(ROOT, "dirA"), dirB = join(ROOT, "dirB");
const sA = join(SESSION_DIR, "probe_a.jsonl");
writeFileSync(sA, [
  JSON.stringify({ type: "session", version: 3, id: "probe-a", timestamp: new Date().toISOString(), cwd: dirA }),
  JSON.stringify({ type: "message", id: "aaaaaaaa", parentId: null, timestamp: new Date().toISOString(), message: { role: "user", content: "probe", timestamp: Date.now() } }),
].join("\n") + "\n");

const proc = spawn(PI, ["--mode", "rpc", "--session", sA, "--extension", EXT, "--extension", PROBE], {
  cwd: dirB,
  env: { ...process.env, PI_CODING_AGENT_DIR: AGENT_DIR, PI_CODING_AGENT_SESSION_DIR: SESSION_DIR },
});
let buf = "";
proc.stdout.on("data", (d) => {
  buf += d.toString();
  let i;
  while ((i = buf.indexOf("\n")) >= 0) {
    const line = buf.slice(0, i).trim(); buf = buf.slice(i + 1);
    if (!line) continue;
    let m; try { m = JSON.parse(line); } catch { continue; }
    if (m.type === "extension_ui_request" && m.method === "select") {
      proc.stdin.write(JSON.stringify({ type: "extension_ui_response", id: m.id, value: KEEP_LABEL }) + "\n");
      // after the startup prompt is dismissed, drive a /rehome
      setTimeout(() => proc.stdin.write(JSON.stringify({ type: "prompt", message: "/rehome --fork" }) + "\n"), 300);
    }
    if (m.type === "extension_ui_request" && m.method === "notify" && String(m.message ?? "").includes("Rehomed to")) {
      setTimeout(() => {
        console.log(readFileSync("/tmp/rehome-test/cwd-probe.log", "utf8"));
        proc.kill("SIGTERM");
        process.exit(0);
      }, 800);
    }
  }
});
proc.stderr.on("data", (d) => process.stderr.write(`[pi-stderr] ${d}`));
setTimeout(() => { console.error("timeout"); proc.kill(); process.exit(1); }, 45000);
