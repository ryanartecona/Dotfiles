/**
 * roadmap — two hooks supporting the roadmap skills' workflow.
 *
 * Hook 1 (lint-on-write). Every `edit`/`write` to a file matching
 * `*ai/roadmaps/*.md` is measured by `roadmap-lint --json`, scoped to
 * the lines the write touched (parsed from the edit tool's unified
 * patch; a write or an unparseable patch means the whole file). The
 * linter's block reason is appended to the tool result so the model
 * fixes what it wrote; the write stays on disk and is not an error.
 *
 * Hook 2 (journal-on-settle). When the agent settles, prompt once for
 * the roadmap-journal skill if all of these hold:
 *   1. outcome is "completed" (not an abort/error), and this is not
 *      our own continuation — guaranteed by condition 3 plus the
 *      promptedSeq watermark;
 *   2. engaged — the session touched a path under ai/roadmaps/ with any
 *      tool, read a roadmap SKILL.md, or the user typed /skill:roadmap-*;
 *   3. ordering — the last code write (edit/write outside ai/roadmaps/)
 *      came after the last roadmap write, and after the write count
 *      recorded when we last prompted;
 *   4. a live roadmap exists — any *.md at depth 1 in ai/roadmaps/.
 *
 * State is a handful of counters fed by tool_result events, rebuilt
 * from the session branch on session_start so /reload and /resume keep
 * the ordering and the watermark (the journal prompt itself is a
 * custom_message entry in the branch, so no separate marker file is
 * needed). Nested tool calls (subagents, codemode) are ignored: only
 * top-level edit/write calls move the counters.
 *
 * The prompt is a trigger, not a briefing — "Run the
 * /skill:roadmap-journal skill." The skill judges in the main loop
 * whether journaling is owed and delegates the write to the
 * roadmap-journal subagent, so an unwarranted prompt costs one reply
 * and a warranted one costs one Agent call plus a one-line result.
 * The skill stays model-invocable on purpose: a skill hidden via
 * disable-model-invocation is not advertised to the model, and the
 * injected prompt names the skill, not its file — the description
 * carries the "invoke only when instructed" discipline instead.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
  ToolResultEvent,
} from "@earendil-works/pi-coding-agent";
import type { AssistantMessage } from "@earendil-works/pi-ai";
import { execFile } from "node:child_process";
import { existsSync, readdirSync, realpathSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

/** A write aimed at a roadmap, mirroring a `*ai/roadmaps/*.md` glob. */
const ROADMAP_FILE_RE = /(^|\/)ai\/roadmaps\/.*\.md$/;
/** Anything touching the roadmaps dir, for engagement. */
const ROADMAP_DIR_RE = /ai\/roadmaps\//;
/** A read of one of the four skills. */
const SKILL_FILE_RE = /roadmap-(start|stage|journal|close)\/SKILL\.md$/;
/** The user forcing a roadmap skill. */
const SKILL_COMMAND_RE = /^\s*\/skill:roadmap-(start|stage|journal|close)\b/;

const JOURNAL_CUSTOM_TYPE = "roadmap-journal";
const JOURNAL_PROMPT = "Run the /skill:roadmap-journal skill.";
const LINT_TIMEOUT_MS = 15_000;

function lintScriptPath(): string {
  // The copy in the dotfiles repo's pi/bin, reached through the real
  // path of this file (home-manager deploys ~/.pi/agent/extensions as
  // out-of-store symlinks, so resolve the symlink first). Fall back to
  // PATH for other layouts.
  try {
    const here = path.dirname(realpathSync(fileURLToPath(import.meta.url)));
    const bundled = path.join(here, "..", "..", "..", "bin", "roadmap-lint");
    if (existsSync(bundled)) return bundled;
  } catch {
    // fall through to PATH
  }
  return "roadmap-lint";
}

const LINT = lintScriptPath();

/**
 * Semicolon-separated "start end" line ranges this write produced, from
 * the edit tool's unified patch: each `@@ -a,b +c,d @@` gives c to
 * c+d-1, with an absent d meaning 1 and a d of 0 counting as 1 (as the
 * original hook does). Null when nothing parses — scoping narrows the
 * report, never the gate, so the caller lints the whole file then.
 */
function touchedRangesFromPatch(patch: string): string | null {
  const ranges: string[] = [];
  const re = /^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@/gm;
  let m: RegExpExecArray | null;
  while ((m = re.exec(patch)) !== null) {
    const start = Number.parseInt(m[1], 10);
    let len = m[2] === undefined ? 1 : Number.parseInt(m[2], 10);
    if (len === 0) len = 1;
    ranges.push(`${start} ${start + len - 1}`);
  }
  return ranges.length > 0 ? ranges.join(";") : null;
}

/** The linter's block reason, or null when the write is clean or the
 * lint could not run (fail open — a missing linter must not wedge pi). */
function runLint(file: string, touched: string | null): Promise<{ reason: string | null; error: string | null }> {
  const args = ["--json", ...(touched ? ["--range", touched] : ["--all"]), file];
  return new Promise((resolve) => {
    execFile("bash", [LINT, ...args], { timeout: LINT_TIMEOUT_MS }, (err, stdout, stderr) => {
      const out = stdout.trim();
      if (out) {
        try {
          const parsed = JSON.parse(out) as { reason?: unknown };
          if (typeof parsed.reason === "string") return resolve({ reason: parsed.reason, error: null });
        } catch {
          // not the JSON we expected; treat as a lint problem
        }
      }
      if (err) return resolve({ reason: null, error: (stderr || String(err)).trim() });
      resolve({ reason: null, error: null });
    });
  });
}

export default function (pi: ExtensionAPI) {
  // Write-ordering state. writeSeq counts successful edit/write results;
  // lastCode/lastRoadmap are the seq of the most recent write of each
  // kind; promptedSeq is writeSeq at the moment we last asked for a
  // journal. Prompt when lastCode is ahead of both.
  let writeSeq = 0;
  let lastCode = 0;
  let lastRoadmap = 0;
  let promptedSeq = 0;
  let engaged = false;
  let lintProblemNotified = false;

  function noteEngagementPayload(payload: unknown): void {
    if (engaged) return;
    try {
      const s = JSON.stringify(payload);
      if (ROADMAP_DIR_RE.test(s) || SKILL_FILE_RE.test(s)) engaged = true;
    } catch {
      // unserializable input: ignore
    }
  }

  function noteWrite(filePath: unknown): void {
    if (typeof filePath !== "string" || filePath === "") return;
    writeSeq++;
    if (ROADMAP_DIR_RE.test(filePath)) lastRoadmap = writeSeq;
    else lastCode = writeSeq;
  }

  function hasLiveRoadmap(cwd: string): boolean {
    try {
      return readdirSync(path.join(cwd, "ai", "roadmaps"), { withFileTypes: true }).some(
        (e) => e.isFile() && e.name.endsWith(".md"),
      );
    } catch {
      return false;
    }
  }

  function notifyLintProblem(ctx: ExtensionContext, problem: string): void {
    if (lintProblemNotified) return;
    lintProblemNotified = true;
    if (ctx.hasUI) {
      ctx.ui.notify(`roadmap: lint-on-write disabled this session — ${problem.split("\n")[0]}`, "warning");
    }
  }

  // Rebuild the counters from the current branch (covers /reload,
  // /resume, and session switches). The journal prompt is itself an
  // entry in the branch, so the watermark survives too.
  function rebuildFromBranch(ctx: ExtensionContext): void {
    writeSeq = 0;
    lastCode = 0;
    lastRoadmap = 0;
    promptedSeq = 0;
    engaged = false;
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type === "message") {
        const msg = entry.message as { role?: string; content?: unknown };
        if (msg.role === "assistant") {
          for (const block of (msg as AssistantMessage).content) {
            if (block.type !== "toolCall") continue;
            noteEngagementPayload(block.arguments);
            if (block.name === "edit" || block.name === "write") {
              noteWrite((block.arguments as { path?: unknown }).path);
            }
          }
        } else if (msg.role === "user") {
          const text =
            typeof msg.content === "string"
              ? msg.content
              : Array.isArray(msg.content)
                ? (msg.content as { type?: string; text?: string }[])
                    .filter((b) => b.type === "text")
                    .map((b) => b.text ?? "")
                    .join("\n")
                : "";
          if (SKILL_COMMAND_RE.test(text)) engaged = true;
        }
      } else if (entry.type === "custom_message" && entry.customType === JOURNAL_CUSTOM_TYPE) {
        promptedSeq = writeSeq;
      }
    }
  }

  pi.on("session_start", (_event, ctx) => {
    rebuildFromBranch(ctx);
  });

  pi.on("input", (event) => {
    if (!engaged && SKILL_COMMAND_RE.test(event.text)) engaged = true;
  });

  pi.on("tool_result", async (event: ToolResultEvent, ctx) => {
    if (event.parentToolCallId) return;
    noteEngagementPayload(event.input);
    if (event.toolName !== "edit" && event.toolName !== "write") return;
    if (!event.isError) noteWrite(event.input.path);

    // Hook 1: lint the write, scoped to the lines it touched.
    const p = event.input.path;
    if (typeof p !== "string" || p === "") return;
    const abs = path.isAbsolute(p) ? p : path.join(ctx.cwd, p);
    if (!ROADMAP_FILE_RE.test(abs) || !existsSync(abs)) return;

    let touched: string | null = null;
    if (event.toolName === "edit") {
      const patch = (event.details as { patch?: string } | undefined)?.patch;
      if (patch) touched = touchedRangesFromPatch(patch);
    }
    const { reason, error } = await runLint(abs, touched);
    if (error) {
      notifyLintProblem(ctx, error);
      return;
    }
    if (!reason) return;
    return { content: [...event.content, { type: "text" as const, text: reason }] };
  });

  pi.on("agent_before_settle", (event, ctx) => {
    if (event.outcome !== "completed") return;
    if (!engaged) return;
    if (lastCode === 0 || lastCode <= lastRoadmap || lastCode <= promptedSeq) return;
    if (event.entries.some((e) => e.type === "custom_message" && e.customType === JOURNAL_CUSTOM_TYPE)) return;
    if (!hasLiveRoadmap(ctx.cwd)) return;
    promptedSeq = writeSeq;
    return {
      entries: [
        {
          type: "custom_message" as const,
          customType: JOURNAL_CUSTOM_TYPE,
          content: JOURNAL_PROMPT,
          display: true,
        },
      ],
      continue: true,
    };
  });
}
