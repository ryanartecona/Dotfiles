# roadmap

## What it gives you

A staged-planning workflow for pi: any feature-set, refactor, or
redesign too big to just do gets planned as a **roadmap** — one file
under `ai/roadmaps/` that climbs three resolutions, each entered only
on your word:

- **Shape** — the goal, the findings that make it the right goal, and
  one short intent paragraph per stage, plus open questions. Under 100
  lines, then it stops.
- **Contract** — per stage, exactly what code **Dies**, what interfaces
  are **Born**, and what concepts are **Named** (added to the project
  vocabulary).
- **Workspace** — assumptions, tasks, and objective done-when checks
  (compiles, greps to zero, tests green) for the next stage or two.

Execution goes stage by stage: assumptions are re-verified against the
code as it now is, the contract is implemented as ratified, and a
contract that turns out wrong forces stop → revert → re-ratify →
restart rather than a patch-around. When a roadmap finishes (or is
abandoned), its vocabulary, decisions, and rationale are harvested
into `ai/vocab.md`, `ai/state.md`, and `ai/decisions.md` — durable,
git-tracked project memory — and the roadmap itself is gzipped into
`ai/roadmaps/archive/`.

A typical flow:

1. You: "roadmap X" → the agent surveys the code, interviews you, and
   writes a Shape document, then stops.
2. You iterate on the shape, then ratify Contract per stage, then
   Workspace for the near stages.
3. The agent executes stages one at a time, proving each done-when
   before marking it done.
4. A session that did roadmap work settles without journaling it →
   the extension prompts once, and the work is journaled by the
   `roadmap-journal` subagent (one Agent call, one line back).
5. Last stage done → close harvests the durable files, completes the
   ledger line in `ai/ledger.md`, and archives the roadmap.

Throughout, every write to a roadmap file is linted against the
resolution bars, scoped to the lines that write touched; violations
come back to the agent with the remedy, so oversized stages and
rung-breaking content are caught the moment they are written.

## What lives where

| Piece | Location | Deployed to |
|---|---|---|
| The two hooks | `pi/agent/extensions/roadmap/index.ts` | `~/.pi/agent/extensions/roadmap/` (home-manager symlink) |
| The four skills | `pi/agent/skills/roadmap-{start,stage,journal,close}/` | `~/.pi/agent/skills/` (home-manager symlinks) |
| The journal subagent | `pi/agent/agents/roadmap-journal.md` | `~/.pi/agent/agents/` (home-manager symlink) |
| The linter | `pi/bin/roadmap-lint` | `~/bin/roadmap-lint` (on PATH); the extension resolves it relative to its own real path, PATH is the fallback |

A new skill, agent, or extension directory is enumerated from the
flake source: `git add` + a home-manager rebuild. Edits inside
existing entries are live immediately (`/reload` in pi to re-read
skills).

## The two hooks

1. **lint-on-write** — every `edit`/`write` to `*ai/roadmaps/*.md`
   runs `roadmap-lint --json` scoped to the lines the write touched
   (parsed from the edit tool's unified patch; whole file for
   `write`). A finding's block reason is appended to the tool result;
   the write stays on disk and is not marked an error.
2. **journal-on-settle** — when a session that engaged with a roadmap
   writes code after its last roadmap write and settles, the extension
   injects one "Run the /skill:roadmap-journal skill." message and
   requests one continuation. Write-ordering counters are rebuilt from
   the session branch on `session_start`, so `/reload` and `/resume`
   keep the watermark; the prompt entry itself is the guard against
   re-prompting, and aborted runs never prompt.

## Design notes

- **Journaling runs in a subagent, not inline.** The settle prompt
  reaches the main loop, which alone can judge whether the session
  actually worked within the roadmap (answering "no entry" costs one
  reply, not a spawn). When journaling is owed, the main loop hands
  the `roadmap-journal` subagent a compact brief of the work — the
  subagent sees no conversation — and relays its one-line result. The
  main conversation spends one Agent call and one line on journaling.
- **`disable-model-invocation` is deliberately NOT set** on
  roadmap-journal. A hidden skill is not advertised to the model, so
  the injected prompt (which names the skill, not its file) would
  leave the model with no path to load. The description carries the
  "invoke only when instructed" discipline instead.
- **The settle prompt is a trigger, not a briefing**, by design: the
  standing instructions live in the skill description, which rides in
  the cached system prompt at no per-fire cost.

## Dependencies

- A subagent extension providing the `Agent` tool (developed against
  `@tintinweb/pi-subagents`, already in `settings.json` packages);
  without it, journal prompts degrade to the model reporting it cannot
  delegate.
- `roadmap-lint` needs `bash`, `awk`, `git`, `jq`, and coreutils on
  the PATH of the pi process (stock macOS awk is sufficient).
- The skills' conventions cite §1–§3 of a standards doc (role &
  philosophy, interface criteria, TDD workflow) as always-loaded
  `AGENTS.md`; without an equivalent AGENTS.md those citations have no
  definitions.

## Test

```sh
node test/harness.mjs   # 12 behavior checks against fake events; PI_JITI overrides the jiti path
```
