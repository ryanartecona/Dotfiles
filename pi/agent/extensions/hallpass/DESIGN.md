# hallpass — design doc

A pi extension that puts every `bash` tool call behind a user-defined,
switchable **profile**: coarse-grain OS sandboxing (filesystem read/write
scoping, network policy incl. domain allowlists) via Anthropic's
sandbox-runtime (`srt`), plus — in later chunks — unification with tmux-mcp's
fine-grain command policy, tool selection, and per-subagent profile
assignment.

Status: **chunk 1 complete and verified** (2026-10-18). Core sandboxing
live: profiles, bash override with per-call srt wrapping, file-tool guard,
branch-persisted profile state. `tsc --noEmit` clean; behavioral smoke
tests pass (dev/offline/allowlist; fs EPERM + proxy 403); initial `pi -p`
load test passes. Interactive UX pass added `/hp` dashboard
(status / switch / reload) plus direct shortcuts `/hp-status` (plain
summary), `/hp-profile [name]` (picker) and `/hp-reload`; simplified after
review (dropped redundant `/hp-profiles` browser). `tsc --noEmit` was clean
before the simplification and has not been re-run since.
 A later print-mode startup/registry check was blocked
by inability to write `~/.pi/agent/settings.json.lock` and missing model
API key, so command registration is verified by typecheck but not by that
runtime check. Known srt 0.0.80 quirks documented in §4. Next: chunk 2
(violation-annotation UX, user_bash polish), chunk 3 (subagents).

Chunk-1 deployment notes:
- `~/.pi/agent/hallpass.toml` is a manual symlink to the dotfiles repo —
  NOT home-manager-managed yet; a rebuild may drop it.
- macOS: use `/private/tmp` in allowWrite, not `/tmp` (srt 0.0.80 keeps
  the bare symlink unresolved and seatbelt never matches it).
- npm audit flags 2 high-severity advisories in srt 0.0.80's dep tree
  (unreviewed; beta dep pinned deliberately).

---

## 1. Goals / non-goals

### Goals

- Profiles defined and owned by the user, switchable mid-session,
  effective on the *next* tool call (no need to alter in-flight tools).
- Coarse-grain enforcement per bash call: fs read scoping, fs write
  scoping, network off, network domain allowlists.
- Profile state stored in the session sequence (survives resume, follows
  branch navigation), fully owned by hallpass (no `settings.json`
  involvement at runtime).
- Explicit per-tool-call capabilities: each bash call is wrapped with the
  profile config current at exec time. No long-lived external daemons; the
  only shared state is an in-process proxy (see §4).
- Applies to the main session and to subagent sessions.
- Profile schema carries slots for future unification: tmux-mcp fine-grain
  profile reference, tool selection, per-subagent assignment.

### Non-goals (for now)

- Fine-grain command/argument policy (tmux-mcp already does this;
  unification is chunk 4).
- Windows support. macOS primary, Linux portable via srt's bubblewrap path.
- Sandboxing `pi` itself or whole-process isolation (see pi docs
  `containerization.md` for that tier).
- Sandboxing user-typed `!`/`!!` commands by default (per-profile opt-in,
  see §5).

---

## 2. Key research findings (why the design looks like this)

### Sandboxing layer: `@anthropic-ai/sandbox-runtime` (srt)

Chosen over raw SBPL codegen and over Microsoft MXC.

- **Per-call API**: `SandboxManager.wrapWithSandbox(command, binShell?,
  customConfig?, abortSignal?, {commandId, commandText?}) → Promise<string>`.
  A per-call config override exists; wrapping is pure string-building (plus
  a ripgrep glob walk on Linux); caller spawns the result. This is exactly
  "explicit capabilities passed per tool call".
- macOS: generates SBPL per wrap, emits `env … /usr/bin/sandbox-exec -p
  <profile> <shell> -c <command>`. Linux: bubblewrap with `--unshare-net`,
  host proxies reached via bind-mounted unix sockets + socat.
- **Network**: in-process HTTP+SOCKS5 mux proxy on an ephemeral port,
  started in `initialize()`, running even for fully-offline profiles (so
  `updateConfig` can open access later). Domain filtering is
  CONNECT/SNI-based — **no MITM by default** (opt-in via
  `network.tlsTerminate`). Proxy env vars (`HTTP_PROXY`, `HTTPS_PROXY`,
  `ALL_PROXY`, `GIT_SSH_COMMAND`, trust-store vars) are injected into the
  wrapped command automatically.
- **Dynamic updates**: `updateConfig()` is atomic and live-swaps network
  allowlists on the running proxy (per-request re-read, no restart).
  Filesystem config is baked at wrap time — fine, because we wrap per call.
- **Per-invocation network grants**:
  `registerCommandNetworkLists(commandId, {allowedDomains})` /
  `unregisterCommandNetworkLists(commandId)` — allowlists keyed by an
  unguessable ≥128-bit commandId passed via proxy username. We map
  `commandId` = pi tool-call id.
- **Violation UX**: `SandboxViolationStore` (in-process) +
  `annotateStderrWithSandboxFailures(commandId, stderr)` translates opaque
  `EPERM` into readable denials. Optional `log stream` monitor via
  `enableLogMonitor` — keep off by default.
- **Caveats**: 0.0.x "Beta Research Preview", 2–4 releases/month, explicit
  API-churn warning → **pin the exact version** in `package.json`.
  ESM-only, Node ≥ 22.12. `SandboxManager` is a **module-level singleton**
  (one per process) — see §4 for the concurrency consequence.

### pi extension API (all confirmed against pi 1.3.0 docs/types)

- **Bash tool replacement is first-class**: the Gondolin example
  (`examples/extensions/gondolin`) replaces `bash` via
  `pi.registerTool({ ...createBashTool(cwd), execute(id, params, signal,
  onUpdate, ctx) { … } })`. Inside `execute` we have the tool-call `id`
  (→ srt `commandId`) and can build per-call `operations` /
  per-call srt configs.
- **`spawnHook` is too weak for us** (sync; only `{command, cwd, env}`; no
  tool-call id). Use the Gondolin-style `execute` wrapper instead.
- **`tool_call` event**: pre-execution for every tool (built-in and
  extension-registered), `event.input` mutable, `{block, reason}` to veto,
  `ctx.ui.confirm` available for interactive asks. Typed narrowing via
  `isToolCallEventType("bash", event)`.
- **File tools** (`read/write/edit/grep/find/ls`) never touch srt; their
  path scoping must come from a `tool_call` handler validating structured
  `event.input.path` — a second, separate enforcement surface.
- **`user_bash` event**: covers `!`/`!!`; returning `{operations}`
  redirects execution. Per-profile flag decides whether user shell is
  sandboxed.
- **Tool selection**: `pi.getActiveTools()` / `pi.setActiveTools()` —
  immediate, appends transcript delta.
- **Session-sequence state**: `pi.appendEntry()` (durable, excluded from
  model context); reconstruct on `session_start` from
  `ctx.sessionManager.getBranch()`. Branch-relative — tree navigation
  reverts profile state, which is the desired semantics.
- `session_start` carries only `reason` (`startup|reload|new|resume|fork`)
  — **no subagent identity** (see §6).

### pi-subagents integration seams (from source exploration)

Package: `@tintinweb/pi-subagents` at
`~/.pi/agent/npm/node_modules/@tintinweb/pi-subagents/`.

- Subagent sessions are **in-process SDK sessions**
  (`agent-runner.ts:1106` → `runInChildSessionContext(() =>
  createAgentSession(sessionOpts))`), each with a full
  `DefaultResourceLoader` — **hallpass loads fresh in every subagent
  session** and gets its own `ExtensionAPI`. Enforcement inside subagents
  needs no cooperation from pi-subagents.
- Agent frontmatter parser (`custom-agents.ts:185–216`) **drops unknown
  keys** — but agent-file discovery is trivially replicable
  (`~/.pi/agent/agents/` ← `.agents/agents/` ← `.pi/agents/` ← package
  dirs). Hallpass parses the same `.md` files itself for a `profile:` key.
- Lifecycle events on `pi.events`: `subagents:created {id, type, …}`,
  `subagents:started/completed/failed/steered/compacted`.
- Workflow-engine `agent()` calls go through the identical spawn path.
- **No subagent identity is propagated into the child session** (no env
  var, no metadata field). Usable channel: session name is set to
  `{agentName}#{agentId[:8]}`.
- Escape hatch: frontmatter `extensions: false` /
  `excludeExtensions: [hallpass]` keeps hallpass out of a child session →
  must be policed from the parent (see §6).

### tmux-mcp (fine-grain; chunk 4, but schema-shaped now)

- TOML schema already supports `[profiles.<name>]` with `+`/`-` deltas and
  `[[profiles.x.assert]]` tests; rules have `via: "bash"|"tmux"`.
- Evaluation exposed to non-Rust callers only via CLI: `tmux-mcp
  policy-eval --via bash --profile X --json <lines>`.
- Runtime profile selection is `$TMUX_MCP_PROFILE` — **process-global**,
  and subagents share the process → per-subagent fine-grain profiles need
  per-check `policy-eval --profile` calls, not env vars.

---

## 3. Profile schema

TOML, `~/.pi/agent/hallpass.toml` (user) + `.pi/hallpass.toml` (project,
layered after trust grant). Sketch:

```toml
default = "dev"

[profiles.dev]
description = "Normal development: cwd rw, home ro minus secrets, open network"
filesystem.allowWrite = ["$CWD", "/tmp"]
filesystem.denyWrite  = ["$CWD/.env"]
filesystem.denyRead   = ["~/.ssh", "~/.aws", "~/.gnupg"]
network.mode = "open"               # open | offline | allowlist
tools = ["+grep", "+find", "+ls"]   # pi tool-selection delta (chunk: tools)
sandboxUserBash = false             # wrap `!`/`!!` too?
tmuxProfile = "default"             # fine-grain ref (chunk 4)

[profiles.audit]
filesystem.allowWrite = []          # read-only
network.mode = "offline"
tools = ["-write", "-edit"]
sandboxUserBash = true

[profiles.web-research]
filesystem.allowWrite = ["$CWD"]
network.mode = "allowlist"
network.allowedDomains = ["github.com", "*.githubusercontent.com"]
```

Schema slots reserved from day one: `tools`, `tmuxProfile`,
`sandboxUserBash`, `network.allowedDomains/deniedDomains`. Hallpass config
also gets an `agentProfiles` map for subagent assignment fallback
(§6):

```toml
[agentProfiles]
"Explore" = "audit"
"*"       = "dev"        # default for any subagent
```

Variable expansion: `$CWD` (session cwd), `~`, env vars. Profile configs
compile to srt `SandboxRuntimeConfig` at wrap time (cheap; no caching
needed initially).

---

## 4. Architecture

```
┌─ pi process ─────────────────────────────────────────────────┐
│ hallpass extension (one instance per session, in-process)    │
│                                                              │
│  ProfileStore      — parsed hallpass.toml, current profile   │
│                      ref (in-memory atom), appendEntry log   │
│  BashToolOverride  — pi.registerTool({...createBashTool,     │
│                      execute}) wrapping each call via srt    │
│  FileToolGuard     — pi.on("tool_call") path checks for      │
│                      read/write/edit/grep/find/ls            │
│  UserBashGuard     — pi.on("user_bash") per-profile          │
│  Command/UI        — /hallpass switch|status|list,           │
│                      ctx.ui.setStatus indicator              │
│  SubagentSeams     — (chunk 3) frontmatter parsing,          │
│                      Agent-call interception, identity       │
│                                                              │
│  srt SandboxManager — MODULE-LEVEL SINGLETON, shared by      │
│  main session + all subagent sessions (they're in-process).  │
│  Proxy: in-process HTTP+SOCKS5 on ephemeral port.            │
└──────────────────────────────────────────────────────────────┘
```

### The singleton rule (critical design constraint)

Because subagent sessions share the process, there is **one**
`SandboxManager`. Verified against srt 0.0.80 (smoke tests, 2026-10-18):

- `initialize()` is one-shot and performs **no Zod validation**; the base
  config must carry **complete arrays** — the proxy filter iterates
  `config.network.*` per request and 500s everything on `undefined`.
- Base config = **restrictive floor** (`allowedDomains: []`, empty fs
  lists). Widening is per call:
  `registerCommandNetworkLists(randomId, {allowedDomains})` +
  `wrapWithSandbox(…, {commandId: randomId})`. The grant id must be random
  ≥128 bits — **pi tool-call ids do not qualify** (they're only used as
  violation-attribution labels).
- Per-call `customConfig.network` **never reaches the proxy filter** in
  0.0.80 — it only toggles whether restriction applies. So `toSrtConfig`
  compiles **filesystem only**; network is handled by sandbox.ts.
- `updateConfig()` is process-global — never used for per-session policy.
- Filesystem policy is exact per call (baked at wrap time).
- The proxy is in-process (ephemeral localhost port + unix-socket mux),
  dies with the pi process. Wrapped commands must be executed **async**
  (sync exec in the proxy-hosting process self-deadlocks).
- pi's jiti loader uses `moduleCache: false`: hallpass's own modules may
  reload per session, but srt (plain ESM in node_modules) is a true
  singleton. → all process-wide hallpass state lives on **globalThis**.

**Network "open" mode (chunk-1 limitation):** srt has no per-call
allow-all (bare `*` rejected in allow entries) and the ask callback
receives only `{host, port}` — no command identity. So "open" is decided
by an **ask callback reading a process-wide floor** that tracks the last
explicit `/hallpass switch` (initialized from the default profile at first
session start). Correct for single-session use; concurrent in-process
sessions on divergent network modes share the floor (fail-open toward
"open"). Revisit in chunk 3. Per-profile `network.deniedDomains` is parsed
but **not yet enforced** (per-call denies need global updateConfig or
callback support — chunk 2 decision).

### Per-bash-call flow (as implemented, chunk 1)

1. Model calls `bash`; pi dispatches hallpass's replacement `execute(id,
   params, signal, onUpdate)`.
2. Hallpass reads current profile ref → compiles fs-only srt config.
3. allowlist profile: `registerCommandNetworkLists(randomId, …)`.
4. `wrapped = await wrapWithSandbox(command, undefined, fsConfig, signal,
   {commandId: randomId ?? id})`.
5. Execute `wrapped` through the built-in local bash tool (streaming,
   truncation, timeout, PI_* env all preserved); `finally: cleanup()`
   revokes the grant.
6. open/offline: no grant; proxy + ask callback decide (floor).
7. (chunk 2) on non-zero exit, `annotateStderrWithSandboxFailures` to
   translate denials for the model.

### State / profile switching

- Current profile = in-memory ref per session, changed by `/hallpass
  switch <name>` (or future tools/skills).
- Every switch: `pi.appendEntry({ type: "hallpass:profile", name, … })`
  → durable in session JSONL, excluded from model context.
- `session_start`: replay branch via `ctx.sessionManager.getBranch()` to
  restore the latest profile entry; fall back to config `default`.
- Bash override reads the ref at call time → next tool call uses the new
  profile. `pi.setActiveTools()` applies tool-selection changes
  immediately.

---

## 5. Enforcement surfaces checklist

| Surface | Mechanism | Chunk |
|---|---|---|
| Model `bash` calls | bash tool replacement + srt per-call wrap | 1 |
| `read/write/edit/grep/find/ls` paths | `tool_call` input path validation | 1 |
| `!`/`!!` user shell | `user_bash` handler → custom operations | 2 (flag in schema now) |
| Network domain allowlists | srt proxy + per-call command lists | 2 |
| Violation UX | `annotateStderrWithSandboxFailures` | 2 |
| Subagent assignment + enforcement | §6 seams | 3 |
| tmux-mcp fine-grain unification | `via: "bash"` eval, `tmuxProfile` refs | 4 |

---

## 6. Subagent integration (no fork of pi-subagents)

Assignment **data** lives in each agent's frontmatter (`profile: <name>`);
assignment **logic** lives entirely in hallpass. Fallback/override map in
hallpass config (`[agentProfiles]`).

1. **Parse agent files independently**: replicate discovery
   (`~/.pi/agent/agents/`, `.agents/agents/`, `.pi/agents/`, package
   dirs), read frontmatter `profile:` ourselves (pi-subagents drops
   unknown keys, which is fine — we never ask it for them).
2. **Parent-side spawn observation/interception**: `pi.on("tool_call")`
   for the `Agent` tool (believed to fire for extension tools — **verify
   first thing in chunk 3**, see §8). Uses: record pending type→profile
   assignment; **block spawning agents whose frontmatter disables
   hallpass** (`extensions: false` / `excludeExtensions`) while a
   sandboxed profile is in force — closes the escape hatch as policy.
3. **Child-side activation**: hallpass loads fresh in each child session;
   at `session_start` it resolves its agent type (fragile seam: parse
   session name `{agentName}#{id8}`; fallbacks: parent-shared module-level
   registry via `subagents:created` correlation; final fallback:
   `[agentProfiles] "*"`), then applies that profile locally (same
   BashToolOverride/FileToolGuard path as the main session). No `/hallpass`
   switching UI in child sessions initially.
4. All identity resolution behind one function
   (`resolveSubagentProfile(ctx)`) so a future tiny upstream PR (env var /
   exported child-context) slots in without restructuring.

Estimated elegance: ~85%. The fragile 15% is child self-identification —
no fork needed unless that seam breaks.

---

## 7. Build chunks (pause for review after each)

1. **Core + macOS sandboxing**: package scaffold in
   `pi/agent/extensions/hallpass/` (ESM, pinned srt dep), profile schema
   (fs + network open/offline + reserved slots), ProfileStore with
   appendEntry persistence, bash replacement with per-call
   `wrapWithSandbox`, FileToolGuard path checks, `/hallpass
   list|switch|status`, status widget. No proxy/domains yet.
2. **Network policy**: srt proxy lifecycle, allowlist mode +
   `registerCommandNetworkLists` per call, violation annotation UX,
   `sandboxUserBash` handling.
3. **Subagents**: verify `tool_call` interception of Agent calls
   (empirical, first task), frontmatter `profile:` parsing, identity
   resolution, escape-hatch policing, child-session activation.
4. **Unification**: tmux-mcp `via: "bash"` evaluation per call
   (`policy-eval` CLI bridge), `tmuxProfile` refs, tool-selection
   profiles, dynamic capability grant UX (skills/tools that select
   profiles from task descriptions).

---

## 8. Risks / assumptions to verify early

- [ ] **`tool_call` fires for the pi-subagents `Agent` tool with mutable
      input + block** (core docs say yes for all tools; Explore agent
      claimed otherwise — 5-minute empirical test, first task of chunk 3).
- [x] ~~jiti module sharing~~ — RESOLVED: jiti uses `moduleCache: false`
      for extension modules; srt singleton is native-ESM-cached.
      Process-wide hallpass state lives on globalThis.
- [x] ~~srt base-config shape~~ — RESOLVED: complete arrays, restrictive
      floor; partial network object poisons the proxy (500s).
- [x] ~~per-call network policy mechanism~~ — RESOLVED:
      registerCommandNetworkLists + random commandId (verified 200/403);
      customConfig.network does not reach the filter in 0.0.80.
- [ ] jiti module caching actually shares module-level state across
      sessions in one process (fallback identity channel). If not, use
      `pi.events` bus or accept session-name parsing only.
- [x] ~~srt `initialize()` idempotence~~ — RESOLVED: one-shot, early
      return; second calls are silently ignored. Version pinned (0.0.80).
- [ ] srt proxy starts at initialize (not lazily) because the base config
      defines allowedDomains; init is deferred until first sandboxed wrap
      (hallpass calls ensureSandboxInitialized lazily per command).
- [ ] `session.getSessionName()` (or equivalent) is reachable from an
      extension inside a subagent session at `session_start`.
- [ ] SBPL/sandbox-exec behavior on current macOS for our rule shapes
      (dev-machine smoke tests during chunk 1).
- [ ] bash replacement composes with other installed extensions that
      touch bash (none known today; keep load-order note in README).

## References

- srt: github.com/anthropics/sandbox-runtime (`SandboxManager`,
  `wrapWithSandbox`, `registerCommandNetworkLists`,
  `annotateStderrWithSandboxFailures`)
- pi docs: `extensions.md` (tool_call, user_bash, appendEntry,
  setActiveTools), `security.md`, `containerization.md`; Gondolin example
  `examples/extensions/gondolin/index.ts`; types
  `dist/core/extensions/types.d.ts`, `dist/core/tools/bash.d.ts`
- pi-subagents: `src/agent-runner.ts` (spawn, ResourceLoader),
  `src/custom-agents.ts` (frontmatter parse, discovery),
  `src/child-context.ts` (AsyncLocalStorage marker)
- tmux-mcp: `src/policy/config/format.rs` (schema),
  `src/policy/mod.rs` (`evaluate_with`), CLI `policy-eval`
