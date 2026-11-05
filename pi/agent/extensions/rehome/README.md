# rehome

Change a live pi session's embedded working directory — "rehoming" it to a
new cwd — without leaving the client.

## Why

pi bakes the cwd it was started from into the session header, the built-in
tool definitions, the system prompt, the resource discovery (skills,
AGENTS.md), and every extension's `ctx.cwd`. There is no API to change it in
a live session. If you resume a session from a different directory than the
one it was started in, the session stays tied to its original directory
forever: tools run there, prompts point there, and the session file keeps
living under the original directory's bucket in
`~/.pi/agent/sessions/--<workdir>--/`, invisible to a native `--continue` /
`/resume` from where you actually are.

## What rehoming means

After a rehome to a new cwd:

- all new tool calls run relative to the new cwd (the session runtime —
  tools, system prompt, resource discovery, project trust — is fully rebuilt
  against it),
- the new cwd is embedded in session state *as if it were the original*:
  the session file's header carries it, `ctx.sessionManager.getCwd()`,
  `ctx.cwd`, and `process.cwd()` all report it, for every extension,
- the session file moves into the new cwd's session directory, so native
  `pi --continue` / `/resume` from that directory find it,
- **no history is rewritten**: the first system message keeps its original
  `cwd` prompt section, all entries are copied verbatim, and the header gets
  a `parentSession` backlink to the source file,
- **the original session file is left untouched** — it was never rehomed,
  the fork was. Only the rehomed file gains a marker entry.

### History traceability

The cwd every tool call ran against stays derivable from the file alone:

- entries before the rehome ran under the original cwd, recorded in the
  (copied) first system message's `cwd` prompt section;
- pi persists a system patch message with the new `cwd` section before the
  next model request after the rehome (its normal prompt-section diffing),
  so replaying system messages yields the exact boundary — no rehome-specific
  knowledge needed;
- rehome additionally appends a visible marker to the rehomed session
  (`custom_message`, `customType: "rehome"`, with machine-readable
  `details: {from, to, mode, sourceFile}`) so the boundary is explicit to
  the model, the transcript UI, exports, and rehome-aware tooling.

## Usage

### `/rehome [path] [--fork|--move]`

Rehome the current session. With no path argument, an interactive directory
picker opens (type to fuzzy-filter, arrows to navigate, Enter/→ to descend,
←/Backspace to go up, `./` to pick the shown directory, Esc to cancel);
outside TUI mode it falls back to the process's current directory. Path
arguments get native autocompletion for directories (and the flags), and a
leading `~` is expanded. Without a mode flag you are prompted:

- **Fork**: copy the session to the new cwd, keep the old file (it stays
  listed under the old workdir as an archive),
- **Move**: copy the session to the new cwd, delete the old file after the
  switch succeeds,
- **Cancel**.

### Startup prompt

If a session is opened (`--continue`, `--resume`, `--session`) from a
directory different from the one embedded in it, rehome asks at startup
whether to rehome it to the current directory (fork/move) or keep it tied to
its original cwd (pi's native behavior; the default, first option).

### `/resume` intercept

Picking a session from a different cwd in `/resume` offers the same
fork/move/keep choice *before* switching. Keep (default) reproduces pi's
native behavior exactly: the session opens still tied to its original cwd.

Sessions whose embedded cwd no longer *exists* are left to pi's own
missing-cwd prompt in all cases.

## How it works

Session replacement rebuilds the whole runtime from the replacement file's
header cwd — that is the door this extension walks through:

1. `SessionManager.forkFrom(file, newCwd)` — writes a new session file in the
   new cwd's session dir with a rewritten header (`cwd`, new id,
   `parentSession` backlink) and all entries copied verbatim,
2. `process.chdir(newCwd)` so anything reading the process cwd follows,
3. `ctx.switchSession(forkedFile)` — the same in-place mechanism `/resume`
   uses; no exit/re-enter,
4. append the marker message to the rehomed session. If the switch is
   cancelled, the orphaned fork copy is removed again.

The `/rehome` command gets `switchSession` from its command context. The
startup prompt and `/resume` intercept run in *event* handlers, which pi
deliberately denies session-switching powers to. For those, rehome captures
pi's own command-context actions by wrapping
`ExtensionRunner.prototype.bindCommandContext` (extensions resolve
`@earendil-works/pi-coding-agent` to the same module instance pi runs on)
and invokes the captured `switchSession` in a macrotask after the handler
returns — calling it inside the handler could deadlock the runtime. If a
future pi changes those internals and capture fails, both flows degrade
gracefully to a notification plus a pre-filled `/rehome` in the editor.

## Limitations

- Rehoming requires a persisted session (no `--no-session`).
- The rehomed file gets a new session id (that is how `forkFrom` works); the
  old id remains reachable via the kept old file (fork mode) or not at all
  (move mode). The header's `parentSession` points back to the source file.
- `/new` after a rehome still roots at the directory pi was *launched* from
  (pi captures that internally at startup; unrelated to rehome).
- With a custom `sessionDir` setting, the target session directory is
  derived from the current session's directory layout when it matches the
  per-cwd encoding, and falls back to the default location otherwise.
- Tested against pi 1.2.0; written against the 1.4.0 extension API surface
  (identical for everything used here).

## Tests

`test/drive.mjs` is a self-contained integration suite that drives
`pi --mode rpc` over stdin/stdout (no TTY needed) against fixture sessions in
`/tmp/rehome-test`, exercising the startup prompt, `/rehome` (flag, select,
no-op), and the `/resume` intercept, asserting on the resulting session
files. `test/cwd-probe-drive.mjs` verifies other extensions observe the new
`ctx.cwd` and `process.cwd()` after a rehome.

```sh
node test/drive.mjs
node test/cwd-probe-drive.mjs
node test/dir-picker.test.mjs
```

Both use an isolated `PI_CODING_AGENT_DIR`/`PI_CODING_AGENT_SESSION_DIR`
under `/tmp` and never touch your real sessions.
