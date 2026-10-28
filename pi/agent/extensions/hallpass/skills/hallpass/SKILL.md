---
name: hallpass
description: Understand or edit hallpass sandbox profiles and session overrides. Use when working with hallpass.toml, profile inheritance, cycleProfiles, agentUsage, filesystem/network/tool rules, hallpass_status, or hallpass_session_edit.
---

# Hallpass profiles and session policy

Hallpass applies a resolved profile to bash, pi's file tools, tool selection, and optionally tmux-mcp. Bash commands are wrapped per call with sandbox-runtime (srt); file tools use hallpass's separate path guard.

Use `hallpass_status` to inspect actual effective state before proposing changes. Do not infer policy from a profile name or try to bypass a denial.

## Persistent configuration

User config is `~/.pi/agent/hallpass.toml`; trusted project config is `.pi/hallpass.toml`. Project files load later. A same-named project profile replaces the raw user definition; use a new name with `extends` to build on a user profile.

```toml
default = "dev"
cycleProfiles = ["dev", "offline", "audit"]

[network]
# Process-global always-deny list. Project config can add, never remove, entries.
deniedDomains = ["169.254.169.254", "metadata.google.internal"]

[profiles.dev]
description = "Normal development"
agentUsage = "Keep changes inside the project. Do not access credential stores."
filesystem.allowWrite = ["$CWD", "/tmp"]
filesystem.denyRead = ["~/.aws", "~/.gnupg"]
network.mode = "open"
sandboxUserBash = false
tmuxProfile = "default"

[profiles.offline]
extends = "dev"
description = "Offline development"
network.mode = "offline"
filesystem.denyRead = ["+~/.ssh"]
sandboxUserBash = true

[profiles.audit]
extends = "offline"
description = "Read-only review"
agentUsage = "Explain findings; do not modify files."
filesystem.allowWrite = []
tools = ["-write", "-edit"]

[agentProfiles]
"Explore" = "audit"
"*" = "dev"
```

`cycleProfiles` affects only alt+n/alt+p. If omitted, every valid profile participates in declaration order. An empty list disables cycling. Profiles outside the list remain available to `/hp-profile`, `default`, and `[agentProfiles]`.

`agentUsage` is agent-facing guidance reflected into the system prompt and `hallpass_status` for the effective profile. It is configuration, not a substitute for enforcement.

## Inheritance and list semantics

A profile may name one parent with `extends = "parent"`. Multiple levels are allowed. Unknown parents, cycles, and invalid fields replace the affected profiles with a visible fail-closed lockdown (and exclude them from cycling) rather than partially applying them or falling back to a looser profile.

- Omitted fields inherit.
- Strings, booleans, and `network.mode` replace inherited scalar values.
- A raw list replaces the inherited list. `[]` therefore clears it.
- A list whose entries all start with `+` or `-` is a delta against the inherited list.
- Mixing signed and unsigned entries is invalid.
- Empty entries and bare `"+"`/`"-"` operands are invalid.
- Delta matching is exact and happens before path/environment expansion.

```toml
[profiles.child]
extends = "dev"
# Adds one write root and removes one inherited credential denial.
filesystem.allowWrite = ["+$CWD/dist"]
filesystem.denyRead = ["-~/.aws"]
# Replaces, rather than extends, the inherited domain list.
network.allowedDomains = ["github.com", "*.githubusercontent.com"]
```

`tools` is intentionally special because its entries operate on pi's active tool set rather than naming literal profile-list members:

- Entries remain signed operations such as `"-write"` or `"+grep"`.
- The last operation for a tool wins across inheritance, so child `"+write"` cancels inherited `"-write"`.
- `tools = []` clears inherited tool policy.
- Disabled tools are hidden in the main session and blocked on call in every session. Subagent visibility remains under pi-subagents' control.

## Profile fields

- `description`: human/model-facing summary.
- `agentUsage`: agent-facing usage instructions.
- `sandbox`: defaults to true. False leaves bash unsandboxed; the file-tool guard still applies.
- `sandboxUserBash`: defaults to false. When true, also sandbox user-typed `!`/`!!` commands.
- `filesystem.denyRead`, `allowRead`, `allowWrite`, `denyWrite`: string lists passed to srt and used by the file-tool guard. An `allowRead` entry can reallow a subpath of a `denyRead` root.
- `network.mode`: `"open"`, `"offline"`, or `"allowlist"`; root default is `"open"`.
- `network.allowedDomains`: domains used in allowlist mode. Changing this list alone does not change an inherited offline/open mode.
- `network.allowUnixSockets`: composable list of Unix socket paths allowed by srt. Path-based on macOS and ignored by srt on Linux. Empty by default. Hallpass expands paths and applies the list per bash wrap, including session overlays.
- `tools`: signed pi tool operations described above.
- `tmuxProfile`: one tmux-mcp profile name or a composable list of names. Omit to inherit; `[]` clears an inherited selection.

Paths support `~`, `$CWD`, and other `$ENV_VAR` references. Relative paths resolve against session cwd. Existing path ancestors have symlinks resolved. Glob paths are enforced for bash by srt but skipped by the JS file-tool guard, so prefer literal paths where both surfaces matter.

Per-profile/session `network.deniedDomains` is unsupported. Use only the top-level `[network]` list, which also applies in open mode.

## Ephemeral session overrides

Session settings are an initially-empty profile patch layered after the active configured profile. They use the same fields and list semantics, except `extends` is implicit and forbidden. They cannot change `default`, `cycleProfiles`, `[agentProfiles]`, or global denied domains.

The overlay is retained when switching profiles or running `/hp-reload`, so it applies to the newly active base profile. An invalid base's fail-closed lockdown ignores any retained overlay and rejects new session edits until the config is fixed; the overlay remains available if a valid profile is selected again. Overlays are never written to `hallpass.toml` or the session transcript and reset with a fresh session/extension runtime.

For manual edits, use `/hp-session` to open a TOML editor, `/hp-session <TOML>` for a direct fragment, or `/hp-session clear`. `/hp-status` shows both the raw overlay and the fully resolved result.

For agent-requested edits:

1. Call `hallpass_status`; preserve any existing overrides that should remain and note its session revision.
2. Build the **complete desired overlay**, not merely an incremental fragment.
3. Call `hallpass_session_edit` with that TOML and `expectedRevision`.
4. The user sees the effective-policy diff and must approve. In the TUI they can attach an optional note to either decision by typing while that option is selected; any approval note or rejection reason is returned in the tool result. Rejection, cancellation/abort, stale revisions, invalid TOML, and noninteractive sessions leave policy unchanged.
5. An empty TOML string clears the overlay, subject to the same approval.

Example overlay that adds implementation capabilities to a restrictive base:

```toml
filesystem.allowWrite = ["+$CWD"]
tools = ["+write", "+edit"]
network.mode = "allowlist"
network.allowedDomains = ["+github.com", "+*.githubusercontent.com"]
```

Because this is a complete overlay, include other existing session fields when they should survive. The configured base profile is unchanged.

## Operational cautions

- Filesystem policy is baked per bash call. Session edits affect the next call, not an already-running command.
- Network deny-list state is process-global. Do not attempt to make it profile- or session-specific.
- Unix socket grants can expose powerful host services. In particular, allowing Docker/Podman control sockets can amount to a host sandbox bypass; request only the specific socket required.
- `sandbox = false` is an explicit escape hatch, not a safe default for untrusted work.
- Environment/file credentials are separate concerns: a domain allowlist does not prevent reading credentials allowed by filesystem policy.
- Prefer narrow, task-specific grants. Do not request broad access merely because a narrower rule would require more thought.
