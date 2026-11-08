---
name: roadmap-journal
description: "Journal this session's roadmap work out of the main context, via the roadmap-journal subagent. Invoke ONLY when the roadmap extension's journal prompt (customType: roadmap-journal) instructs it — never auto-invoke; SKIP in every other circumstance. When instructed: decide whether the session worked within the roadmap, and if it did, spawn the roadmap-journal subagent with a brief of the work — never journal inline. Relay its one-line result and stop."
---

# /skill:roadmap-journal

The roadmap extension prompted this run: a live roadmap exists under
ai/roadmaps/, this session engaged with one, and code was written
after the session's last roadmap write.

One judgment is yours, here in the main loop, because you hold the
conversation: was the session actively working within that roadmap —
executing or planning its stages? If no — it was read in passing and
the work went elsewhere — answer "no entry" and stop. Do not
construct a connection, and do not spend a subagent on one.

If yes, delegate the writing. Spawn the `roadmap-journal` subagent
(the Agent tool, `subagent_type: "roadmap-journal"`, in the
foreground) with a prompt carrying:

- the active roadmap's path;
- a compact brief of what the session did within that roadmap:
  discoveries, decisions with the alternatives discarded, deviations
  from the Plan and why, stage landings. The subagent does not see
  this conversation — the brief is all it knows, so make it complete,
  and leave out everything that does not belong in a Journal.

The subagent appends ONE dated entry — or reports "already captured"
or "no entry" — and returns one line. Relay that line and stop
without further commentary.
