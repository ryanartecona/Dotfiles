---
name: roadmap-start
description: "Author or revise a staged implementation plan (roadmap) in ai/roadmaps/ by climbing three resolutions — Shape (goal, findings, a short STE intent per stage), then Contract (Dies/Born per stage), then Workspace (Assumes, tasks, Done when) — converging with the user at each rung. Auto-invoke when the user asks to plan a feature/refactor/redesign as a roadmap (/skill:roadmap-start, \"roadmap this\", \"plan X as a roadmap\"), gives design feedback on a draft or live roadmap, or when a roadmap stage delegates to a child. SKIP work small enough to just do, and executing an already-ratified stage (that is /skill:roadmap-stage). AUTHORING PRODUCES A SHAPE DOCUMENT UNDER 100 LINES AND STOPS: no code blocks, no Dies/Born, no Assumes/tasks/Done-when. Later rungs are entered only on the user's explicit word, one stage at a time, and far stages stay at Shape until /skill:roadmap-stage deepens them. Execution begins only after ratification, which registers the roadmap as a live line in ai/ledger.md."
---

# /skill:roadmap-start

Author or revise a roadmap (`ai/roadmaps/<id>.md`), climbing one
resolution at a time and converging with the user at each rung.

**Read `references/roadmap-conventions.md`, bundled beside this
SKILL.md, first** — the format spec, cited below as "the conventions". `§1`/`§2`/`§3` cite `agents.md`.

## Invariant

The draft roadmap is the ONLY artifact — no side reports, no scratch
files. It lives at `ai/roadmaps/<id>.md` from the first pass (create
the directory if absent — that opts the repo in). Every pass rewrites
the doc and appends a Journal entry: what changed and why. Between any
two passes the doc is a valid roadmap; stop and resume anywhere.

## The hard stop

**"Roadmap it" produces a Shape document — under 100 lines — and
stops.** Rung 2 is the user's call. Under 100 lines, no code block, no
`---`, no Dies/Born/Named/Assumes/tasks/Done-when.

## Rung 1 — Shape

Goal, the findings that make it the right goal, a short intent per
stage, `Open questions` — all but the questions in the conventions' STE
form, with `*Concept*` cites and code identifiers as the nouns. Iterate
here until the user says the shape is right. Size
the effort — a small feature runs these inline; an arc fans out
(subagents for survey and skeptics for the attack, where the harness
has them).

1. **Survey.** Read the code before designing. (A goal too ambiguous
   to know what to read → ask first.) Record every touched interface's
   verdict — exactly one of the conventions' two: clean fit (justify
   it) or concept review. For an abstraction roadmap, inventory every
   user: usage pattern plus scar tissue.
2. **Find the thesis.** The goal says what we want; the findings say
   why this is the right shape to want it — the conceptual verdict,
   what it deletes, what it makes cheap, what property replaces
   eyeballing. Goal + stages with no findings is not a plan, only a
   decomposition. Record what was dropped and where it was routed.
3. **Interview.** Ask what the code cannot answer and the draft would
   otherwise guess: open design choices, scope, appetite, and any
   failure scenario whose desired outcome is a product call ("what
   should happen when X fails?"). Grounded in what survey found —
   never generic, never confirming derivable defaults. Journal each
   answer; `harvest:` where load-bearing.
4. **Stage it.** Numbered stages in dependency order, each a heading
   and **a short intent — one to three STE paragraphs, within the
   conventions' ` — shape` bar** — what changes conceptually, not how,
   held to the conventions' cold-read test: heading and intent together
   let a reader who knows the code and the goal say what changes, with
   you out of the room. The name is a vocabulary word plus what happens
   to it, or a plain sentence of what becomes true; the intent names
   what the stage makes exist and stop existing, in `*Concept*` cites
   and identifiers, and the difference a user or test observes. A name
   you would have to explain is a title; a sentence that fits another
   stage is empty. Ordering: prepare → land → contract; replacement
   demolishes first. Write the far stages too, and mark them ` — shape`.
5. **Simplify.** Attack the plan the way §1 attacks code: hunt the
   special cases a model change dissolves; front-load interface
   cleanup ("make the change easy, then make the easy change");
   confirm what the roadmap obsoletes dies in the demolish stage; no
   interface work in a land stage (the feature lands as the n-th
   user).
6. **Attack the thesis.** Is the goal the real problem? Does the
   verdict name a concept that dies, or an addition wearing a verb?
   What would have to be true for this to be wrong? Surviving doubt
   goes to `Open questions` with its resolution route. A load-bearing
   structural call is contested before it is ratified: generate its
   alternatives independently (a skeptic subagent is the cheap way)
   and put the choice to the user — never ratify on a single
   unreplicated line of reasoning.
7. **Present and STOP.** First reread the goal, the findings and every
   heading and intent as the cold reader; rewrite any you would have
   to explain, and any sentence that breaks the conventions' STE rule
   or names a concept in words `ai/vocab.md` does not use.

## Rung 2 — Contract

On the user's word, and only with `Open questions` empty. Per stage in
order: the `---` appears, the intent loses every sentence Dies/Born now
state (still STE, still cold-readable), Dies, Born and Named go above
it.

**Born and Named are exhaustive and exact.** At execution time a shape
or concept they missed — or one they name that goes unused — costs a
full revert and restart of the stage (`/skill:roadmap-stage`). This rung is
where that accuracy is bought; a stage you cannot yet specify to that
standard stays at ` — shape` instead.

1. **Dies first** — the empty case must be argued, and that claim is
   ratified.
2. **Born from the consumer side.** Write the contract-test body — the
   ideal calling code — *before* settling the signature; judge it
   against §3's four checks. A failure means Born is wrong; rewrite it
   here. It survives as the stage's contract-test task at rung 3.
3. **Criteria.** Each Born against §2's six, then §2's two inverses.
4. A Born delta over ~a screen: split the stage, or delegate it to a
   child roadmap when it is a sub-arc in its own right.
5. **Named last, and reluctantly.** Read `ai/vocab.md` first — the
   right entry is usually already there, and reuse is the win.
   Reluctance means reaching harder for existing vocabulary, never
   omitting a concept the stage will need: coin exactly what its
   comments and docs must say and cannot say in existing words. Fill
   *Rejected synonyms* from the naming argument as it happens; the
   losing candidates are recoverable nowhere else.

Present each stage's contract as a diff and converge before the next.
Names get their own beat in that conversation: the user picks the word,
and a word they don't like is changed here for free and nowhere else
for free.
A stage the user won't settle keeps ` — shape` and waits.

## Rung 3 — Workspace, then begin

Fill the workspace only where it can be planned against real code —
usually the next stage or two. The rest keep ` — contract`;
`/skill:roadmap-stage` deepens each when it arrives. Per filled stage:

- **Assumes** — its priors, verified against the code now, any
  `file:line` re-derived fresh. The replan tripwire.
- **Coverage** — happy path plus failure/edge scenarios (concurrency,
  bad input, unexpected states); each becomes a named test task or a
  Done-when check. One whose outcome only the user can pick goes back
  to interview. Name the failing tests to write first; list what the
  harness needs; add dependency-contract tests at third-party seams.
- **Done when** — the conventions' three compile/grep checks, plus
  behavior checks that cannot lie (a test, a command).
- **Pressure-test** — break its Assumes; find coupling survey missed.
  A measurable unknown gets a measurement checkpoint with pre-stated
  decision rules and an abort path.

Then journal the birth and register the live line in `ai/ledger.md`
(create it if absent). Execution begins — `/skill:roadmap-stage`.

## Convergence — feedback at any rung

Feedback re-enters at the rung it lands on, not the top. "Stage 2's
Born is wrong" is Contract; "wrong stages" is Shape; "that's not the
problem" is Shape's thesis.

1. Discuss until an agreed shape exists — a design conversation, not a
   workflow re-run.
2. Rewrite that rung. **Everything below it is discarded, not
   patched.** A stage whose intent moved loses its Dies/Born and drops
   to ` — shape`.
3. **Reconcile** every downstream stage whose Assumes or Dies/Born
   referenced the old shape; re-run the rung's checks on those only.
4. Journal the revision: the disagreement, the agreement, what
   rippled; mark user-declared choices for harvest.
5. Re-present as a diff of the changed portions only. Untouched
   ratified material stays ratified.

The same loop serves execution-time replans, entered via
`/skill:roadmap-stage`.

## Delegation

Rewrite the parent stage's tasks to "plan and run `<id>`"; lineage as
prose in the child's goal. The child starts at rung 1. A
concept-review child is scoped to the abstraction and all its users.
