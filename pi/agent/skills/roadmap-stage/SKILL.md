---
name: roadmap-stage
description: "Stage-boundary checklist for the active roadmap in ai/roadmaps/. Auto-invoke BEFORE starting work on a roadmap stage (verify its Assumes against the actual code first — never code past a broken assumption) and WHEN a stage's Done-when is met (run the Done-when checks — every Dies name greps to zero — and mark it done; invoke /skill:roadmap-close after the last stage). SKIP when no roadmap in ai/roadmaps/ covers the current work, and for revising the plan itself (that is /skill:roadmap-start)."
---

# /skill:roadmap-stage

The stage-boundary checklist for the active roadmap. Format:
`references/roadmap-conventions.md`, bundled beside this SKILL.md —
read it first; cited here as "the conventions".

## Before starting a stage

1. **Deepen it.** A stage arrives at the resolution its heading
   declares (conventions): ` — shape` needs Dies/Born/Named,
   ` — contract` needs its
   workspace, unmarked needs neither. Climb it now, against the code
   as it IS — earlier landings have moved the ground the plan was
   written on, which is exactly why this detail was not written at
   authoring time. Follow `/skill:roadmap-start`'s rung 2 and rung 3, one
   stage only; present any new Dies/Born/Named as a re-ratification
   diff and wait. Drop the marker from the heading when it is whole.
2. Read the stage. Verify every Assumes claim against the actual
   code, re-deriving any `file:line` fresh.
3. All hold → begin, from a clean tree so the stage has a commit to
   revert to. The ratification portion is the contract: implement to
   Born, delete to Dies, say it in Named's words. Read `ai/vocab.md`
   before writing any comment or doc: concepts are cited as
   `*Concept*`, spelled as vocab spells them, and a synonym it already
   rejected is a defect. Comments cite concepts and
   decision titles, never this roadmap's id or stage numbers — except
   to tag scaffolding that dies with it (conventions: Referencing from code).
4. Any fails → never code past a broken assumption. Journal the
   violation, then choose by blast radius:
   - **Local:** rewrite the stage in place and reconcile downstream
     stages (`/skill:roadmap-start`, Convergence); present edits above the
     `---` to the user as a diff for re-ratification.
   - **Structural:** propose wind-down via /skill:roadmap-close plus a
     refactor roadmap. The user pops the stack — nothing resumes
     automatically.

## On a Born or Named violation

Born and Named are exhaustive and exact (the conventions). The moment
implementation needs a consumer-visible shape or a concept they do not
name, or one they do name proves unused, the contract is wrong — and a
wrong contract is not patched around:

1. **Stop.** Do not finish the file, do not add it and move on.
2. **Journal it first** — what was needed or unused, and why the
   contract missed it. The Journal is append-only and survives the
   revert; nothing else does.
3. **Revert the stage's work entirely**, back to the commit it started
   from. Every line written under the wrong shape was steered by it.
4. **Re-ratify.** Edit Born/Named and present the diff — an edit above
   the `---` is a re-ratification event. Wait for the user.
5. **Restart the stage from scratch** against the ratified contract.

Patching Born to match what you already built inverts the contract and
makes ratification a rubber stamp. If the missing shape keeps the stage
past its bar, split it before restarting.

## On meeting a stage's Done-when

1. Run the checks literally: Born shapes compile; every Dies name
   greps to zero hits — survivors block done regardless of what
   works; every Named concept greps to at least one `*Concept*`
   citation — a coined word with no users is vocabulary debt, so
   either use it or strike the bullet; behavior checks green.
2. Mark the stage done in its Plan heading (append ` — done`). If
   its body has gone stale, compress it to the heading + a one-line
   result — **but keep the Named bullets verbatim**; they are what
   /skill:roadmap-close harvests into `ai/vocab.md`, and compressing them
   away loses the vocabulary between here and the close.
3. Journal the landing: date, what actually landed, deviations from
   the plan.
4. Last stage done → invoke /skill:roadmap-close.
5. Otherwise verify the next stage's Assumes immediately — this
   landing may have changed the code they were written against.
