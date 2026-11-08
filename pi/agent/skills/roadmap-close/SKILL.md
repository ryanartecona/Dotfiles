---
name: roadmap-close
description: "Wind down a roadmap — the ONLY way a file leaves ai/roadmaps/. Auto-invoke when a roadmap's last stage is marked done, or when the user supersedes/abandons one mid-flight (their explicit go-ahead required). One commit: harvest the Journal, update the parent, route residue, complete the roadmap's live ledger line in place, gzip the file into ai/roadmaps/archive/."
---

# /skill:roadmap-close

Wind down a roadmap — the ONLY way a file leaves `ai/roadmaps/`.
Finished (all stages done) and superseded/abandoned take the same
path. Everything below lands in ONE commit. Format:
`references/roadmap-conventions.md`, bundled beside this SKILL.md
(read it first), cited here as the conventions.

## Procedure

1. **Confirm.** If any stage is not marked done, this is a
   supersession or abandonment — stop and get the user's explicit
   go-ahead. If a live child names this roadmap as parent, resolve
   it first: close it or re-parent it. Grep the repo (outside `ai/`)
   for this roadmap's id — the `<id>:` tag form precisely, then bare
   mentions: tagged scaffolding must be resolved and leaked
   coordinates rewritten to concept or decision names — zero hits
   before the file may be archived.
2. **Harvest the vocabulary** into `ai/vocab.md` (format: the
   conventions, "Durable memory"; create it on first harvest). This
   is a copy, not a judgment call — the ratification surface already
   decided what counted:
   - Collect every stage's **Named** bullets, done stages included
     (they are kept verbatim for exactly this). Append each into the
     alphabetical list byte-for-byte; if a bullet needs rewording to
     land, the stage said something it did not mean — reword it and
     say so in the ledger line.
   - **Then delete.** Every concept in any stage's Dies leaves
     `vocab.md`. Where the roadmap replaced it, its name joins the
     successor's `Rejected synonyms:` clause — the list shrinks while
     the negative index grows. A close that adds entries and removes
     none, on a roadmap whose Dies were concept-level, skipped this.
   - Merge on arrival: two entries serving one purpose are one entry
     (§2). If the file is near 25,000 characters, the merge-or-kill
     pass happens now, not next close.
   - Verify each surviving new entry's tags are themselves entries,
     and that it greps to at least one `*Concept*` citation — the
     stage's Done-when already checked this, so a miss here means a
     stage landed unverified. Say so in the Journal.
3. **Harvest the Journal** into the other durable files (create them
   on first harvest); skip entries already promoted:
   - → `ai/decisions.md`: choices clearing BOTH bars — scope (it
     constrains a whole subsystem or how future code in the area is
     written) and durability (the rationale outlives the roadmap and
     is not obvious from the code) — and user declarations (what they
     picked, their justification in their own terms, the options they
     discarded).

     Append-only, new entries at the top after the header. Entry:
     `## <one-line declarative title>` (the id the ledger and Journal
     cite), date, status (`active` at write), what was chosen, **Why**
     (the constraint, or the user's reason in their own terms),
     **Alternatives considered** (each with why rejected, one line),
     optional **Revisit when**. Under 25 lines — longer is two
     decisions. Prior entries are never edited or reordered, except
     flipping status to `superseded-by: <new title>`.
   - → `ai/state.md`: changed subsystem rationale and what it rules
     out, thesis shifts. Names do NOT come here — they went to
     `vocab.md` in step 2, and state.md holds no vocabulary layer.

     Two layers: systems (what each subsystem is, why, and what that
     choice rules out) and concepts (the thesis, in the opening
     paragraph). Present-tense and self-correcting — rewrite or delete
     superseded framing in the same pass, including prose that now
     merely restates a vocab entry; a correct gap beats content that
     misleads. Under 300 lines.
   - Everything unpromoted dies with the file, by design — no "just
     in case" copies.
4. **Update the parent** (if lineage names one and it is live):
   - Append a wind-down entry to the parent's Journal: what landed,
     decision titles promoted, residue routed.
   - Check off the delegated task in the parent's stage.
   - Re-verify the parent's downstream Assumes against the code this
     roadmap changed; rewrite stale ones (changes to a ratification
     portion go back to the user as a diff).
5. **Route residue.** Unfinished work → the parent's Plan. With no
   parent: surface all residue to the user — they own the queue of
   future roadmaps — and note the items they drop in the ledger line.
6. **Ledger** (`ai/ledger.md`): complete this roadmap's live line in
   place per the conventions — never append a second. For a supersession the
   outcome reads `superseded — re-plan after <new-id>` (name the
   successor now if it isn't filed yet) — the resumption record.
7. **Archive** the file — gzipped into `ai/roadmaps/archive/`, never
   deleted outright:

   ```sh
   mkdir -p ai/roadmaps/archive
   gzip -9n ai/roadmaps/<id>.md   # writes archive-side, removes source
   mv ai/roadmaps/<id>.md.gz ai/roadmaps/archive/
   ```

   No `git` verbs here: `ai/roadmaps/` is gitignored, so the roadmap
   was never tracked and the archive won't be either. It lives in the
   working tree and nowhere else — which is exactly why the harvests
   in steps 2–3 are not optional. Compression is the point, not disk: an
   archived roadmap is an archaeological artifact, and gzip keeps it
   out of ripgrep and glob results so a future session cannot mistake
   a dead plan for current truth. If `<id>.md.gz` already exists the id
   was reused — prefix the new one with the close date
   (`<YYYY-MM-DD>-<id>.md.gz`). **Never read from `archive/` to answer
   a question.** Anything still needed belonged in `ai/vocab.md`,
   `ai/state.md`, or `ai/decisions.md`, and the fix is to promote it
   there now.
8. **Commit** with `git` — the roadmap itself is invisible to git
   (`ai/roadmaps/` is gitignored); what lands is the code plus the
   tracked durable files this close touched (`ai/vocab.md`,
   `ai/state.md`, `ai/decisions.md`, `ai/ledger.md`).
