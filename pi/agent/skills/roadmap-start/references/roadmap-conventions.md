# Roadmap Conventions

The format spec for roadmaps (`ai/roadmaps/<id>.md`). Loaded on demand by
the `/skill:roadmap-start`, `/skill:roadmap-stage` and `/skill:roadmap-close` skills, which cite it
as "the conventions". `§1`–`§3` cite always-loaded `agents.md`: §1 Role &
Philosophy, §2 Code Organization (interface criteria), §3 Development
Workflow (TDD).

A **roadmap** is a staged implementation plan for a feature-set, refactor,
or abstraction redesign — ADR-weight, as much implementation as design,
the unit of planned work at every scale. An epic-level roadmap's stages
mostly delegate to child roadmaps; a leaf roadmap's stages are direct
work. The file holds only its Plan and Journal; procedure lives in the
three skills.

## Files & invariants

*   One file per roadmap: `ai/roadmaps/<id>.md`. Filename is the id (short
    slug), H1 is the title. The directory's top level holds live roadmaps
    and nothing else.
*   No status fields, no frontmatter — the top level is current truth.
    Live = the file exists; done = every stage marked done in its heading;
    dead = archived, only via `/skill:roadmap-close`.
*   `ai/roadmaps/archive/` holds closed roadmaps as `<id>.md.gz`, written
    only by `/skill:roadmap-close`. Gzipped so ripgrep and `*.md` globs pass over
    it. **Never read from `archive/` to answer a question** — what a
    closed roadmap still owed the future was promoted to `ai/vocab.md`,
    `ai/state.md`, and `ai/decisions.md` at harvest.
*   The active roadmap is the one the user is directing work at; it
    changes only by user direction — when ambiguous, ask.
*   A session working within a roadmap journals its learnings there; other
    sessions write no roadmap entries. The durable files are written only
    during `/skill:roadmap-close`'s harvest.

## Sections — two permanent, one transient

1.  **Plan** — the current understanding of *remaining* work. Stateless
    and freely rewritten as implementation teaches; history lives in the
    Journal. Opens with the goal, the findings that make it the right
    goal, the code shape and constraints it is planned against, and
    lineage in prose ("carved out of `tags`"). Then numbered stages in
    dependency order.
2.  **Open questions** — transient, and the gate. Every claim the plan
    rests on that is not yet true, one bullet each, naming how it
    resolves: read the code, ask the user, or measure it. **A roadmap may
    not climb a rung while this section is non-empty**; delete the section
    when it empties. A claim that can be neither read, asked, nor measured
    is a guess — delete it, and whatever rested on it.
3.  **Journal** — append-only, dated, never edited: discoveries, decisions
    (with discarded alternatives), plan rewrites and why, stage landings,
    child wind-downs. Prefix `harvest:` on entries destined for the
    durable files. Cruft is tolerated here; it faces judgment once, at
    close. Its last entry is where the roadmap stands.

## Resolution — the three rungs

A roadmap climbs one rung at a time, each entered only on the user's word.
Every rung is a complete, valid roadmap.

1.  **Shape** — the goal, the findings that make it right, and stages as
    numbered headings, each with **a short intent** (what changes
    conceptually, never how — one to three STE paragraphs, held to the
    cold-read test and the STE rule below, within the ` — shape` bar).
    Plus Open questions. No code blocks, no `---`, no
    Dies/Born/Named/Assumes/tasks/Done-when. Authoring produces a Shape
    document and stops.
2.  **Contract** — each stage gains its ratification portion: the `---`
    appears, the intent loses every sentence Dies/Born now state,
    Dies/Born/Named above it.
3.  **Workspace** — a stage gains Assumes, tasks, and Done when below the
    line. Filled only for stages close enough to plan against real code —
    often just the next one or two.

**Resolution is per stage below Shape, declared in the heading.** A
stage's heading names the highest rung it has reached (`### 5. Lineage —
shape`, `### 5. Lineage — contract`) and carries no marker once whole
(then ` — done` when it lands). An unmarked stage is a promise it is fully
written; a stage planned thinly *without* its marker is a lie. Resting at
` — shape` or ` — contract` is legitimate and needs no apology. Detail
written for stage 7 today is planned against a tree stages 1–6 will
change — plan far stages at Shape and say so.

**Bars.** `roadmap-lint` owns these and prints the remedy; the numbers are
here for authoring, not for arguing with.

| Scope | Warn | Hard |
|---|---|---|
| Whole plan at Shape (Journal never counted) | — | 100 |
| Stage at ` — shape` | 20 | 60 |
| Stage at ` — contract` | 100 | 120 |
| Stage unmarked (carrying workspace) | 120 | 140 |
| Material before stage 1 | 70 | — |

Past the hard bar the remedy is **splitting into stages**, each with its
own Dies/Born and place in the dependency order. Delegate only when the
stage is a sub-arc in its own right. Advancing a rung is the user's call,
never a way to make an oversized stage legal.

Run `roadmap-lint` on demand: it checks each file in `ai/roadmaps/`,
scoped by default to lines changed against HEAD; `-a` measures whole
files. It infers each rung from the `---` and the heading markers —
nothing is declared to it. The lint-on-write hook of the roadmap extension is
the same command scoped to the write it just saw.

## Stage grammar

**The heading and the intent are read cold.** The test for both: a reader
who knows the code and has read the goal — not the Journal, not the
session that wrote it — can say what the stage changes. The name after
the number is one of two things: a vocabulary word (a `*Concept*`, a
Named bullet already ratified, a code identifier) plus what happens to
it, or a plain sentence stating what is true once the stage lands.

```markdown
### 2. The clock flip — LamportClock dies, seqno is the frontier
### 2. Tests create through fixtures
```

A metaphor or a coined phrase (`A window on the thread`, `Hydration that
refuses`) is a title, not a name: only its author can decode it. The
intent names what the stage makes exist and stop existing, in
identifiers and concepts, and the difference a user or test observes.
"What changes conceptually, never how" forbids algorithm and file layout;
it does not forbid nouns. A sentence that could sit under a different
stage of the same roadmap (`One resolution rule for a reader, and a
rebuild that fails visibly`) has said nothing. Journal titles are
narrative and may be wry; stage names may not.

**The ratification surface is written in ASD-STE100** (Simplified
Technical English): the goal, the findings, and every stage's intent, at
every rung. STE is the cold-read test made mechanical. The Journal and
the workspace are exempt.

*   One topic per sentence, at most 20 words, at most six sentences per
    paragraph. A paragraph opens with its topic sentence.
*   Active voice, present tense, simple verb forms, articles kept. The
    subject is a named thing; the verb says what it does or what happens
    to it. No metaphor, no idiom, no noun cluster over three words.
*   One word, one meaning, the same word every time. The nouns are
    `*Concept*` cites, spelled as `ai/vocab.md` spells them, and
    backticked code identifiers — STE's technical names — and they carry
    the sentence. Every noun that has a vocab entry is cited. Every other
    noun is a plain general word, or it is a concept the roadmap has not
    named yet: at Contract it goes in Named, or the sentence is rewritten
    without it.

An intent at ` — shape` is one to three such paragraphs, within the
` — shape` bar. Longer is a stage that wants splitting, not a rung
advanced early.

Each stage is a ratification portion (above a `---`) and a workspace
(below). The user ratifies ONLY what is above the line; the workspace is
the agent's, freely rewritable. Any edit above the line is a re-ratification
event — present it as a diff. A roadmap's **ratification surface** is the
goal plus every stage's ratification portion. Done stages leave it and may
be compressed — except their Named bullets, kept verbatim until close
harvests them.

The ratification portion, in fixed order:

*   *Intent* — the stage's Shape text with every sentence Dies or Born
    now states deleted; what remains says which concept dies,
    generalizes, or gains symmetry, and why. STE and the cold-read test
    still apply.
*   ***Dies*** — a code block naming the identifiers and concepts this
    stage removes. **Always present**; deletion outranks addition. An
    empty list must be argued (`Dies: nothing — <why>`), is ratified, and
    can be wrong. `roadmap-lint`'s `stage-dies` warns on a ratified stage
    with no Dies.
*   ***Born*** — a code block with the actual new interface shapes
    (signatures, types), settled from the consumer side: write the ideal
    calling code first (§3) and let the shapes serve it. A delta over ~a
    screen means the stage is too big to ratify — delegate it.
*   ***Named*** — the concepts this stage coins, one bullet each in
    `ai/vocab.md`'s exact format, so harvest is a copy. **Born is shapes;
    Named is words** — a Born signature whose name is obvious needs no
    entry, and a Named concept often has no signature at all. Fill
    *Rejected synonyms* from the naming argument as it happens; it is
    recoverable nowhere else. Omitted entirely when the stage coins
    nothing — the common and preferred case, with no `Named: nothing`
    ritual and no lint warning. More than two or three entries means the
    stage is doing more than one thing.

**Born and Named are exhaustive and exact — a contract, not a sketch.**
Together they state completely what the stage adds: every
consumer-visible signature and type in Born, every concept the stage's
comments and docs will name in Named, and nothing in either that goes
unused. Implementation that needs a shape or concept they do not name —
or that leaves one they do name unused — has proved the contract wrong,
and the remedy is stop / revert / re-ratify / restart (`/skill:roadmap-stage`).
Both directions cost the same, so padding Born defensively is not an
escape.

Scope: Born covers what the stage exposes. A private helper created and
consumed entirely inside the stage's own implementation is not a Born
shape and trips nothing. A new *concept* trips Named even when internal,
because a comment will name it.

The revert cost is bounded by the stage's size — one more reason a stage
past its bar wants splitting before it is started.

The workspace: an **Assumes** line (the priors the stage rests on — the
replan tripwire), `- [ ]` tasks (outcome-level, never line-level code),
and a **Done when** line: Born shapes compile, every Dies name greps to
zero, every Named concept greps to at least one `*Concept*` use, plus
behavior checks that can't lie (a test, a command).

````markdown
### 2. Extract and point

One function owns note-text parsing; call sites keep their divergences
at the boundary, not in the parser.

Dies:
```rust
// add.rs    — TITLE_RE, parse_title_line()
// import.rs — HEADER_RE, scan_header()
// index.rs  — NOTE_RE, extract_meta()
```

Born:
```rust
// parse.rs (new)
pub struct ParsedNote { pub title: Option<Span>, pub date: Option<Span> }
pub fn parse_note(text: &str) -> ParsedNote;
```

Named:

- *Span* (*parse*) - A byte range into the source note text, resolved by
  the caller rather than copied out. Rejected synonyms: slice, extent.

---

Assumes: stage 1's characterization tests are trusted.

- [ ] Extract `parse_note` per Born; strictest site's behavior is the
      default, divergences handled at call sites
- [ ] Point all three sites at it as their regexes are deleted

Done when: Born shapes compile; every Dies name greps to zero; `*Span*`
greps to at least one real use; characterization suite green unchanged.
````

`ParsedNote` and `parse_note` are in Born and absent from Named — they
need no explanation. `*Span*` earns an entry: it encodes a decision (the
caller resolves the range) its signature cannot state.

## Interface discipline

For every interface a feature touches, survey returns exactly one of two
verdicts — there is no "just add a method":

*   **Clean fit** — used unmodified, as a typical user. Justify it: no
    semantic stretching, no smuggling through a metadata field.
*   **Concept review** — any modification triggers the cascade: review the
    interface's concepts against the new requirement → redesign so it
    integrates cleanly, held to §2's criteria → migrate the implementation
    and existing users → only then does the feature use it. Depth varies;
    it may end in one line ("concept-completion: the missing symmetric
    operation").

**The feature lands as the interface's n-th user, never its special
guest.** Interface work found in a land stage means prepare was
incomplete.

**Stage ordering:** prepare (interface/concept work) → land (the feature)
→ contract (existing users swept and simplified). Survey happens at
authoring time. Replacement work demolishes first: delete everything the
roadmap replaces before building, so old and new can never be merged.

**Delegation.** A stage too large to plan in place, or a concept review
that turns structural, becomes a child roadmap. A child born from a
concept review is scoped to THE ABSTRACTION AND ALL ITS USERS — never to
the triggering feature's need. Its survey inventories every user's usage
and scar tissue (the workarounds the old concepts forced); its contract
stage sweeps every user, simplifying each under the new concepts. The
proof a redesign is right: code gets deleted in users that never heard of
the triggering feature.

## `ai/ledger.md`

The roadmap index: one line per roadmap for its whole life, indented to
delegation depth. `/skill:roadmap-start` registers it live at ratification;
`/skill:roadmap-close` completes that same line in place — outcome (`landed |
abandoned | superseded`) and decision titles replacing `live`, position
and indent kept.

```markdown
- **cal** (07-19, live) — calendar sync
- **cal** (07-19) landed — calendar sync; decisions: "Derive tags into the index"
```

A superseded outcome reads `superseded — re-plan after <new-id>`. The
ledger only makes ids resolvable; what an id resolves to lives in the
durable files.

## Durable memory

`ai/vocab.md`, `ai/state.md`, `ai/decisions.md` — written only during
`/skill:roadmap-close`'s harvest, with the Journal and the stages' Named bullets
as the only capture sinks meanwhile. They partition by question and
nothing belongs in two: **vocab** *what is it called*, **state** *why is
the system this way*, **decisions** *why was this choice made*.

The partition is by lifetime: everything directly under `ai/` is durable
and tracked in git; everything under `ai/roadmaps/` is mortal and
gitignored. If a fact matters after close, it is in one of these three or
it is gone.

*   **`ai/vocab.md`** — the canonical name for every concept, and the only
    home for one. Flat, alphabetical, one bullet each:

    ```markdown
    - *Frontier* (*engine*) - An *arrangement*'s write edge, the position
      it is stepped through. Rejected synonyms: upper, write frontier, seal.
    ```

    Name, parenthesized tags, ` - `, one or two sentences, then an
    optional `Rejected synonyms:` clause. Tags are themselves vocab
    entries. Rejected synonyms are the negative index — an agent reaching
    for a rejected word greps, finds it, and uses the real one. **Under
    25,000 characters**, so the whole vocabulary is one `Read` away;
    approaching it triggers a merge-or-kill pass, not a second file.
*   **`ai/state.md`** — the current *thinking*: per-subsystem why and what
    it rules out, plus the thesis every design choice traces back to. No
    vocabulary layer — names live in `vocab.md`. Authoritative over
    contradicting code comments; a live roadmap's Plan overrides it for
    the area under work.
*   **`ai/decisions.md`** — append-only log of settled choices, each
    titled with the one-line declarative sentence the ledger and Journal
    cite.

Their write formats live in the `/skill:roadmap-close` skill, which is the only
thing that writes them.

## Referencing from code

Comments cite what outlives the roadmap: code identifiers, concept names,
and decision titles (citable once the ruling is journaled with `harvest:`)
— never a roadmap id or stage number, which are mortal coordinates. Name
every concept a comment will need in Named; harvest keeps the references
real.

**A concept is cited as `*Concept*`** — single asterisks, spelled as
`vocab.md` spells it, in comments, docs, `state.md`, and roadmaps.
Markers wrap the canonical form only, so inflections go outside:
`*Arrangement*'s`, `*Cone*s` — never `*arrangement's*`, which no grep
finds. Case is the one latitude, matched case-insensitively. Ordinary code
identifiers are backticked, not asterisked.

The one sanctioned use of a roadmap id in code is tagging scaffolding that
must die with the roadmap, in exactly the form `<id>:` (`todo!("<id>: …")`,
stubs, red-pin messages) — the colon makes the sweep precise. The tag
joins the roadmap's own Dies list, and close greps it to zero.
