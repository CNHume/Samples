# LCSRickSpace — Rick (2000) linear-space LCS

An implementation of the Longest Common Subsequence (LCS) problem based on:

> Claus Rick, "Simple and fast linear space computation of longest common
> subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.

`LCSRickSpace` is the Rick/Hirschberg refinement integrated into the `compare`
application.  It is a run-time alternative to `LCSHuntSzymanski`, selected with
the `-as` switch in `-t` (text) mode.  (`delta` continues to use only the
Hunt–Szymanski algorithm.)

## Background

The contour/dominant-match paradigm behind `LCSRickSpace` predates Rick's papers.
Its lineage, as traced in Rick (2000):

- **Hirschberg (1977)** first suggested computing the LCS match classes
  ("contours") rank by rank.
- **Apostolico & Guerra (1985)** introduced the first fast *linear-space* LCS
  algorithm beyond the `O(mn)` DP, representing the DP frontier as contours of
  *dominant* matches — the origin of the `Contour` / dominant-match
  representation used throughout `LCSRickSpace`.
- **Apostolico, Browne & Guerra (1992)** improved this to `O(pm)` and
  `O(n(m−p))` time while retaining linear space.
- **Rick (1995)** gave the "flexible algorithm" with the
  `O(ns + min{pm, p(n−p)})` bound, since widely regarded as the fastest
  general-purpose LCS method.
- **Rick (2000)** — the paper implemented here — added the linear-space
  construction: forward/backward contours computed in alternation, the
  crossing test (Lemma 3), and Hirschberg recursion, all while keeping that
  bound.
- **Goeman & Clausen (2002)** re-derived the same bound from the *dual*
  (antichain / T–L–B–R decomposition) viewpoint.

Rick (2000) credits the split explicitly: computing contours rank by rank was
"first suggested by Hirschberg [6]", with "efficiency … later gained by
concentrating on the computation of dominant matches [3]" (Apostolico et al.
1992).

Underlying the dual viewpoint is **Dilworth's theorem** (1950): in a finite
partially ordered set, the maximum size of an antichain equals the minimum
number of chains in a chain decomposition — and, dually (Mirsky's theorem), the
maximum size of a *chain* equals the minimum number of *antichains* in an
antichain decomposition.  For LCS, the match set `M` is a poset under the
product order: a **chain** is a common subsequence, and an **antichain** is a
set of pairwise incomparable matches.  Hence the LCS length `p` is at once the
length of the longest chain (what Rick's contours find) and the minimum number
of antichains needed to cover `M` (what Goeman & Clausen's decomposition
constructs).  This equivalence is why the chain-based and antichain-based
algorithms share the same bound.

### The "DP frontier" in the Apostolico–Guerra description

The Background phrasing "representing the DP frontier as contours of dominant
matches" compresses three ideas.  The classic LCS dynamic program fills an
`(m+1)×(n+1)` table of prefix LCS lengths — `O(mn)` cells, nearly all of them
irrelevant.  Its **frontier** is the sparse boundary of that computation: the
matches lying on the edge of the already-settled region.  The frontier of rank
`k` is exactly the contour `FC[k]`, and it can be represented by its
**dominant** (minimal) matches alone — an antichain — which is what makes each
contour cheap to extend.  The whole DP thus collapses to a walk over the
contours:

| DP view | Sparse view |
|---|---|
| fill `O(mn)` cells | walk only the frontier |
| the frontier at length `k` | the contour `FC[k]` |
| all rank-`k` matches on it | only its dominant (minimal) matches |

That is the compression that turns `O(mn)` into Hirschberg's `O(pn)` and the
`O(ns + min{pm, p(n−p)})` bound below.  The fuller citation history is
centralized in `LCS Overview.md`.

Linear-space contour algorithms, as tabulated in Rick (2000):

| Year | Authors | Time |
|---|---|---|
| 1985 | Apostolico, Guerra | `O(m log m + pm)` |
| 1992 | Apostolico, Browne, Guerra | `O(pm)` / `O(n(m−p))` |
| 1999 | Goeman, Clausen | `O(min{pm, m log m + p(n−p)})` |
| 1999/2000 | Rick | `O(min{pm, p(n−p)})` |

## Relationship to the other classes

```
LCS                        shared typedefs: PAIRS, INDEXES,
                           STRING_TO_INDEXES_MAP, MATCHES
├── LCSHuntSzymanski       Find(pairs, matches)          — threshold array
└── LCSRickSpace                Find(pairs, matches, m, n) / Find(pairs, s1, s2)
                           — contours + Hirschberg
```

Both algorithm classes derive publicly from `LCS`.  `LCSHuntSzymanski` and the
record path of `LCSRickSpace` consume the `Match()` output; the character path of
`LCSRickSpace` builds its tables straight from the code points instead.  Their
`Find()` methods all return the LCS length and fill `*pairs` with matched
positions in ascending order, so the downstream `Delta::Coalesce` / `Context`
/ `Complement` / display pipeline is identical regardless of which algorithm
ran.

## Input

`LCSRickSpace` answers every occurrence query in O(1) from dense `next`/`prev`
tables (the "LeftPos"/"TopPos" preprocessing of the `O(ns)` bound), built over
*both* strings so the column sweep of the dual diagonal sweep can run.  The two
`Find()` overloads build those tables differently:

- **Character path (`-t -as`)** — `Find(pairs, s1, s2)` compresses the code
  points of `s2` and builds the tables directly in `O(s·(m+n))` time and space,
  where `s` is the number of distinct code points.  No match lists are
  materialized, so this path stays feasible for very long strings even when
  the number of matches `r` would be quadratic.
- **Record path (`-as` without `-t`)** — `Find(pairs, matches, m, n)` reuses the
  shared `Match()` output (`MATCHES = deque<INDEXES*>`, where `*matches[i]` is
  the sorted list of positions in the second sequence matching position `i` in
  the first).  Two rows sharing a symbol share the same match-list pointer, so
  pointer identity is the symbol→id compression; the tables cost `O(s·(m+n) + r)`.

## Algorithm

A *match* is an ordered pair `(i, j)` with `A[i] = B[j]`.  A match has a
*forward rank* `k` when the longest common prefix length ending at it is `k`,
and a *backward rank* `k` when the longest common suffix length beginning at
it is `k`.

A match `(i1, j1)` *dominates* `(i2, j2)` when `i1 ≤ i2` and `j1 ≤ j2` and they
differ.  The `k`-th forward contour `FC[k]` is the antichain of *dominant*
(minimal under the product order) rank-`k` matches; `BC[k]` is the symmetric
antichain of *maximal* rank-`k` matches.  Being an antichain, each contour is
stored sorted by ascending `index1`, which forces descending `index2`.

Contours are computed in the alternating order `FC[1], BC[1], FC[2], BC[2], …`.
When the two most recently computed contours first *cross* — no backward match
is strictly below-right of any forward match — the LCS length is
`p = f + b − 1`, and a *midpoint* of some LCS is any forward match having a
backward match non-strictly below-right of it (Rick, Lemmas 2 and 3).

`Hirschberg` then recurses on the prefix and suffix separated by that midpoint.
Only the two most recent contours of each direction are retained, so the
working set is linear in the input lengths.

Four refinements from the paper are implemented:

- **O(p) contour scans** — the cut (`CutForward`/`CutBackward`), the crossing
  test, and the midpoint search are single merged scans over the two sorted
  contours (`O(|forward| + |backward|)`), not quadratic products.
- **Lemma 5 contour cutting** — before extending `FC[k]`, forward sources that
  have no backward-contour match strictly below-right (and so can only reach
  matches whose rank sum cannot reach the LCS) are pruned; symmetrically for
  backward sources.
- **Dual diagonal sweep** — each contour is built by Rick's (1995) row +
  column sweep: a row sweep for the horizontal stripe (`j − j0 ≥ i − i0`) and
  a column sweep for the vertical stripe (`i − i0 > j − j0`), interleaved over
  the diagonal and begun at the first source, so the idle prefix before the
  sources is skipped.  A cross-stripe dominance test (a column-sweep match at
  row `≤ i` dominates a row-sweep match at `(i, j)`, and vice versa) keeps the
  two sweeps consistent.
- **Completeness detection** — once a dominant match has both components within
  the current diagonal (`lmin(k)`), no later diagonal can hold another dominant
  match, so the sweep stops.  This is the "outer matches" narrowing of Lemma 5:
  it caps each contour's work at `O(min{m, n−p})`, giving the paper's
  `O(ns + min{pm, p(n−p)})` bound.

## Reconstruction

`LCSRickSpace::Hirschberg` recurses over full-string index ranges `[row0,row1) ×
[column0,column1)` and emits each matched position as a `(row, column)` `Match`,
in ascending order.  Because full-string coordinates are used, no offset
bookkeeping is required, and the emitted pairs feed `Delta::Coalesce` unchanged.

Rick's early-termination rule is applied: when a sub-problem's LCS length
equals the length of its shorter string, that whole string is part of the LCS,
so the recursion stops and the greedy leftmost matches are emitted directly.
This prunes the recursion tree on near-identical inputs and removes the
`O(log m)` factor the balanced divide-and-conquer would otherwise introduce.

## Threading (rejected)

A two-thread Hirschberg root split was prototyped behind a `TEST_THREADING`
compile-time switch, measured, and removed.  It was a wash on the 4.6M E. coli
corpus (9.12 s → 9.01 s at 100 substitutions, 52.3 s → 52.5 s at 1,000): the
contour sweep is memory-bound, so two threads scale it only ~1.1–1.2× on this
hardware, and the atomic increments the threaded build needed for its counters
cost about as much as that gain.  A deeper thread pool would only add contention
for the same memory bandwidth, so parallelizing the recursion is not worthwhile
— the sweep, not the split, is the bottleneck, and the sweep is already
memory-bound.

## Orientation

Rick's time bound assumes `m ≤ n` (the shorter sequence is scanned, the longer
is tabled).  `LCSText::Compare` therefore swaps the inputs when necessary
before calling `Match()`, and un-swaps the reported `begin1`/`begin2` positions
on the resulting pairs.  `LCSRickSpace::Find()` itself is orientation-agnostic.

## Selection

In `compare`, the `-a` switch selects the LCS algorithm: `-ad`
(`LCSHuntSzymanski`, the default), `-at` (`LCSRickTime`, the 1995 flexible
algorithm), or `-as` (`LCSRickSpace`, this algorithm).  `LCSRickSpace` serves
the character (code-point) path under `-t` and the record/word path otherwise.

## Future work

The **merge instead of sort** refinement has landed: the row-swept and
column-swept matches are each emitted in order, so `NextForward`/`NextBackward`
now reverse the descending list and linearly merge instead of sorting.  Profiling
(Where the time goes) then located the remaining gap in the contour sweep, and
the **contour reuse** attack on it was implemented, fuzz-verified, benchmarked as
a wash, and reverted — see its Result subsection.  Both experiments (the range
jump and contour reuse) point to the same conclusion: RickSpace's linear-space
recomputation is near-optimal, and the gap to `LCSRickTime` is the price of
RickTime's `O(d)` recording, not a fixable overhead.  The remaining plan is the
**advisory guidance** below: keep the explicit algorithm choice and warn or
suggest when the other algorithm would serve the input better.

## Where the time goes

Instrumenting `Find` to accumulate wall-clock per phase — `BuildOccurrences`
(table build), the `FindMidpoint` contour sweep, and the greedy early-termination
emission — gives the split that decides the next improvement.  Timings are
wall-clock on an Intel Core i7-9700K (8 cores) at 3.60 GHz with 15 GB of RAM,
running Ubuntu 26.04 LTS under WSL2.

| Input | total | table build | **contour sweep** | greedy | misc | Contours | Rows (diagonals) |
|---|---:|---:|---:|---:|---:|---:|---:|
| 4.6M / 100 subs | 9.80 s | 0.48 s (5%) | **9.04 s (92%)** | 0.06 s | 0.22 s | 44.3M | 139M |
| 4.6M / 1000 subs | 53.6 s | 0.55 s (1%) | **52.8 s (99%)** | 0.05 s | 0.21 s | 56.9M | 981M |
| 16M / 100 subs | 21.5 s | 2.27 s (11%) | **18.2 s (84%)** | 0.14 s | 0.95 s | 120M | 523M |

The **contour sweep is 84–99% of the time** at every scale and mutation level;
table build (5–11%), greedy emission (<1%), and recursion bookkeeping (2–4%)
are noise.  The recomputation — 44M–120M rank-by-rank forward+backward contour
builds summing to 139M–981M diagonal steps — *is* the cost.  This closes the
question the range jump left open: the gap is not the per-diagonal walk (only
3.3% of steps are skippable), it is the *number of contour builds* themselves.
Attacking that directly — contour reuse — halved the builds but not the time: the
sweep is memory-bound, not CPU-bound (see the Contour reuse Result below).  One
structural detail: diagonal steps per contour build grow with mutation
(3.1 at 100 subs, 17.2 at 1000 subs on 4.6M), so the cost concentrates in the
high-mutation regime where the antichains are wide.

## Contour reuse (reverted)

The design below was the direct attack on the recomputation the profile
identifies: stop rebuilding the contours a parent already computed.  It was
implemented, fuzz-verified, benchmarked as a wash, and reverted — the Result
subsection records why and what it implies.  A Hirschberg node builds `FC[1..f]`
and `BC[1..b]` to find one midpoint, then recurses into a prefix and a suffix:

- the **prefix**'s forward contours are the parent's `FC` restricted to matches
  strictly above-left of the midpoint (rank is preserved there, because every
  predecessor of such a match lies above-left of the midpoint too), and
- the **suffix**'s backward contours are the parent's `BC` restricted to matches
  strictly below-right of it.

So pass the filtered parent contours down and let each child build only its
*cross* direction (the prefix's backward, the suffix's forward).  Each level's
contour work then drops from `p` to `p/2`, roughly **halving** the contour builds
and the diagonal steps — the predicted 65 s → ~37–40 s at 46.7M/100-subs (≈1.8×).

### Ceiling and memory

Halving is the ceiling for this scheme: the cross directions are genuinely new at
each level, and reusing those too would mean retaining every contour — RickTime's
`O(d)` recording.  Retaining the reused (aligned) contours costs one level's
worth of matches: ~`p` matches (a few hundred MB at 46.7M), negligible beside the
8.36 GB tables.  But at high mutation the contours are wide antichains
(`Rows`/contour 3.1 → 17.2), so a level's contours approach `O(d)` — the same
growth that OOMs RickTime.  To keep the "never OOMs" guarantee there, cap the
retained contours and recompute past the cap; that cap is a fixed budget (a
smooth time↔space tradeoff), not a fallback.

### Result (reverted)

The reuse was implemented, fuzz-verified (all 200,000 trials passed), and
benchmarked back-to-back against the discard-and-recompute baseline on
4.6M/100-subs:

| Build | Contours | Rows | User time | Wall | Peak RSS |
|---|---:|---:|---:|---:|---:|
| Baseline | 44.3M | 139M | 9.01 s | 9.46 s | 0.80 GB |
| Contour reuse | 24.5M | 94.8M | 8.75 s | 10.09 s | 1.93 GB |

The reuse cut the recomputation exactly as designed — 1.81× fewer contour builds
and 1.47× fewer diagonal steps — yet the net time was a wash (3% faster on CPU,
7% slower wall) and the memory 2.4× worse.  Retaining and filtering the contours
(`FilterBox` allocations plus a 1.13 GB larger working set) consumed the saved
sweep work, because the sweep is memory-bound, not CPU-bound.

This overturns the premise of this section.  RickSpace's discard-and-recompute is
already near-optimal for linear space: the 8 GB occurrence tables — not the
contour rebuilds — are the binding cost, and retention only adds to the one
resource that is scarce.  The gap to RickTime is therefore the price of
RickTime's `O(d)` recording (its memory), not a fixable recomputation overhead,
and the code was reverted.

## Range jump (reverted)

The design below was the first attempt to close RickSpace's remaining wall-clock
gap to RickTime on the character path while keeping its linear memory.  It
replaced the per-diagonal walk in `NextForward`/`NextBackward` with a *range
jump* that skips the diagonals whose candidates the dominance test is bound to
reject.  It was implemented, fuzz-verified, benchmarked, and reverted — the
Result subsection records why — and is superseded by the contour reuse design
above.

### The confirmation cost

`NextForward` walks each diagonal `d` (`rowIndex = row0 + d`,
`columnIndex = column0 + d`) and runs two sweeps:

- **Row sweep** — candidate `(rowIndex, column)`,
  `column = next[rowId[rowIndex]][lower]`, emitted when
  `column < rowThresh && rowIndex < colThresh`, then `rowThresh = column`.
- **Column sweep** — candidate `(row, columnIndex)`,
  `row = next1[colId[columnIndex]][lower]`, emitted when
  `row < colThresh && columnIndex < rowThresh`, then `colThresh = row`.

Between two emissions the sweep still visits every diagonal, computes a
candidate, and has the dominance test reject it.  A substitution inside a
repeated run (chr21's `N` regions) shifts the match off the diagonal, so this
confirmation walk is the 65.4 s vs 25.1 s gap to `LCSRickTime` at
46.7M/100-subs.

### The jump

Advance the diagonal straight to the smallest `d` where either sweep can emit,
instead of `diagonal++`: two sub-jumps, then `nextDiagonal = min(rowJump, colJump)`.

**Row-sweep jump.**  A symbol `σ`'s candidate passes when its next occurrence at
or after the diagonal column is left of `rowThresh` — equivalently, when the
diagonal column does not exceed `σ`'s last occurrence before `rowThresh`:

    rowJump = min over σ of  next1[σ][r + 1]
              where next1[σ][r + 1] ≤ row0 + prev[σ][rowThresh − 1] − column0
                and sourceCol + 1 ≤ prev[σ][rowThresh − 1]

**Column-sweep jump** (transposed):

    colJump = min over σ of  next[σ][c + 1]
              where next[σ][c + 1] ≤ column0 + prev1[σ][colThresh − 1] − row0 − 1
                and sourceRow + 1 ≤ prev1[σ][colThresh − 1]

Both queries use only the `next`/`prev`/`next1`/`prev1` tables already built, so
on the character path (`s = 5` symbols) each jump is `O(s) = O(1)` — no new data
structure.  For rank-1 contours (`contour` empty) the `sourceCol`/`sourceRow`
conditions drop out.

### Completeness

The jump is clamped to the existing completeness diagonal — the first `d` where
`column0 + d ≥ rowThresh` or `row0 + d ≥ colThresh` — and the loop breaks there,
exactly as the per-diagonal scan does.

### `NextBackward`

The mirror image, with the thresholds moving rightward/downward; the queries
become "smallest previous row/column" over `prev`/`prev1` where the forward
sweep used `next`/`next1`.

### Record path

The same logic applies, but `s` is large there, so the `O(s)` symbol scan is
infeasible and would be replaced by a wavelet matrix or per-symbol rank/select
with range-min.  Deferred; the character path is the target.

### Verification and plan

The jump must emit exactly the same dominant matches as the per-diagonal scan,
so the 200k-trial fuzz harness is the correctness gate.  The plan: add
`RowJump`/`ColumnJump` helpers, rewire the two loops to
`min(rowJump, colJump)`, advance the source cursors in one step, then fuzz and
benchmark 46.7M/100-subs.

### Result (reverted)

The jump was implemented, fuzz-verified (all 200,000 trials passed, emitting
exactly the same dominant matches), and benchmarked back-to-back against the
per-diagonal scan on 46.7M/100-subs:

| Build | Wall | Peak RSS |
|---|---:|---:|
| Per-diagonal scan | 71.5 s | 8.36 GB |
| Range jump | 158.3 s | 8.36 GB |

The jump is ~2.2× *slower*, with memory unchanged.  Instrumenting the `Rows`
counter on 4.6M/100-subs shows why: the jump visits 134,775,921 diagonals
against the scan's 139,382,907 — it skips only **3.3%** — while every visited
diagonal now pays an `O(s)` scan (5 symbols × 2 sweeps with `lower_bound`)
instead of the scan's `O(1)`.  On near-identical inputs the matches are dense
on the diagonal, so there is almost nothing to skip and the overhead dominates.

This falsifies the hypothesis above that the confirmation walk is the 65.4 s
vs 25.1 s gap to `LCSRickTime`: that gap is structural — RickSpace recomputes
forward and backward contours at every Hirschberg level, while RickTime sweeps
once.  The jump's ceiling on substitution-only data is a few percent, so it
cannot reach the RickTime time and was reverted.  A jump could still pay off on
insertion/deletion-heavy inputs with long off-diagonal gaps, but no such corpus
is benchmarked here.

## Advisory guidance (proposed)

The two reverted experiments settled the question: RickSpace's linear-space
recomputation is near-optimal, and the only lever that reaches RickTime's speed
is RickTime's memory.  So instead of making either algorithm something it is not —
or auto-switching between them — `compare` keeps the explicit `-ad`/`-as`/`-at`
choice and *advises* the user when that choice is pathological for the input.
This delivers the hybrid's value (steering toward the better algorithm) without
its try/fail/retry: no automatic re-run, no bimodal performance, and the user
stays in control.

### Two one-way hints

The two algorithms fail asymmetrically, so the guidance is two one-way hints
rather than one symmetric mechanism:

**`-at` (RickTime) — a proactive warning.**  RickTime's peak memory is its
`O(ns)` tables plus one `DomMatch` (8 bytes) per recorded dominant match in
`byRank`, and `byRank` grows monotonically during its single sweep.  It can
watch its own record count `d` and, when `d` would push the working set past
available RAM (with a safety margin), print
"match recording needs ~X GB beyond the tables; use `-as` for linear memory" and
abort cleanly — before the OS OOM-killer does.  This turns an abrupt kill into a
clear diagnostic; the user re-runs with `-as`.

**`-as` (RickSpace) — a post-hoc suggestion.**  RickSpace never runs out of
memory, so its pathology is slowness: near-identical inputs are exactly where it
is ~2.6× slower than RickTime and where RickTime's recording is small enough to
fit.  The one cheap signal is the LCS length `p` RickSpace already returns; when
`min(m, n) − p` is small, it can print
"near-identical input; `-at` would likely fit and run ~2.6× faster".  Post-hoc
and advisory, so imprecision is acceptable — the user simply re-runs.

### Why advisory, not automatic

- `compare` is already user-driven through `-ad`/`-as`/`-at`; an auto mode would
  second-guess an explicit choice.
- No try/fail/retry means no variable, bimodal performance (the objection to the
  auto-select form of this plan).
- The user may legitimately prefer RickSpace's flat `O(ns)` memory even where
  RickTime would be faster; a suggestion lets them make that call.
- Deterministic: the hints are a pure function of (input, chosen algorithm,
  budget) and never alter the emitted LCS.

### Implementation sketch

- **RickTime warning** — a `byRank` budget check in the recording path (RickTime
  already records through `push_back`); when the count exceeds a RAM-derived cap,
  free `byRank` and report "too large" instead of running on into an OOM.  This
  reuses the bail-out *detection* from the earlier auto-select design, but it
  *reports* rather than *retries*.
- **RickSpace suggestion** — after `Find` returns `p`, compare emits the hint when
  `min(m, n) − p ≤ threshold`; the existing `Diagnostic` machinery is the natural
  home for both hints.

### Open questions

- Budget source for the RickTime cap (query available RAM at startup vs. a
  conservative constant) and the size of the safety margin.
- The "near-identical" threshold for the RickSpace suggestion — an absolute bound
  on `min(m,n) − p`, or a ratio — and whether to also report estimated RickTime
  time/memory.
- A quiet switch so the hints can be suppressed in scripts.

## Build and test

C++20 (uses `std::format`, `std::ssize`, and designated initializers).  From
`../compare`:

```sh
g++ -std=c++20 -O2 -Wall -Wextra *.cpp -o compare

# Difference (Hunt-Szymanski vs. RickSpace):
compare -t     file1 file2
compare -as -t file1 file2

# Correspondence:
compare -as -c -t file1 file2
```

Correctness was verified against an O(m·n) dynamic-programming reference and
against `LCSHuntSzymanski::Find` on over 220,000 fixed and randomized cases.

## References

- Claus Rick, "[Simple and fast linear space computation of longest common subsequences](https://theory.cs.uni-bonn.de/blum/Mitarbeiter/rick/welcome-english.html)", *Information Processing Letters* 75 (2000), pp. 275–281.  [DOI](https://doi.org/10.1016/S0020-0190(00)00100-1)
- Claus Rick, "[A new flexible algorithm for the longest common subsequence problem](https://theory.cs.uni-bonn.de/blum/Mitarbeiter/rick/welcome-english.html)", *Proc. CPM'95*, Lecture Notes in Computer Science 937, Springer, 1995, pp. 340–351.  Also in *Nordic Journal of Computing* 2 (1995), pp. 444–461.  [DOI](https://doi.org/10.1007/3-540-60044-2_53)
- A. Apostolico, C. Guerra, "[A fast linear space algorithm for computing longest common subsequences](https://www.semanticscholar.org/paper/A-Fast-Linear-Space-Algorithm-for-Computing-Longest-Apostolio-Guerra/7d380f831959f869af2dcacad264c95468822109)", *Proc. 23rd Allerton Conference on Communication, Control, and Computing*, 1985, pp. 76–84.
- A. Apostolico, S. Browne, C. Guerra, "[Fast linear-space computations of longest common subsequences](https://www.sciencedirect.com/science/article/pii/030439759290132Y)", *Theoretical Computer Science* 92 (1992), pp. 3–17.
- D. S. Hirschberg, "[Algorithms for the longest common subsequence problem](https://dl.acm.org/doi/pdf/10.1145/322033.322044)", *Journal of the ACM* 24(4), 1977, pp. 664–675.  [DOI](https://doi.org/10.1145/322033.322044)
- H. Goeman, M. Clausen, "[A new practical linear space algorithm for the longest common subsequence problem](https://www.kybernetika.cz/content/2002/1/45)", *Kybernetika* 38(1), 2002, pp. 45–66.
- R. P. Dilworth, "[A Decomposition Theorem for Partially Ordered Sets](https://doi.org/10.2307/1969503)", *Annals of Mathematics* 51 (1950), pp. 161–166.
- Daniel S. Hirschberg, "[A linear space algorithm for computing maximal common subsequences](https://dl.acm.org/doi/pdf/10.1145/360825.360861)", *Communications of the ACM* 18(6), 1975, pp. 341–343.  [DOI](https://doi.org/10.1145/360825.360861)
