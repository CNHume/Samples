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

## Orientation

Rick's time bound assumes `m ≤ n` (the shorter sequence is scanned, the longer
is tabled).  `LCSString::Compare` therefore swaps the inputs when necessary
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
RickTime's `O(d)` recording, not a fixable overhead.  The hybrid auto-select
remains the only path to RickTime's speed.

## Where the time goes

Instrumenting `Find` to accumulate wall-clock per phase — `BuildOccurrences`
(table build), the `FindMidpoint` contour sweep, and the greedy early-termination
emission — gives the split that decides the next improvement:

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
One structural detail: diagonal steps per contour build grow with mutation
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

## Hybrid auto-select (alternative)

With contour reuse reverted, this is the only remaining path to RickTime's ~25 s,
at the cost of a try/fail/retry heuristic.  The range-jump and contour-reuse
results together pinned the RickSpace↔RickTime gap to *memory*, not to
any avoidable per-step cost: RickTime finds each dominant match exactly once in
one diagonal sweep (`O(m + d)` probes), while RickSpace rebuilds forward and
backward contours rank-by-rank at every Hirschberg level — ~9.5× the LCS length
in contour builds at 4.6M/100-subs.  No within-sweep optimization can close a
recomputation gap, so the pragmatic design stops trying to make RickSpace run
like RickTime and instead **chooses between them**: run RickTime when its
recording fits, fall back to RickSpace when it does not.

### Selection

`compare` gains an auto mode (`-a a`, made the default) that runs `LCSRickTime`
under a **byRank budget** and hands off to `LCSRickSpace` when the budget is
exceeded:

- RickTime's peak memory is its `O(ns)` occurrence tables plus one `DomMatch`
  (8 bytes) per recorded dominant match in `byRank`.  The budget is a cap on
  that record count `d`, derived from available RAM minus the known,
  size-constant table footprint, with a safety margin.
- When the count exceeds the cap, RickTime stops recording, frees `byRank`, and
  reports "too large"; `compare` then re-runs the same inputs through
  `LCSRickSpace`.

Because the cap is RAM-derived, RickTime is only attempted when its recording
can actually fit, so the auto mode keeps RickSpace's "never OOMs" property while
taking RickTime's speed whenever memory allows.  Scope is the character path
(`-t`), where `LCSRickTime` exists; the record path is unchanged.

### Worst case

The concern with any fallback is that the fallback path itself becomes
pathological.  It does not, because every branch is bounded by either the cap or
RickSpace's own bound:

**Time.**  In the `d ≤ cap` regime the auto mode is exactly RickTime.  In the
`d > cap` regime it is a *partial* RickTime sweep (recording stops at the cap)
followed by a full RickSpace run.  The partial sweep costs at most one `O(ns)`
table build plus an `O(m)` diagonal walk — bounded by the cap, not by `d` — so
the worst case is **RickSpace + one RickTime sweep**, roughly 65 s + 25 s
≈ 90 s at 46.7M, i.e. ~1.4× RickSpace.  It is never unboundedly worse than
running RickSpace directly; the overhead is a constant controlled by the cap.

**Memory.**  Peak RSS is the larger of (a) RickTime's tables plus a cap-sized
`byRank` — the instant before a bail-out — and (b) RickSpace's flat `O(ns)`
footprint.  Both stay under the host limit by construction, because the cap is
set from available RAM with a margin.  The hybrid is therefore **not**
flat-memory like RickSpace: it deliberately spends up to the cap to buy
RickTime's speed.  Its peak is capped, never `O(d)`-unbounded, and never an
OOM — which is the property that matters.

**Fits-but-slower risk** (the one tunable).  If RickTime fits yet is slower
than RickSpace — a boundary region near the cap — the auto mode is slower than
pure RickSpace.  At every benchmarked point where RickTime fit (100 and
1000 subs) it was faster — 3.4–3.6× at 16M, 1.35× at 4.6M/1000-subs — and it
OOM'd rather than slowed at 10k subs.  If a fits-but-slower case appears,
tightening the cap (a smaller record budget) moves the fallback earlier; a
safety margin in the cap also guards against swap-thrash as the working set
approaches the host limit.

**Double work on abort.**  The bail-out discards RickTime's tables and redoes
RickSpace's different `next`/`prev`/`next1`/`prev1` build — a second `O(ns)`
table build of a few seconds — paid only in the high-mutation regime where
RickSpace is required anyway.  Correctness is unaffected: both algorithms are
fuzz-verified to agree with the DP reference, so the emitted LCS is valid no
matter which one ran.

### Open questions

- Exact cap derivation — query available RAM at startup vs. a conservative
  constant calibrated to the benchmark host — and the size of the safety margin.
- Whether the auto mode is the default or stays opt-in behind `-a a`.
- Whether a cheap `d` pre-estimate (e.g. a length-only forward pass) is worth
  it to skip the double table build in the common case; only if the abort
  regime turns out to be frequent.

## Build and test

C++20 (uses `std::format`, `std::ssize`, and designated initializers).  From
`../compare`:

```sh
g++ -std=c++20 -O2 -Wall -Wextra *.cpp -o compare

# Difference (Hunt-Szymanski vs. RickSpace):
compare -t     file1 file2
compare -t -as file1 file2

# Correspondence:
compare -t -as -c file1 file2
```

Correctness was verified against an O(m·n) dynamic-programming reference and
against `LCSHuntSzymanski::Find` on over 220,000 fixed and randomized cases.

## References

- Claus Rick, "Simple and fast linear space computation of longest common
  subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.
- Claus Rick, "A new flexible algorithm for the longest common subsequence
  problem", *Proc. CPM'95*, Lecture Notes in Computer Science 937, Springer,
  1995, pp. 340–351.  Also in *Nordic Journal of Computing* 2 (1995),
  pp. 444–461.
- A. Apostolico, C. Guerra, "A fast linear space algorithm for computing longest
  common subsequences", *Proc. 23rd Allerton Conference on Communication,
  Control, and Computing*, 1985, pp. 76–84.
- A. Apostolico, S. Browne, C. Guerra, "Fast linear-space computations of
  longest common subsequences", *Theoretical Computer Science* 92 (1992),
  pp. 3–17.
- D. S. Hirschberg, "Algorithms for the longest common subsequence problem",
  *Journal of the ACM* 24(4), 1977, pp. 664–675.
- H. Goeman, M. Clausen, "A new practical linear space algorithm for the longest
  common subsequence problem", *Kybernetika* 38(1), 2002, pp. 45–66.
- R. P. Dilworth, "A Decomposition Theorem for Partially Ordered Sets",
  *Annals of Mathematics* 51 (1950), pp. 161–166.
- Daniel S. Hirschberg, "A linear space algorithm for computing maximal common
  subsequences", *Communications of the ACM* 18(6), 1975, pp. 341–343.
