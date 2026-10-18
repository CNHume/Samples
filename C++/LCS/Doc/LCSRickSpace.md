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
now reverse the descending list and linearly merge instead of sorting.  The
remaining item is the **range jump**, designed in the Enhancements section below.

## Enhancements

The following design closes RickSpace's remaining wall-clock gap to RickTime on
the character path while keeping its linear memory.  It replaces the
per-diagonal walk in `NextForward`/`NextBackward` with a *range jump* that skips
the diagonals whose candidates the dominance test is bound to reject.

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
