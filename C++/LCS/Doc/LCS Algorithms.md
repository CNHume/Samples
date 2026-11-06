# LCS Algorithms

`nabla` ships four Longest Common Subsequence algorithms, each with a regime
where it is fastest.  This file documents the algorithms themselves; how `-Aa`
chooses among them is in `LCS Algorithm Selection.md`.

Notation: `m, n` are the sequence lengths (`m ≤ n` after swap); `p` is the LCS
length; `D = m + n − 2p` is the edit distance; `r` is the match count (the
number of matched position pairs); `d` is the number of dominant matches; `s`
is the alphabet size (number of distinct symbols).

| Algorithm | Time | Space | Regime | Switch |
|---|---|---|---|---|
| Myers (1986) | `O(ND)` | `O(D²)` history (`O(D)` length-only) | near-identical (`D` small) | `-Ad` |
| Hunt–Szymanski (1977) | `O((r+n) log n)` | `O(r)` | sparse matches (`r` small) | `-Am` |
| RickSpace (2000) | `O(ns + min{pm, p(n−p)})` | `O(ns)` linear | general, never OOMs | `-As` |
| RickTime (1995) | `O(ns + min{pm, p(n−p)})` | `O(ns + d)` | dense, fastest find | `-At` |

The first two are parameterized by the *edit distance* and the *match count*
respectively, so each has a hard failure mode (Myers degrades as `D` grows;
Hunt–Szymanski as `r` densifies).  The two Rick algorithms are parameterized by
the *LCS length* and the *alphabet size*, so they stay bounded across the whole
similarity spectrum.

## Myers (1986) — minimal edit script

> Eugene W. Myers, "An O(ND) Difference Algorithm and Its Variations",
> *Algorithmica* 1 (1986), pp. 251–266.

Myers computes the shortest edit script (insertions + deletions) between two
sequences.  The shortest edit script is the dual of the LCS — the diagonal
"snake" runs are exactly the matched positions.

The algorithm walks the `(m+1)×(n+1)` edit graph by breadth-first search over
the **edit distance**:

- Maintain `V[k]`, the furthest row reached on diagonal `k = x − y` after `D`
  edits; extend each diagonal by a down move (insertion) or a right move
  (deletion), then follow the **snake** — the maximal diagonal run of matches.
- The first `D` whose frontier reaches `(m, n)` is the minimal edit distance.
- Reconstruction walks back across the stored frontiers; each snake contributes
  a run of matched positions, which are the LCS matches.

Minimality is by construction: the search visits distances `0, 1, 2, …` in
order, so the first `D` that reaches the end is the shortest edit script.  The
diagonal-extension tie is fixed (prefer the deletion/right move), so the
reconstructed LCS is deterministic.

Because `D` can be `O(m+n)` for dissimilar input, `Find` is guarded:

1. A **composition pre-filter** lower-bounds `D` from the symbol counts
   (`D ≥ m + n − 2·Σ_s min(count1[s], count2[s])`) in `O(n)`; when that already
   exceeds the budget it aborts without sweeping.
2. A **budget cap** `Analysis::MyersBudget` — the largest `D` whose `O(D²)`
   reconstruction history fits a memory-derived budget — bounds the sweep and
   aborts with a clear diagnostic pointing at `-As`/`-At`.

Length-only (`-l`) uses the prev-only sweep, so its memory is `O(D)` rather than
`O(D²)`; only interval reconstruction materializes the full `V` history.

## Hunt–Szymanski (1977) — threshold array over match lists

> James W. Hunt and Thomas G. Szymanski, "A Fast Algorithm for Computing
> Longest Common Subsequences", *Communications of the ACM* 20(5), 1977,
> pp. 350–353.

Hunt–Szymanski is the classic *sparse* LCS algorithm.  It builds the **match
lists** — for each position `i` in the first sequence, the sorted list of
positions in the second sequence that carry the same symbol — and then walks
those lists maintaining a **threshold array** `T[k]`, the smallest column index
at which a common subsequence of length `k` has been found.

Each matched pair is examined once and either extends an existing subsequence
or opens a new one, via binary search on `T`.  With `r` matched pairs the time
is `O((r + n) log n)`.

The regime is exactly where the other algorithms struggle in reverse: when the
alphabet is large and matches are sparse (`r` small), the match lists are short
and the walk is nearly linear.  When the alphabet is small (e.g. DNA), every
position matches ~`n/4` others, so `r ≈ n²/4` and the algorithm degenerates —
which is why `-Aa` never chooses it there (see `LCS Algorithm Selection.md`).

## RickSpace (2000) — contours + Hirschberg, linear space

> Claus Rick, "Simple and fast linear space computation of longest common
> subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.

RickSpace is the Rick/Hirschberg linear-space refinement.  It reaches the
`O(ns + min{pm, p(n−p)})` time bound while retaining `O(ns)` space, so it never
runs out of memory regardless of input.

### Background

A *match* is an ordered pair `(i, j)` with `A[i] = B[j]`.  A match has *forward
rank* `k` when the longest common prefix length ending at it is `k`, and
*backward rank* `k` when the longest common suffix length beginning at it is
`k`.  A match `(i1, j1)` *dominates* `(i2, j2)` when `i1 ≤ i2` and `j1 ≤ j2`
and they differ.  The `k`-th forward **contour** `FC[k]` is the antichain of
dominant (minimal) rank-`k` matches; `BC[k]` is the symmetric antichain of
maximal ones.

This contour/dominant-match paradigm traces back through Rick (2000): suggested
by Hirschberg (1977), given linear space by Apostolico & Guerra (1985),
improved by Apostolico–Browne–Guerra (1992), and made "flexible" by Rick (1995).
Underlying the dual viewpoint is Dilworth's theorem: the LCS length `p` is at
once the length of the longest chain and the minimum number of antichains
needed to cover the match set.  See `LCS Overview.md` for the fuller history.

### Algorithm

Contours are computed in the alternating order `FC[1], BC[1], FC[2], BC[2], …`.
When the two most recently computed contours first **cross** — no backward match
is strictly below-right of any forward match — the LCS length is `p = f + b − 1`,
and a midpoint of some LCS is any forward match having a backward match
non-strictly below-right of it.

`Hirschberg` then recurses on the prefix and suffix separated by that midpoint.
Only the two most recent contours of each direction are retained, so the
working set is linear.  Rick's early-termination rule stops the recursion when
a sub-problem's LCS length equals its shorter string's length, emitting the
greedy matches directly — which prunes the recursion tree on near-identical
inputs.

Four refinements from the paper are implemented: `O(p)` contour scans (merged,
not quadratic), Lemma-5 contour cutting, the dual diagonal sweep (row + column
interleaved), and completeness detection (each contour's work is capped at
`O(min{m, n−p})`).

### Input

Occurrence queries are answered in `O(1)` from dense `next`/`prev` tables built
over *both* strings (the `O(ns)` preprocessing):

- **Character path (`-T -As`)** builds the tables directly from the code points
  in `O(s·(m+n))`; no match lists are materialized, so it stays feasible even
  when `r` would be quadratic.
- **Record path (`-As`)** reuses the shared `Match()` output, compressing
  symbols by match-list pointer identity; tables cost `O(s·(m+n) + r)`.

## RickTime (1995) — diagonal sweep, dual thresholds

> Claus Rick, "A new flexible algorithm for the longest common subsequence
> problem", *Proc. CPM'95*, Lecture Notes in Computer Science 937, Springer,
> 1995, pp. 340–351.

RickTime is Rick's *flexible* algorithm — the time-optimal sibling of RickSpace:
it trades the linear-space guarantee for the tighter `p(n−p)` side of the bound
at `O(ns + d)` space, where `d` is the number of dominant matches.

The method is Rick's *diagonal sweep* (Algorithm 2 in the paper):

- Slide down the diagonal `l = 0…m−1`, processing **row `l`** (matches `(l, j)`
  with `j ≥ l`) and **column `l`** (matches `(i, l)` with `i > l`) together.
- Keep **two** threshold arrays — `rowThresh[k]` (leftmost column of a dominant
  k-match) and `colThresh[k]` (topmost row of one) — instead of a single one.
- A `low` index tracks the smallest class not yet complete; a class is detected
  complete when its threshold equals the diagonal, after which it is never
  examined again — capping each class's work at `~ n−p` steps, the source of the
  `p(n−p)` side of the bound.

Reconstruction records each dominant match with its rank and walks back: each
rank's dominant matches form an antichain, so sorting by ascending `i` forces
descending `j`, and the largest-`i` `(k−1)`-match with `i < current.i` is
necessarily above-left of the current match.

Because `d` can be `Θ(n²)` at intermediate `p`, interval reconstruction records
through `byRank` under a RAM-derived budget (`Analysis::RecordingBudget`) and
aborts cleanly when it would overflow — `-Aa` falls back to `-As` on that
exception.  Length-only (`-l`) skips recording entirely, keeping memory `O(ns)`.

## References

- Eugene W. Myers, "An O(ND) Difference Algorithm and Its Variations",
  *Algorithmica* 1 (1986), pp. 251–266.
- James W. Hunt, Thomas G. Szymanski, "A Fast Algorithm for Computing Longest
  Common Subsequences", *Communications of the ACM* 20(5), 1977, pp. 350–353.
- Claus Rick, "A new flexible algorithm for the longest common subsequence
  problem", *Proc. CPM'95*, LNCS 937, Springer, 1995, pp. 340–351.  Also in
  *Nordic Journal of Computing* 2 (1995), pp. 444–461.
- Claus Rick, "Simple and fast linear space computation of longest common
  subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.
- A. Apostolico, C. Guerra, "A fast linear space algorithm for computing longest
  common subsequences", *Proc. 23rd Allerton Conference*, 1985, pp. 76–84.
- A. Apostolico, S. Browne, C. Guerra, "Fast linear-space computations of longest
  common subsequences", *Theoretical Computer Science* 92 (1992), pp. 3–17.
- D. S. Hirschberg, "Algorithms for the longest common subsequence problem",
  *Journal of the ACM* 24(4), 1977, pp. 664–675.
- H. Goeman, M. Clausen, "A new practical linear space algorithm for the longest
  common subsequence problem", *Kybernetika* 38(1), 2002, pp. 45–66.
- R. P. Dilworth, "A Decomposition Theorem for Partially Ordered Sets", *Annals
  of Mathematics* 51 (1950), pp. 161–166.
