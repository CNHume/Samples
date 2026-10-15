# LCSRickTime — Rick (1995) flexible LCS

An implementation of the Longest Common Subsequence (LCS) problem based on:

> Claus Rick, "A new flexible algorithm for the longest common subsequence
> problem", *Proc. CPM'95*, Lecture Notes in Computer Science 937, Springer,
> 1995, pp. 340–351.

`LCSRickTime` is Rick's *"flexible"* algorithm — efficient for short,
intermediate, and long LCS.  It reaches the `O(ns + min{pm, p(n−p)})` time
bound at `O(ns + d)` space, where `d` is the number of dominant matches.  In
`compare` it is selected with `-at` (the "time" case of the `-a` algorithm
switch).

## Relationship to the other classes

```
LCS                        shared typedefs and Algorithm enum
├── LCSHuntSzymanski       threshold array over match lists  — O((r+n) log n)
├── LCSRickSpace           contours + Hirschberg             — O(ns + pm), linear space
└── LCSRickTime            diagonal sweep, dual thresholds   — O(ns + min{pm, p(n−p)})
```

`LCSRickTime` is the time-optimal sibling of `LCSRickSpace`: it trades the
linear-space guarantee for the tighter `p(n−p)` time bound.  Because `d` can be
`Θ(n²)` at intermediate `p` (≈ 0.5–0.7·n for random DNA), `LCSRickSpace` remains
the right default for very long strings.

## Algorithm

The method is Rick's *diagonal sweep* (Algorithm 2 in the paper) — "a kind of
dualization" of Apostolico–Guerra:

- Slide down the diagonal `l = 0…m−1`, processing **row `l`** (matches `(l, j)`
  with `j ≥ l`) and **column `l`** (matches `(i, l)` with `i > l`) together.
- Keep **two** threshold arrays — `rowThresh[k]` (leftmost column of a dominant
  k-match) and `colThresh[k]` (topmost row of one) — instead of the single
  `Thresh` of the row-by-row form.
- A `low` index tracks the smallest class not yet complete.  Class `C_k` is
  detected complete when `rowThresh[k] == l` or `colThresh[k] == l`, after which
  it is never examined again; this caps each class's work at `~ n−p` steps,
  which is the source of the `p(n−p)` side of the bound.

`Find` records each dominant match together with its rank and reconstructs an
LCS by a backward walk: each rank's dominant matches form an antichain, so
sorting by ascending `i` forces descending `j`, and the largest-`i`
`(k−1)`-match with `i < current.i` is necessarily above-left of the current
match.  `FindFlexibleLength` is the length-only variant (it skips recording).

## Time and space

| | Time | Space |
|---|---|---|
| `LCSRickTime` | `O(ns + min{pm, p(n−p)})` | `O(ns + d)` |
| `LCSRickSpace` (contrast) | `O(ns + pm)` | `O(ns)`, linear |

## Selection

In `compare`, `-at` selects `LCSRickTime` for the character (code-point) path
(`-t -at`).  The record/word path does not yet support it (it throws), because
the record alphabet (strings) would need its own symbol→id compression.

## References

- Claus Rick, "A new flexible algorithm for the longest common subsequence
  problem", *Proc. CPM'95*, Lecture Notes in Computer Science 937, Springer,
  1995, pp. 340–351.  Also in *Nordic Journal of Computing* 2 (1995),
  pp. 444–461.
- A. Apostolico, C. Guerra, "A fast linear space algorithm for computing longest
  common subsequences", *Proc. 23rd Allerton Conference*, 1985, pp. 76–84.
- A. Apostolico, S. Browne, C. Guerra, "Fast linear-space computations of
  longest common subsequences", *Theoretical Computer Science* 92 (1992),
  pp. 3–17.
- R. P. Dilworth, "A Decomposition Theorem for Partially Ordered Sets",
  *Annals of Mathematics* 51 (1950), pp. 161–166.
