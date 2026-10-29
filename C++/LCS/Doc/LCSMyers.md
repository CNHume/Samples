# LCSMyers — Myers (1986) minimal edit script

An implementation of the Longest Common Subsequence (LCS) problem based on:

> Eugene W. Myers, "An O(ND) Difference Algorithm and Its Variations",
> *Algorithmica* 1 (1986), pp. 251–266.

`LCSMyers` computes the shortest edit script (insertions + deletions) between
two sequences.  The shortest edit script is the dual of the LCS — the diagonal
"snake" runs are exactly the matched positions — and the classic algorithm
finds it in `O(ND)` time, where `D` is the edit distance.  In `compare` it is
selected with `-ad` (the edit-distance case of the `-a` algorithm switch).

## Relationship to the other classes

```
LCS                        shared typedefs and Algorithm enum
├── LCSHuntSzymanski       threshold array over match lists  — O((r+n) log n)
├── LCSRickSpace           contours + Hirschberg             — O(ns + min{pm, p(n−p)}), linear space
├── LCSRickTime            diagonal sweep, dual thresholds   — O(ns + min{pm, p(n−p)})
└── LCSMyers               edit-graph D-path sweep          — O(ND)
```

Myers is parameterized by the *edit distance* `D = n + m − 2p`, not by the LCS
length `p` or the alphabet size `s`.  It is therefore the fastest choice when
`D` is small — the near-identical regime — and quadratic (`D ≈ n+m`) when the
inputs are dissimilar.  The other three algorithms are parameterized by `p`
and `s` and stay bounded across the whole similarity spectrum, which is why
they remain the general-purpose choices.

## Algorithm

Myers walks the `(n+1)×(m+1)` edit graph by breadth-first search over the
**edit distance**:

- Maintain `V[k]`, the furthest row reached on diagonal `k = x − y` after `D`
  edits; extend each diagonal by a down move (insertion) or a right move
  (deletion), then follow the **snake** — the maximal diagonal run of matches.
- The first `D` whose frontier reaches `(n, m)` is the minimal edit distance.
- Reconstruction walks back across the stored frontiers; each snake contributes
  a run of matched positions, which are the LCS matches.

Minimality is by construction: the search visits distances `0, 1, 2, …` in
order, so the first `D` that reaches the end is the shortest edit script.  The
diagonal-extension tie is fixed (prefer the deletion/right move), so the
reconstructed LCS is deterministic.

## Guard

Because `D` can be `O(n+m)` for dissimilar input, `Find` is guarded before it
wastes time:

1. A **composition pre-filter** lower-bounds `D` from the symbol counts
   (`D ≥ n + m − 2·Σ_s min(count1[s], count2[s])`) in `O(n)`; when that already
   exceeds the budget it aborts without sweeping.
2. A **budget cap** `Diagnostic::MyersBudget` — the largest `D` whose `O(D²)`
   reconstruction history fits a memory-derived budget — bounds the sweep and
   aborts with a clear diagnostic pointing at `-as`/`-at`.

The near-identical regime (`D` small) needs only a few hundred kilobytes of
history; the budget is the safety net that keeps a dissimilar input from
spending `O(D²)` time and memory.

## Selection

In `compare`, `-ad` selects `LCSMyers` for both the character (code-point) path
(`-T -ad`) and the default record/word path.  On the record path each record is
normalized for case/space before comparison; normalization preserves record
indices, so the matched positions map 1:1 back to the input records.

## References

- Eugene W. Myers, "An O(ND) Difference Algorithm and Its Variations",
  *Algorithmica* 1 (1986), pp. 251–266.
