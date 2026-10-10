# LCSRick — Rick (2000) linear-space LCS

An implementation of the Longest Common Subsequence (LCS) problem based on:

> Claus Rick, "Simple and fast linear space computation of longest common
> subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.

`LCSRick` is the Rick/Hirschberg refinement integrated into the `compare`
application.  It is the run-time alternative to `LCSHuntSzymanski`, selected
with the `-r` switch in `-t` (text) mode.  (`delta` continues to use only the
Hunt–Szymanski algorithm.)

## Relationship to the other classes

```
LCS                        shared typedefs: PAIRS, INDEXES,
                           STRING_TO_INDEXES_MAP, MATCHES
├── LCSHuntSzymanski       Find(pairs, matches)          — threshold array
└── LCSRick                Find(pairs, matches, m, n)    — contours + Hirschberg
```

Both algorithm classes derive publicly from `LCS` and consume the **same**
match structure built by `Match()`.  Their `Find()` methods return the LCS
length and fill `*pairs` with matched positions in ascending order, so the
downstream `Delta::Coalesce` / `Context` / `Complement` / display pipeline is
identical regardless of which algorithm ran.

## Input: the shared `Match()` output

`Match()` builds `MATCHES = deque<INDEXES*>`, where `*matches[i]` is the sorted
list of positions `j` in the second sequence matching position `i` in the
first.  `LCSRick::Find()` uses these lists directly, answering each occurrence
query with `std::lower_bound` / `std::upper_bound`.

This is the principal difference from the standalone `Rick/` reference
implementation, which precomputes O(1) `next`/`prev` occurrence tables
(the "LeftPos"/"TopPos" preprocessing) at `O(s·n)` time and space per call,
indexed by a symbol→id compression.  Reusing the match lists removes the
symbol→id compression and the occurrence tables entirely — no symbol type is
needed — at the cost of an `O(log r)` factor per occurrence query.  If the
tighter `min{pm, p(n−p)}` bound becomes important, the O(1) tables can be
reintroduced as a pre-pass over the same match lists without changing
`Find()`'s interface.

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

Two refinements from the paper are implemented:

- **Lemma 5 contour cutting** — before extending `FC[k]`, forward sources that
  have no backward-contour match strictly below-right (and so can only reach
  matches whose rank sum cannot reach the LCS) are pruned; symmetrically for
  backward sources.
- **Row-range skipping** — a forward contour of rank `f` scans only rows
  `≤ m − b` (a match of backward rank `≥ b` needs `b − 1` further rows below
  it), and a backward contour of rank `b` scans only rows `≥ b − 1`.  This
  tightens the contour work from `O(p·m)` toward `O(p·(m − p/2))`.

## Reconstruction

`LCSRick::Hirschberg` recurses over full-string index ranges `[i0,i1) × [j0,j1)`
and emits each matched position as an `(index1, index2)` `Pair`, in ascending
order.  Because full-string coordinates are used, no offset bookkeeping is
required, and the emitted pairs feed `Delta::Coalesce` unchanged.

## Orientation

Rick's time bound assumes `m ≤ n` (the shorter sequence is scanned, the longer
is tabled).  `LCSString::Compare` therefore swaps the inputs when necessary
before calling `Match()`, and un-swaps the reported `begin1`/`begin2` positions
on the resulting pairs.  `LCSRick::Find()` itself is orientation-agnostic.

## Selection

In `compare`, `-t -r` selects `LCSRick::Find` for the character (code-point)
path; omitting `-r` selects `LCSHuntSzymanski::Find`.  The record/word path
ignores `-r`.

## Build and test

C++20 (uses `std::format`, `std::ssize`, and designated initializers).  From
`../compare`:

```sh
g++ -std=c++20 -O2 -Wall -Wextra *.cpp -o compare

# Difference (Hunt vs. Rick produce identical output):
compare -t    file1 file2
compare -t -r file1 file2

# Correspondence:
compare -t -r -c file1 file2
```

Correctness was verified against an O(m·n) dynamic-programming reference and
against `LCSHuntSzymanski::Find` on over 220,000 fixed and randomized cases.

## References

- Claus Rick, "Simple and fast linear space computation of longest common
  subsequences", *Information Processing Letters* 75 (2000), pp. 275–281.
- Daniel S. Hirschberg, "A linear space algorithm for computing maximal common
  subsequences", *Communications of the ACM* 18(6), 1975, pp. 341–343.
