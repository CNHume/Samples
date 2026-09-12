# Rick (2000) LCS

A C++ implementation of the Longest Common Subsequence (LCS) problem based on:

> Claus Rick, "Simple and fast linear space computation of longest common
> subsequences", *Information Processing Letters* 75 (2000), pp. 275-281.

This project is a standalone companion to the Hunt-Szymanski implementation in
`../compare`, `../delta` and `../LCSDemo`.

## Status

- [x] Forward/backward contour machinery (`FC`/`BC`).
- [x] LCS length and midpoint (`LCS::Length`, `LCS::FindMidpoint`).
- [x] Full linear-space construction (`LCS::Correspondence`, Hirschberg-style recursion).
- [x] Lemma 5 "contour cutting" source pruning (instrumented via `LCS::Candidates`).
- [x] Row-range skipping in the contour scans (instrumented via `LCS::Rows`).

## Algorithm

Each match `(i, j)` has a *forward rank* (longest common prefix length ending
at it) and a *backward rank* (longest common suffix length beginning at it).
The `k`-th forward contour `FC[k]` and backward contour `BC[k]` are the
antichains of *dominant* rank-`k` matches.

Contours are computed in an alternating order `FC[1], BC[1], FC[2], BC[2], ...`.
When the two most recently computed contours first cross, the LCS length is
`p = f + b - 1` and a shared (or touching) match is a midpoint of some LCS
(Rick, Lemmas 2 and 3).  `Correspondence` recurses on the prefix and suffix
separated by that midpoint, in the manner of Hirschberg.  Only the two most
recent contours of each direction are retained, so the working set is linear.

Per Lemma 5, each contour is "cut": only forward matches still uncovered by
the backward contours (those with a backward-contour match strictly below-
right) are extended, and symmetrically for backward matches.  Sources that
could only produce matches whose rank sum cannot reach the LCS are pruned.

The contour scans also skip whole row ranges: a forward contour of rank `f`
only scans rows `<= m - b` (a match of backward rank `>= b` needs `b - 1`
further rows below it), and a backward contour of rank `b` only scans rows
`>= b - 1`.  This tightens the contour work from `O(p*m)` toward
`O(p*(m - p/2))`, approaching the paper's `min{pm, p(n-p)}` bound.

## Build and test

C++20 and any recent compiler (the code uses `std::ssize`, `std::format`,
and designated initializers).  From this directory:

```sh
# Demo: print the LCS length, the LCS, and a midpoint.
g++ -std=c++20 -O2 -Wall -Wextra LCS.cpp main.cpp -o rick
./rick thisisatest testing123testing

# Verification: cross-check against a DP reference and Hunt-Szymanski.
g++ -std=c++20 -O2 -Wall -Wextra LCS.cpp verify.cpp -o verify
./verify
```
