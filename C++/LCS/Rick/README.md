# Rick (2000) LCS

A C++ implementation of the Longest Common Subsequence (LCS) problem based on:

> Claus Rick, "Simple and fast linear space computation of longest common
> subsequences", *Information Processing Letters* 75 (2000), pp. 275-281.

This project is a standalone companion to the Hunt-Szymanski implementation in
`../compare`, `../delta` and `../LCSDemo`.

## Status

- [x] Forward/backward contour machinery (`FC`/`BC`).
- [x] LCS length and midpoint (`LCS::Length`, `LCS::FindMidpoint`).
- [ ] Full linear-space construction (Hirschberg-style recursion at the midpoint).
- [ ] Lemma 5 "contour cutting" speedup and linear-time contour scans.

## Algorithm

Each match `(i, j)` has a *forward rank* (longest common prefix length ending
at it) and a *backward rank* (longest common suffix length beginning at it).
The `k`-th forward contour `FC[k]` and backward contour `BC[k]` are the
antichains of *dominant* rank-`k` matches.

Contours are computed in an alternating order `FC[1], BC[1], FC[2], BC[2], ...`.
When the two most recently computed contours first cross, the LCS length is
`p = f + b - 1` and a shared (or touching) match is a midpoint of some LCS
(Rick, Lemmas 2 and 3).  The full LCS is then obtained by recursing on the
prefix and suffix separated by that midpoint, in the manner of Hirschberg.

## Build and test

C++17 and any recent compiler.  From this directory:

```sh
# Demo: print the LCS length and a midpoint.
g++ -std=c++17 -O2 -Wall -Wextra LCS.cpp main.cpp -o rick
./rick thisisatest testing123testing

# Verification: cross-check against a DP reference and Hunt-Szymanski.
g++ -std=c++17 -O2 -Wall -Wextra LCS.cpp verify.cpp -o verify
./verify
```
