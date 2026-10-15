// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-09 CNHume  Created LCSRick class
//
// Rick (2000) linear-space LCS.  The contour scans query O(1) next/previous
// occurrence tables ("LeftPos"/"TopPos" preprocessing) built either directly
// from code-point strings (the character path) or from the shared
// Hunt-Szymanski match lists (the record path).
//
// Based on:
//   Claus Rick, "Simple and fast linear space computation of longest common
//   subsequences", Information Processing Letters 75 (2000), pp. 275-281.
//
#pragma once

#include "LCS.h"                        // for MATCHES, Pair

#include <cstdint>                      // for int64_t, uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for u32string
#include <vector>

using namespace std;

class LCSRick : public LCS {
public:
  // Find the LCS length and matched positions.  On return, *pairs (when
  // non-null) is a linked list of matched (begin1, begin2) positions in
  // ascending order.  The match-list overload serves the record path; the
  // code-point overload builds O(ns) tables directly from the strings and
  // serves the character (-t -r) path.  Both assume size1 <= size2.
  static uint32_t Find(shared_ptr<Pair>* pairs,
    const MATCHES& matches, uint32_t size1, uint32_t size2);
  static uint32_t Find(shared_ptr<Pair>* pairs,
    const u32string& s1, const u32string& s2);

private:
  struct Match {
    int64_t index1 = 0;
    int64_t index2 = 0;
  };

  struct Result {
    bool hasMidpoint = false;
    Match midpoint;
  };

  // A contour is an antichain of dominant matches sorted by ascending
  // index1, which forces descending index2.
  typedef vector<Match> Contour;

  // O(1) next/previous occurrence tables.  rowId[i] is the symbol id of row i
  // (-1 when the symbol never occurs); next[id][j] is the smallest position
  // >= j holding symbol id (n if none); prev[id][j] is the largest position
  // <= j holding it (-1 if none).  This is the "LeftPos/TopPos" O(ns)
  // preprocessing of the Rick contour scan.
  struct Occurrences {
    vector<int64_t> rowId;             // symbol id of each row, [0, size1)
    vector<vector<int64_t>> next;      // [id][j], j in [0, n]
    vector<vector<int64_t>> prev;      // [id][j], j in [0, n)
    int64_t n = 0;                     // size2
  };

  // Record path: compress distinct match-list pointers (two rows sharing a
  // symbol share a pointer) and build tables from the sorted position lists.
  static Occurrences BuildOccurrences(const MATCHES& matches,
    size_t size1, size_t size2);
  // Character path: compress the code points of s2 (the longer string) and
  // build tables directly, never materializing match lists.
  static Occurrences BuildOccurrences(const u32string& s1, const u32string& s2);

  static uint32_t Solve(shared_ptr<Pair>* pairs, const Occurrences& occ,
    size_t size1, size_t size2);

  static Result FindMidpoint(const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static void Hirschberg(const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1, vector<Match>& out);

  // Lemma 5 contour cutting: retain only the sources that can still extend
  // an LCS (the ones "uncovered" by the opposite contour).
  static Contour CutForward(const Contour& forward, const Contour& backward);
  static Contour CutBackward(const Contour& backward, const Contour& forward);

  static Contour FirstForward(const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static Contour NextForward(const Contour& contour, const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1, int64_t rowLimit);
  static Contour FirstBackward(const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static Contour NextBackward(const Contour& contour, const Occurrences& occ,
    size_t i0, size_t i1, size_t j0, size_t j1, int64_t rowFloor);
  static bool Crossed(const Contour& forward, const Contour& backward);
  static Match Midpoint(const Contour& forward, const Contour& backward);
};
