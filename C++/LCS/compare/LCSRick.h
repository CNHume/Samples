// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-09 CNHume  Created LCSRick class
//
// Rick (2000) linear-space LCS, adapted to reuse the Hunt-Szymanski
// Match() output.  The contour scans query the sorted match lists with
// binary search instead of building O(1) next/previous occurrence tables.
//
// Based on:
//   Claus Rick, "Simple and fast linear space computation of longest common
//   subsequences", Information Processing Letters 75 (2000), pp. 275-281.
//
#pragma once

#include "LCS.h"                        // for MATCHES, Pair

#include <cstdint>                      // for int64_t, uint32_t
#include <memory>                       // for shared_ptr
#include <vector>

using namespace std;

class LCSRick : public LCS {
public:
  // Find the LCS length and matched positions of the sub-problem
  // s1[0:size1] x s2[0:size2], where matches[i] is the sorted list of
  // positions in s2 matching s1[i].  On return, *pairs (when non-null) is
  // a linked list of matched (begin1, begin2) positions in ascending order.
  static uint32_t Find(shared_ptr<Pair>* pairs,
    const MATCHES& matches, uint32_t size1, uint32_t size2);

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

  static Result FindMidpoint(const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static void Hirschberg(const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1, vector<Match>& out);

  // Lemma 5 contour cutting: retain only the sources that can still extend
  // an LCS (the ones "uncovered" by the opposite contour).
  static Contour CutForward(const Contour& forward, const Contour& backward);
  static Contour CutBackward(const Contour& backward, const Contour& forward);

  static Contour FirstForward(const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static Contour NextForward(const Contour& contour, const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1, int64_t rowLimit);
  static Contour FirstBackward(const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1);
  static Contour NextBackward(const Contour& contour, const MATCHES& matches,
    size_t i0, size_t i1, size_t j0, size_t j1, int64_t rowFloor);
  static bool Crossed(const Contour& forward, const Contour& backward);
  static Match Midpoint(const Contour& forward, const Contour& backward);
};
