// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2026-10-09 CNHume  Created LCSRickSpace class
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
#include "Statistics.h"                 // for Statistics

#include <cstdint>                      // for uint32_t, uint64_t
#include <memory>                       // for shared_ptr
#include <string>                       // for u32string

using namespace std;

class LCSRickSpace : public LCS {
public:
  //
  // Find the LCS length and matched positions.  On return, *pairs (when
  // non-null) is a linked list of matched (begin1, begin2) positions in
  // ascending order.  The match-list overload serves the record path; the
  // code-point overload builds O(ns) tables directly from the strings and
  // serves the character (-t -r) path.  Both assume size1 <= size2.
  //
  static uint32_t Find(
    shared_ptr<Pair>* pairs, Statistics* statistics,
    const MATCHES& matches, uint32_t size1, uint32_t size2);
  static uint32_t Find(
    shared_ptr<Pair>* pairs, Statistics* statistics,
    const u32string& s1, const u32string& s2);

  //
  // Diagnostic counters (since the last ResetCounters()): the number of
  // dominant matches generated (Candidates), the number of diagonal sweep
  // steps (Rows), and the number of contour sweeps run (Contours).  They
  // witness the narrowing of the contour scans toward the O(p(n-p)) bound.
  //
  static uint64_t Candidates;
  static uint64_t Rows;
  static uint64_t Contours;
  static void ResetCounters();
};
