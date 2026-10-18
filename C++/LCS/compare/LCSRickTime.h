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
// 2026-10-15 CNHume  Created LCSRickTime class
//
// Rick (1995) flexible LCS algorithm: a diagonal sweep that computes dominant
// matches row-by-row and column-by-column with two threshold arrays.
//
// It reaches the O(ns + min{pm, p(n-p)}) time bound, at O(ns + d) space,
// where d is the number of dominant matches.
//
// Based on:
//   Claus Rick, "A new flexible algorithm for the longest common subsequence
//   problem", Proc. CPM'95, LNCS 937, Springer, 1995, pp. 340-351.
//
#pragma once

#include "LCS.h"                        // for LCS base, Pair

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for u32string

using namespace std;

class LCSRickTime : public LCS {
public:
  //
  // Compute the LCS length and, when pairs is non-null, the matched positions
  // as a linked list of (begin1, begin2) Pairs in ascending order.
  //
  //[Note]The caller should guarantee |s1| <= |s2| by swapping when necessary.
  //
  static uint32_t Find(
    shared_ptr<Pair>* pairs, const u32string& s1, const u32string& s2);
  // Length only.
  static uint32_t FindFlexibleLength(const u32string& s1, const u32string& s2);
};
