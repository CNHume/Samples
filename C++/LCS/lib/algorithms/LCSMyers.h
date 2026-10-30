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
// 2026-10-27 CNHume  Created Myers class
//
// Myers (1986) minimal edit-script algorithm, run over code points (text) or
// records (lines/words).  The shortest edit script (insertions + deletions) is
// the dual of the LCS: the diagonal "snake" runs are exactly the matched
// positions.  It is the near-identical fast path, guarded by a memory-derived
// D budget.
//
// Based on:
//   Eugene W. Myers, "An O(ND) Difference Algorithm and Its Variations",
//   Algorithmica 1 (1986), pp. 251-266.
//
#pragma once

#include "LCS.h"                        // for LCS base, Pair
#include "Statistics.h"                 // for Statistics

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for u32string
#include <vector>                       // for vector

namespace lcs::algorithms {

  class Myers {
  public:
    //
    // Compute the LCS length and, when pairs is non-null, the matched positions
    // as a linked list of (begin1, begin2) Pairs in ascending order.
    //
    //[Note]Myers is symmetric in its two strings, so no |s1| <= |s2| swap is
    // required.  It throws when the edit distance exceeds the memory-derived
    // budget rather than spending O(N*D) time on a dissimilar input.
    //
    static std::uint32_t Find(
      std::shared_ptr<Pair>* pairs, Statistics* statistics,
      const std::u32string& s1, const std::u32string& s2);
    //
    // Record path: compare sequences of records.  Records must already be
    // normalized for case/space; indices map 1:1 to the input records.
    //
    static std::uint32_t Find(
      std::shared_ptr<Pair>* pairs, Statistics* statistics,
      const std::vector<std::string>& r1, const std::vector<std::string>& r2);
  };

} // namespace lcs::algorithms
