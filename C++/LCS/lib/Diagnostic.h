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
// 2026-10-16 CNHume  Created Diagnostic class
//
#pragma once

#include "LCS.h"                        // for Algorithm

#include <cstdint>                      // for uint64_t
#include <string>                       // for string

namespace lcs {

  //
  // Diagnostic reports warnings when an LCS algorithm is about to hit a
  // pathological case, so users can switch to a more suitable algorithm
  // before paying the cost.
  //
  class Diagnostic {
  public:
    // Hunt-Szymanski degenerates when the match count r is large relative
    // to the full m x n matching matrix, since it runs in O((r + n) log n).
    static void WarnMatchCount(std::uint64_t matches, std::uint64_t m, std::uint64_t n);
    // RickTime allocates two dense s x length occurrence tables.
    static void WarnTableSize(std::uint64_t m, std::uint64_t n, std::uint64_t symbols);
    // Maximum dominant matches RickTime may record before its byRank recording,
    // added to the dense tables, would approach available RAM.
    static std::uint64_t RecordingBudget(std::uint64_t m, std::uint64_t n, std::uint64_t symbols);
    // Warn that RickTime's match recording has exceeded its budget.
    static void WarnRecordingSize(std::uint64_t records, std::uint64_t recordBudget);
    // Suggest RickTime when RickSpace's result is near-identical (few edits)
    // and its dominant-match recording would fit in memory, the regime where
    // RickTime is ~2.6x faster.
    static void SuggestRickTime(
      std::uint32_t length, std::uint64_t shorter, std::uint64_t longer,
      std::uint64_t candidates, std::uint64_t symbols);
    // Myers runs in O(N*D); its reconstruction stores the V frontier for every
    // D, an O(D^2) int64 history.  Return the largest D whose history fits a
    // memory-derived budget (also bounding the O(D^2) time).
    static std::uint64_t MyersBudget(std::uint64_t m, std::uint64_t n);
    // Warn that Myers' edit distance exceeded its budget.
    static void WarnMyersDistance(std::uint64_t distance, std::uint64_t budget);
    //
    // Automatic (-aa) dispatch: select the algorithm from the three metrics and
    // record the reason in `reason` for verbose output.
    //
    static Algorithm SelectAutomatic(
      std::string& reason,
      std::uint64_t m, std::uint64_t n,
      std::uint64_t matches, std::uint64_t symbols,
      std::uint64_t editLowerBound);
  };

} // namespace lcs
