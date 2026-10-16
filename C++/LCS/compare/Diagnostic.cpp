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
#include "Diagnostic.h"

#include <format>                       // for format
#include <iostream>                     // for cerr

namespace {
  // Warn when the match count exceeds this percentage of the full m x n
  // matching matrix.  Hunt-Szymanski is O((r + n) log n), so r near m*n is
  // pathological; the algorithm is meant for the sparse r = O(n) regime.
  constexpr uint64_t matchPercent = 1;

  // Absolute floor below which a match count is too small to matter.
  constexpr uint64_t matchFloor = 10'000'000;

  // Warn when the estimated RickTime table footprint exceeds this byte budget.
  constexpr uint64_t tableByteBudget = 1ull << 30;   // 1 GiB
}

void Diagnostic::WarnMatchCount(uint64_t matches, uint64_t m, uint64_t n) {
  auto threshold = m * n * matchPercent / 100;
  if (threshold < matchFloor)
    threshold = matchFloor;
  if (matches > threshold) {
    std::cerr << std::format(
      "warning: Hunt-Szymanski will process {} matches over a {}x{} input, "
      "which is pathological; consider -at (time) or -as (space)\n",
      matches, m, n);
  }
}

void Diagnostic::WarnTableSize(uint64_t m, uint64_t n, uint64_t symbols) {
  // next1 and next2 hold symbols x (length + 2) int64_t entries each.
  auto bytes = 8 * symbols * (m + n + 4);
  if (bytes > tableByteBudget) {
    std::cerr << std::format(
      "warning: RickTime occurrence tables need ~{} bytes for {} symbols "
      "over a {}x{} input; consider -as (linear space)\n",
      bytes, symbols, m, n);
  }
}
