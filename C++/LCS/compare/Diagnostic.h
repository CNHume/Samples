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

#include <cstdint>                      // for uint64_t

//
// Diagnostic reports warnings when an LCS algorithm is about to hit a
// pathological case, so users can switch to a more suitable algorithm
// before paying the cost.
//
class Diagnostic {
public:
  // Hunt-Szymanski degenerates when the match count r is large relative to
  // the full m x n matching matrix, since it runs in O((r + n) log n).
  static void WarnMatchCount(uint64_t matches, uint64_t m, uint64_t n);
  // RickTime allocates two dense s x length occurrence tables.
  static void WarnTableSize(uint64_t m, uint64_t n, uint64_t symbols);
};
