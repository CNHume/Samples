// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-16 CNHume  Created Diagnostic class
//
#pragma once

#include <cstddef>                      // for size_t
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
