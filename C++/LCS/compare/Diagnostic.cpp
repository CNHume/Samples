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

#if defined(_WIN32)
  #define NOMINMAX
  #include <windows.h>                  // for GlobalMemoryStatusEx
#elif defined(__unix__) || defined(__APPLE__) || defined(__linux__)
  #include <unistd.h>                   // for sysconf
#endif

namespace {
  //
  // Warn when the match count exceeds this percentage of the full m x n
  // matching matrix.  Hunt-Szymanski is O((r + n) log n), so r near m*n is
  // pathological; the algorithm is meant for the sparse r = O(n) regime.
  //
  constexpr uint64_t matchPercent = 1;

  // Absolute floor below which a match count is too small to matter.
  constexpr uint64_t matchFloor = 10'000'000;

  // Warn when the estimated RickTime table footprint exceeds this byte budget.
  constexpr uint64_t tableByteBudget = 1ull << 30;   // 1 GB

  // Stop RickTime's recording before it fills this fraction of available RAM.
  constexpr double recordingMargin = 0.8;

  // A recorded dominant match is an 8-byte DomMatch, but byRank's vector
  // capacity growth (up to 2x) and per-rank overhead make ~16 bytes per match
  // the safe estimate; each rank also carries a 24-byte vector object.
  constexpr uint64_t recordBytes = 16;
  constexpr uint64_t vectorHeaderBytes = 24;

  //
  // Near-identical: at most this many edits (min(m, n) - p),
  // and no more than 1% of the shorter string.
  //
  constexpr uint64_t nearIdenticalEdits = 1000;

  //
  // Available physical memory — the working-set ceiling before the OS starts
  // killing (Linux OOM) or paging (Windows) — with a conservative fallback.
  //
  uint64_t availableBytes() {
#if defined(_WIN32)
    MEMORYSTATUSEX mem{};
    mem.dwLength = sizeof(mem);
    if (GlobalMemoryStatusEx(&mem))
      return mem.ullAvailPhys;
#elif defined(_SC_AVPHYS_PAGES)
    auto pages = sysconf(_SC_AVPHYS_PAGES);
    auto pageSize = sysconf(_SC_PAGESIZE);
    if (pages > 0 && pageSize > 0)
      return (uint64_t)pages * (uint64_t)pageSize;
#endif
    return 15ull << 30;                 // conservative fallback: 15 GB
  }
}

void Diagnostic::WarnMatchCount(uint64_t matches, uint64_t m, uint64_t n) {
  auto threshold = m * n * matchPercent / 100;
  if (threshold < matchFloor)
    threshold = matchFloor;
  if (matches > threshold) {
    std::cerr << std::format(
      "Warning: Hunt-Szymanski will process {} matches over a {}x{} input, "
      "which is pathological; consider -at (time) or -as (space)",
      matches, m, n) << std::endl;
  }
}

void Diagnostic::WarnTableSize(uint64_t m, uint64_t n, uint64_t symbols) {
  // next1 and next2 hold symbols x (length + 2) int64_t entries each.
  auto bytes = 8 * symbols * (m + n + 4);
  if (bytes > tableByteBudget) {
    std::cerr << std::format(
      "Warning: RickTime occurrence tables need ~{} bytes for {} symbols "
      "over a {}x{} input; consider -as (linear space)",
      bytes, symbols, m, n) << std::endl;
  }
}

uint64_t Diagnostic::RecordingBudget(uint64_t m, uint64_t n, uint64_t symbols) {
  //
  // next1/next2 are uint32_t tables; byRank is m+2 vector<DomMatch> objects
  // plus the recorded matches.  Reserve a safety margin of the available RAM.
  //
  auto tableBytes = 4 * symbols * (m + n + 4);
  auto headerBytes = vectorHeaderBytes * (m + 2);
  auto budgetBytes = (uint64_t)((double)availableBytes() * recordingMargin);
  auto room = budgetBytes > tableBytes + headerBytes ?
    budgetBytes - tableBytes - headerBytes : 0;
  return room / recordBytes;
}

void Diagnostic::WarnRecordingSize(uint64_t records, uint64_t recordBudget) {
  std::cerr << std::format(
    "Warning: RickTime match recording needs ~{} bytes, "
    "exceeding the ~{} byte budget; use -as (RickSpace) for linear memory instead",
    records * recordBytes, recordBudget * recordBytes) << std::endl;
}

void Diagnostic::SuggestRickTime(uint32_t length, uint64_t shorter) {
  auto edits = shorter - length;
  if (edits <= nearIdenticalEdits && edits * 100 <= shorter) {
    std::cerr << std::format(
      "Note: near-identical input (LCS {} of {}); "
      "-at (RickTime) would likely fit and run ~2.6x faster",
      length, shorter) << std::endl;
  }
}
