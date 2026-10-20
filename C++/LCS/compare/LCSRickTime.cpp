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
// See LCSRickTime.h for an overview.
//
#include "LCSRickTime.h"
#include "Diagnostic.h"

#include <algorithm>                    // for lower_bound, sort
#include <cstdint>                      // for uint64_t
#include <memory>                       // for make_shared
#include <stdexcept>                    // for runtime_error
#include <unordered_map>                // for symbol-id compression
#include <vector>

using namespace std;

namespace {
  //
  // Dense "next occurrence" tables for both strings, with a shared symbol-id
  // compression over the union of their code points.  This is the O(ns)
  // preprocessing (s = distinct symbols) that the sweep queries in O(1).
  //
  //[Assert]next1[id][i] is the first position >= i in s1 holding symbol id
  // (m if none) and next2[id][j] the first position >= j in s2 (n if none).
  //
  // The extra sentinel column keeps a "first occurrence > j" probe at a length
  // bound within range.
  //
  struct FlexTables {
    vector<uint32_t> id1;               // symbol id of s1[i]
    vector<uint32_t> id2;               // symbol id of s2[j]
    vector<vector<uint32_t>> next1;     // next1[id][j] over s1, j in [0, m]
    vector<vector<uint32_t>> next2;     // next2[id][j] over s2, j in [0, n]
    int64_t m = 0;
    int64_t n = 0;
  };

  // A recorded dominant match (coordinates in the shorter/longer strings).
  struct DomMatch {
    uint32_t i;
    uint32_t j;
  };

  FlexTables BuildTables(const u32string& s1, const u32string& s2) {
    FlexTables t;
    t.m = (int64_t)s1.size();
    t.n = (int64_t)s2.size();

    unordered_map<char32_t, uint32_t> charToId;
    auto idOf = [&](char32_t c) -> uint32_t {
      auto it = charToId.find(c);
      if (it != charToId.end())
        return it->second;
      auto id = (uint32_t)charToId.size();
      charToId.emplace(c, id);
      return id;
    };
    t.id1.assign((size_t)t.m, 0);
    for (int64_t i = 0; i < t.m; i++)
      t.id1[(size_t)i] = idOf(s1[(size_t)i]);
    t.id2.assign((size_t)t.n, 0);
    for (int64_t j = 0; j < t.n; j++)
      t.id2[(size_t)j] = idOf(s2[(size_t)j]);

    auto s = charToId.size();
    //
    // Warn before allocating the dense next tables, reusing the symbol count
    // already gathered above rather than counting distinct symbols again.
    //
    Diagnostic::WarnTableSize((uint64_t)t.m, (uint64_t)t.n, (uint64_t)s);
    //
    // One extra sentinel slot so "first occurrence > j" at j == length (the
    // threshold sentinel) stays in bounds and returns length.
    //
    t.next1.assign(s, vector<uint32_t>((size_t)t.m + 2, (uint32_t)t.m));
    for (int64_t j = t.m - 1; j >= 0; j--) {
      for (size_t id = 0; id < s; id++)
        t.next1[id][(size_t)j] = t.next1[id][(size_t)j + 1];
      t.next1[(size_t)t.id1[(size_t)j]][(size_t)j] = (uint32_t)j;
    }

    t.next2.assign(s, vector<uint32_t>((size_t)t.n + 2, (uint32_t)t.n));
    for (int64_t j = t.n - 1; j >= 0; j--) {
      for (size_t id = 0; id < s; id++)
        t.next2[id][(size_t)j] = t.next2[id][(size_t)j + 1];
      t.next2[(size_t)t.id2[(size_t)j]][(size_t)j] = (uint32_t)j;
    }
    return t;
  }
}

uint32_t LCSRickTime::Find(shared_ptr<Pair>* pairs, Statistics* statistics,
  const u32string& s1, const u32string& s2) {
  auto t = BuildTables(s1, s2);
  auto m = t.m;
  auto n = t.n;

  //
  //[Assert]For each class k, rowThresh[k] is the leftmost column of a dominant
  // k-match found so far (n if none) and colThresh[k] the topmost row (m if
  // none).  A row-l match (l, j) is a new dominant k-match exactly when
  // j < rowThresh[k], and a column-l match (i, l) when i < colThresh[k];
  // assigning one shrinks the threshold toward the diagonal, which keeps each
  // class an antichain.
  //
  // rowThresh[k] = leftmost column of a dominant k-match (n if none);
  // colThresh[k] = topmost row of a dominant k-match (m if none).
  // Sized m+2 so the "low" index may advance one past the last class.
  //
  vector<uint32_t> rowThresh((size_t)m + 2, (uint32_t)n);
  vector<uint32_t> colThresh((size_t)m + 2, (uint32_t)m);
  rowThresh[0] = UINT32_MAX;            // virtual origin C_0 (unused)
  colThresh[0] = UINT32_MAX;
  int64_t low = 1;                      // smallest class not yet complete

  // Recorded dominant matches, grouped by rank.
  auto traceLCS = pairs != nullptr;
  vector<vector<DomMatch>> byRank;
  if (traceLCS)
    byRank.assign((size_t)m + 2, vector<DomMatch>());

  //
  // Bound the match recording by available memory.  The dense next tables are
  // fixed once built, but byRank grows with the number of dominant matches and
  // is what OOMs RickTime, so abort with a clear diagnostic before the OS
  // OOM-killer does.
  //
  uint64_t records = 0;
  uint64_t recordBudget = 0;
  if (traceLCS)
    recordBudget = Diagnostic::RecordingBudget((uint64_t)m, (uint64_t)n, t.next1.size());

  //
  // "Single diagonal sweep": one forward pass over the m diagonals.  At each
  // diagonal l the sweep handles two frontiers in the same pass — row l
  // (dominant matches (l, j) with j >= l, i.e. at/right of the diagonal) and
  // column l (dominant matches (i, l) with i > l, i.e. below the diagonal).
  // There is no separate forward/backward alternation as in Rick (2000); the
  // single pass is possible because each dominant match is found exactly once,
  // via an O(1) next1/next2 occurrence probe.
  //
  for (int64_t l = 0; l < m; l++) {
    //
    //[Assert]On entry to diagonal l every class k < low is complete — its
    // rowThresh/colThresh values are final — because the sweep has already
    // passed the diagonal that could still have produced it.  The row and
    // column sweeps below each advance low by one when their threshold first
    // equals l.
    //
    // Row l: dominant matches (l, j) with j >= l.
    //
    int64_t j;
    if (colThresh[(size_t)low] == l) {
      // Class C_low is complete: resume from its leftmost column.
      j = t.next2[(size_t)t.id1[(size_t)l]][(size_t)(rowThresh[(size_t)low] + 1)];
      low++;
    }
    else {
      j = t.next2[(size_t)t.id1[(size_t)l]][(size_t)l];
    }
    auto k = low;
    while (j != n) {
      if (j < rowThresh[(size_t)k]) {
        auto temp = rowThresh[(size_t)k];
        rowThresh[(size_t)k] = (uint32_t)j;  // dominant k-match (l, j)
        records++;                      // a dominant match was found
        if (traceLCS) {
          byRank[(size_t)k].push_back({ .i = (uint32_t)l, .j = (uint32_t)j });
          if (records > recordBudget) {
            Diagnostic::WarnRecordingSize(records, recordBudget);
            throw runtime_error("LCSRickTime: match recording would exhaust memory; use -as (RickSpace)");
          }
        }
        j = t.next2[(size_t)t.id1[(size_t)l]][(size_t)(temp + 1)];
      }
      if (j == rowThresh[(size_t)k])
        j = t.next2[(size_t)t.id1[(size_t)l]][(size_t)(j + 1)];
      k++;
    }

    //
    // Column l: dominant matches (i, l) with i > l.
    //
    int64_t i;
    if (rowThresh[(size_t)low] == l) {
      // Class C_low is complete: resume from its topmost row.
      i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(colThresh[(size_t)low] + 1)];
      low++;
    }
    else {
      i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(l + 1)];
    }
    k = low;
    while (i != m) {
      if (i < colThresh[(size_t)k]) {
        auto temp = colThresh[(size_t)k];
        colThresh[(size_t)k] = (uint32_t)i;  // dominant k-match (i, l)
        records++;                      // a dominant match was found
        if (traceLCS) {
          byRank[(size_t)k].push_back({ .i = (uint32_t)i, .j = (uint32_t)l });
          if (records > recordBudget) {
            Diagnostic::WarnRecordingSize(records, recordBudget);
            throw runtime_error("LCSRickTime: match recording would exhaust memory; use -as (RickSpace)");
          }
        }
        i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(temp + 1)];
      }
      if (i == colThresh[(size_t)k])
        i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(i + 1)];
      k++;
    }
  }

  //
  // LCS length = number of non-empty classes.  A class may hold dominant matches
  // only above/below the diagonal, so either threshold array can witness it.
  //
  uint32_t p = 0;
  for (int64_t k = 1; k <= m; k++)
    if (rowThresh[(size_t)k] < n || colThresh[(size_t)k] < m)
      p = (uint32_t)k;

  //
  // Reconstruct.  Each rank's dominant matches form an antichain, so sorting
  // by ascending i forces descending j.  Walking backward from the p-th match,
  // the largest-i (k-1)-match with i < current.i is necessarily above-left of
  // the current match (its j < current.j) giving a valid LCS.
  //
  if (traceLCS && p > 0) {
    for (int64_t k = 1; k <= (int64_t)p; k++) {
      auto& v = byRank[(size_t)k];
      sort(v.begin(), v.end(),
        [](const DomMatch& a, const DomMatch& b) { return a.i < b.i; });
    }

    shared_ptr<Pair> chain = nullptr;   // ascending: rank 1 .. rank p
    auto ci = byRank[(size_t)p].back().i;
    auto cj = byRank[(size_t)p].back().j;
    chain = make_shared<Pair>(ci, cj, nullptr);
    for (int64_t k = (int64_t)p - 1; k >= 1; k--) {
      const auto& v = byRank[(size_t)k];
      auto it = lower_bound(v.begin(), v.end(), ci,
        [](const DomMatch& m, uint32_t x) { return m.i < x; });
      --it;                             // largest i < ci
      ci = it->i;
      cj = it->j;
      chain = make_shared<Pair>(ci, cj, chain);
    }
    *pairs = chain;
  }
  else if (traceLCS)
    *pairs = nullptr;
  if (statistics) {
    statistics->algorithm = LCS::Algorithm::RickTime;
    statistics->records = records;
  }
  return p;
}

uint32_t LCSRickTime::FindFlexibleLength(const u32string& s1, const u32string& s2) {
  return Find(nullptr, nullptr, s1, s2);
}
