// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-15 CNHume  Created LCSRickTime class
//
// See LCSRickTime.h for an overview.
//
#include "LCSRickTime.h"

#include <unordered_map>                // for symbol-id compression
#include <vector>

using namespace std;

namespace {
  //
  // Dense "next occurrence" tables for both strings, with a shared symbol-id
  // compression over the union of their code points.  next[id][j] is the first
  // position >= j holding symbol id (the string length if none).
  //
  struct FlexTables {
    vector<int64_t> id1;               // symbol id of s1[i]
    vector<int64_t> id2;               // symbol id of s2[j]
    vector<vector<int64_t>> next1;     // next1[id][j] over s1, j in [0, m]
    vector<vector<int64_t>> next2;     // next2[id][j] over s2, j in [0, n]
    int64_t m = 0;
    int64_t n = 0;
  };

  FlexTables BuildTables(const u32string& s1, const u32string& s2) {
    FlexTables t;
    t.m = (int64_t)s1.size();
    t.n = (int64_t)s2.size();

    unordered_map<char32_t, int64_t> charToId;
    auto idOf = [&](char32_t c) -> int64_t {
      auto it = charToId.find(c);
      if (it != charToId.end())
        return it->second;
      auto id = (int64_t)charToId.size();
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
    // One extra sentinel slot so "first occurrence > j" at j == length (the
    // threshold sentinel) stays in bounds and returns length.
    t.next1.assign(s, vector<int64_t>((size_t)t.m + 2, t.m));
    for (int64_t j = t.m - 1; j >= 0; j--) {
      for (size_t id = 0; id < s; id++)
        t.next1[id][(size_t)j] = t.next1[id][(size_t)j + 1];
      t.next1[(size_t)t.id1[(size_t)j]][(size_t)j] = j;
    }
    t.next2.assign(s, vector<int64_t>((size_t)t.n + 2, t.n));
    for (int64_t j = t.n - 1; j >= 0; j--) {
      for (size_t id = 0; id < s; id++)
        t.next2[id][(size_t)j] = t.next2[id][(size_t)j + 1];
      t.next2[(size_t)t.id2[(size_t)j]][(size_t)j] = j;
    }
    return t;
  }
}

uint32_t LCSRickTime::FindFlexibleLength(const u32string& s1, const u32string& s2) {
  auto t = BuildTables(s1, s2);
  auto m = t.m;
  auto n = t.n;

  // rowThresh[k] = leftmost column of a dominant k-match (n if none);
  // colThresh[k] = topmost row of a dominant k-match (m if none).
  // Sized m+2 so the "low" index may advance one past the last class.
  vector<int64_t> rowThresh((size_t)m + 2, n);
  vector<int64_t> colThresh((size_t)m + 2, m);
  rowThresh[0] = -1;                    // virtual origin C_0
  colThresh[0] = -1;
  int64_t low = 1;                      // smallest class not yet complete

  for (int64_t l = 0; l < m; l++) {
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
        rowThresh[(size_t)k] = j;       // dominant k-match (l, j)
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
        colThresh[(size_t)k] = i;       // dominant k-match (i, l)
        i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(temp + 1)];
      }
      if (i == colThresh[(size_t)k])
        i = t.next1[(size_t)t.id2[(size_t)l]][(size_t)(i + 1)];
      k++;
    }
  }

  // LCS length = number of non-empty classes.  A class may hold dominant
  // matches only above/below the diagonal, so either threshold array can
  // witness it.
  uint32_t p = 0;
  for (int64_t k = 1; k <= m; k++)
    if (rowThresh[(size_t)k] < n || colThresh[(size_t)k] < m)
      p = (uint32_t)k;
  return p;
}
