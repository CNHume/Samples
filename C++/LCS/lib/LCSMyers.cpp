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
// 2026-10-27 CNHume  Created LCSMyers class
//
// Myers (1986) minimal edit-script algorithm, adapted to the character path.
//
// See LCSMyers.h for an overview.
//
#include "LCSMyers.h"
#include "Diagnostic.h"

#include <algorithm>                    // for min, reverse
#include <cstdint>                      // for int64_t, uint32_t, uint64_t
#include <memory>                       // for make_shared
#include <stdexcept>                    // for runtime_error
#include <unordered_map>                // for symbol counts
#include <utility>                      // for move
#include <vector>                       // for vector

using namespace std;
using namespace lcs;

namespace {
  struct Match {
    uint32_t row = 0;
    uint32_t column = 0;
  };

  //
  // O(n) lower bound on the edit distance from the symbol counts:
  //   p <= sum_s min(count1[s], count2[s])   =>   D >= m + n - 2*sum min(...)
  //
  uint64_t DistanceLowerBound(const u32string& s1, const u32string& s2) {
    unordered_map<char32_t, uint64_t> count1;
    unordered_map<char32_t, uint64_t> count2;
    for (auto c : s1)
      count1[c]++;
    for (auto c : s2)
      count2[c]++;
    uint64_t common = 0;
    for (const auto& entry : count1) {
      auto found = count2.find(entry.first);
      if (found != count2.end())
        common += min(entry.second, found->second);
    }
    return s1.size() + s2.size() - 2 * common;
  }
}

uint32_t LCSMyers::Find(
  shared_ptr<Pair>* pairs, Statistics* statistics,
  const u32string& s1, const u32string& s2) {
  auto m = s1.size();
  auto n = s2.size();
  auto budget = Diagnostic::MyersBudget(m, n);

  //
  // Composition pre-filter: when the count-based lower bound already exceeds
  // the budget, the input is too dissimilar, so abort before the O(D^2) sweep.
  //
  auto lower = DistanceLowerBound(s1, s2);
  if (lower > budget) {
    Diagnostic::WarnMyersDistance(lower, budget);
    throw runtime_error(
      "LCSMyers: edit distance exceeds the budget; use -as (RickSpace) or -at (RickTime)");
  }

  //
  // Forward D-path sweep.  V[d] holds the furthest x reached on each diagonal
  // k = x - y, indexed by k + d; snakes follow the matching runs.  The first d
  // whose frontier reaches (m, n) is the minimal edit distance.
  //
  vector<vector<int64_t>> V;
  V.reserve((size_t)budget + 1);

  // d = 0: follow the initial snake on diagonal 0 from (0, 0).
  int64_t x = 0;
  int64_t y = 0;
  while (x < (int64_t)m && y < (int64_t)n && s1[(size_t)x] == s2[(size_t)y]) {
    x++;
    y++;
  }
  V.push_back(vector<int64_t>{ x });

  int64_t D = (x >= (int64_t)m && y >= (int64_t)n) ? 0 : -1;
  for (int64_t d = 1; d <= (int64_t)budget && D < 0; d++) {
    auto& prev = V[(size_t)(d - 1)];
    vector<int64_t> v((size_t)(2 * d + 1), 0);
    for (int64_t k = -d; k <= d; k += 2) {
      if (k == -d || (k != d && prev[(size_t)(k - 1 + d - 1)] < prev[(size_t)(k + 1 + d - 1)])) {
        x = prev[(size_t)(k + 1 + d - 1)];        // down move (insertion)
      }
      else {
        x = prev[(size_t)(k - 1 + d - 1)] + 1;    // right move (deletion)
      }
      y = x - k;
      // Follow the snake along diagonal k.
      while (x < (int64_t)m && y < (int64_t)n && s1[(size_t)x] == s2[(size_t)y]) {
        x++;
        y++;
      }
      v[(size_t)(k + d)] = x;
      if (x >= (int64_t)m && y >= (int64_t)n) {
        D = d;
        break;
      }
    }
    V.push_back(move(v));
  }

  if (D < 0) {
    Diagnostic::WarnMyersDistance(budget, budget);
    throw runtime_error(
      "LCSMyers: edit distance exceeds the budget; use -as (RickSpace) or -at (RickTime)");
  }

  auto length = (uint32_t)((m + n - (size_t)D) / 2);
  if (statistics) {
    statistics->algorithm = LCS::Algorithm::Myers;
    statistics->edits = (uint64_t)D;
  }

  if (pairs == nullptr)
    return length;

  //
  // Reconstruction: walk back from (m, n) across the stored frontiers.  Each
  // depth's snake contributes a run of matched positions on its diagonal.
  // Collect them in reverse, then flip to ascending order.
  //
  vector<Match> matches;
  x = (int64_t)m;
  y = (int64_t)n;
  for (int64_t d = D; d > 0; d--) {
    int64_t k = x - y;
    auto& prev = V[(size_t)(d - 1)];
    int64_t prevK;
    if (k == -d || (k != d && prev[(size_t)(k - 1 + d - 1)] < prev[(size_t)(k + 1 + d - 1)])) {
      prevK = k + 1;                              // down move (insertion) from k + 1
    }
    else {
      prevK = k - 1;                              // right move (deletion) from k - 1
    }
    int64_t prevX = prev[(size_t)(prevK + d - 1)];
    int64_t prevY = prevX - prevK;
    // The snake on diagonal k ran from the move endpoint to (x, y).
    int64_t startX = prevK == k + 1 ? prevX : prevX + 1;
    // Emit the snake high-to-low so the reverse() below yields ascending rows.
    for (int64_t i = x - 1; i >= startX; i--)
      matches.push_back({ .row = (uint32_t)i, .column = (uint32_t)(i - k) });
    x = prevX;
    y = prevY;
  }
  // d = 0: the initial snake on diagonal 0 from (0, 0) to (x, y).
  for (int64_t i = x - 1; i >= 0; i--)
    matches.push_back({ .row = (uint32_t)i, .column = (uint32_t)i });

  reverse(matches.begin(), matches.end());

  shared_ptr<Pair> head = nullptr;
  shared_ptr<Pair> tail = nullptr;
  for (const auto& match : matches) {
    auto pair = make_shared<Pair>(match.row, match.column);
    if (tail == nullptr)
      head = pair;
    else
      tail->next = pair;
    tail = pair;
  }
  *pairs = head;
  return length;
}
