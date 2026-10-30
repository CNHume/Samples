// Copyright (C) 2026, Christopher N. Hume.  All rights reserved.
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
// 2026-10-29 CNHume  Created Edit class
//
#include "formatters/Edit.h"

#include <algorithm>                    // for min
#include <format>                       //[C++20]
#include <iostream>                     // for cout

using namespace std;
using namespace lcs;

namespace lcs::formatters {

void Edit::BeginResult(
  const string& label1, const string& label2, uint32_t, uint32_t) {
  cout << "--- " << label1 << '\n';
  cout << "+++ " << label2 << '\n';
}

void Edit::BeginInterval(
  uint32_t begin1, uint32_t end1, uint32_t begin2, uint32_t end2) {
  cout << format("@@ -{} +{} @@\n",
    Range(begin1, end1), Range(begin2, end2));
}

void Edit::Layout(
  uint32_t, const SideSpan& side1, const SideSpan& side2, EditKind kind) {
  if (kind == EditKind::Match) {
    for (const auto& line : side1.lines)
      cout << ' ' << line << '\n';
  }
  else {
    for (const auto& line : side1.lines)
      cout << '-' << line << '\n';
    for (const auto& line : side2.lines)
      cout << '+' << line << '\n';
  }
}

void Edit::EndResult() {}

//
// Format one side of an @@ header from a semi-open [begin, end) range.  A
// count of 0 means the block has no lines from that file, so the start is the
// line just before the block; a count of 1 is omitted.
//
string Edit::Range(uint32_t begin, uint32_t end) {
  auto count = end - begin;
  if (count == 0)
    return format("{},0", begin);       // empty: start is the line before the block
  auto line = begin + 1;                // 1-based start line
  return count == 1 ?
    format("{}", line) : format("{},{}", line, count);
}

EDIT_INTERVALS Edit::BuildIntervals(
  const shared_ptr<Delta>& matches, const shared_ptr<Delta>& differences,
  uint32_t prefix, uint32_t suffix) {
  EDIT_INTERVALS intervals;

  //
  // Merge the match and difference runs into one ordered, tagged list.  The
  // runs are disjoint, so (begin1, begin2) orders them; the tuple tiebreak
  // keeps a pure insertion (empty old side) before the match that follows it.
  //
  vector<shared_ptr<Delta>> segments;
  auto mp = matches;
  auto dp = differences;
  while (mp != nullptr || dp != nullptr) {
    bool takeMatch;
    if (mp == nullptr)
      takeMatch = false;
    else if (dp == nullptr)
      takeMatch = true;
    else
      takeMatch =
        mp->begin1 < dp->begin1 ||
        (mp->begin1 == dp->begin1 && mp->begin2 < dp->begin2);
    if (takeMatch) {
      mp->kind = EditKind::Match;
      segments.push_back(mp);
      mp = dynamic_pointer_cast<Delta>(mp->next);
    }
    else {
      dp->kind = EditKind::Change;
      segments.push_back(dp);
      dp = dynamic_pointer_cast<Delta>(dp->next);
    }
  }

  // Build a Match context run over a sub-range of a match.
  auto context = [](uint32_t b1, uint32_t e1, uint32_t b2, uint32_t e2) {
    auto run = make_shared<Delta>(b1, b2, e1, e2);
    run->kind = EditKind::Match;
    return run;
  };

  auto n = segments.size();
  size_t i = 0;
  while (i < n) {
    // Skip any leading Match runs (no difference precedes them).
    while (i < n && segments[i]->kind == EditKind::Match)
      i++;
    if (i >= n)
      break;

    vector<shared_ptr<Delta>> interval;
    // Leading context: the tail of the Match before this Change.
    if (i > 0 && segments[i - 1]->kind == EditKind::Match) {
      const auto& lead = segments[i - 1];
      auto len = lead->end1 - lead->begin1;
      auto nctx = min(prefix, len);
      if (nctx > 0)
        interval.push_back(context(
          lead->end1 - nctx, lead->end1, lead->end2 - nctx, lead->end2));
    }

    // The change run(s), merging while the Match between them is small.
    interval.push_back(segments[i++]);
    while (i + 1 < n && segments[i]->kind == EditKind::Match) {
      const auto& between = segments[i];
      auto len = between->end1 - between->begin1;
      if (len > prefix + suffix)
        break;
      interval.push_back(context(
        between->begin1, between->end1, between->begin2, between->end2));
      i++;                              // past the between match
      interval.push_back(segments[i++]); // the following Change
    }

    // Trailing context: the head of the Match after the last Change.
    if (i < n && segments[i]->kind == EditKind::Match) {
      const auto& trail = segments[i];
      auto len = trail->end1 - trail->begin1;
      auto nctx = min(suffix, len);
      if (nctx > 0)
        interval.push_back(context(
          trail->begin1, trail->begin1 + nctx,
          trail->begin2, trail->begin2 + nctx));
    }

    intervals.push_back(interval);
  }

  return intervals;
}

} // namespace lcs::formatters
