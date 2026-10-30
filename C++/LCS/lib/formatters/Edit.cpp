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

//
// MakeRun() creates a Delta run tagged Match (context) or Change.
//
shared_ptr<Delta> Edit::MakeRun(
  EditKind kind, uint32_t begin1, uint32_t begin2, uint32_t end1, uint32_t end2) {
  auto run = make_shared<Delta>(begin1, begin2, end1, end2);
  run->kind = kind;
  return run;
}

//
// BuildIntervals() splits the coalesced differences into unified-diff hunks:
// each hunk is split at the matches it encloses, with prefix/suffix context
// around it.
//
EDIT_INTERVALS Edit::BuildIntervals(
  const shared_ptr<Delta>& matches, const shared_ptr<Delta>& differences,
  uint32_t prefix, uint32_t suffix) {
  EDIT_INTERVALS intervals;
  auto mp = matches;
  for (auto c = differences; c != nullptr; c = dynamic_pointer_cast<Delta>(c->next)) {
    auto cBegin1 = c->begin1, cBegin2 = c->begin2;
    auto cEnd1 = c->end1, cEnd2 = c->end2;

    vector<shared_ptr<Delta>> interval;

    // Prefix context: the tail of the match ending where the hunk begins.
    shared_ptr<Delta> lead = nullptr;
    while (mp != nullptr && mp->end1 <= cBegin1) {
      if (mp->end1 == cBegin1)
        lead = mp;
      mp = dynamic_pointer_cast<Delta>(mp->next);
    }
    if (lead != nullptr) {
      auto n = min(prefix, lead->end1 - lead->begin1);
      if (n > 0)
        interval.push_back(MakeRun(EditKind::Match,
          lead->end1 - n, lead->end2 - n, lead->end1, lead->end2));
    }

    // Body: split the hunk at each enclosed match, emitting Change/Match runs.
    auto b1 = cBegin1, b2 = cBegin2;
    while (mp != nullptr && mp->begin1 < cEnd1) {
      interval.push_back(MakeRun(EditKind::Change,
        b1, b2, mp->begin1, mp->begin2));
      interval.push_back(MakeRun(EditKind::Match,
        mp->begin1, mp->begin2, mp->end1, mp->end2));
      b1 = mp->end1;
      b2 = mp->end2;
      mp = dynamic_pointer_cast<Delta>(mp->next);
    }
    interval.push_back(MakeRun(EditKind::Change, b1, b2, cEnd1, cEnd2));

    // Suffix context: the head of the match starting where the hunk ends.
    if (mp != nullptr && mp->begin1 == cEnd1) {
      auto n = min(suffix, mp->end1 - mp->begin1);
      if (n > 0)
        interval.push_back(MakeRun(EditKind::Match,
          mp->begin1, mp->begin2, mp->begin1 + n, mp->begin2 + n));
    }

    intervals.push_back(interval);
  }

  return intervals;
}

} // namespace lcs::formatters
