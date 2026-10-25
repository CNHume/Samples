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
// 2026-10-08 CNHume  Created LCSFormatText class
//
#include "LCSFormatText.h"
#include "LCSEncoding.h"

#include <format>                       //[C++20]
#include <iostream>                     // for cout

using namespace std;
using namespace lcs;

//
// Delta Formatters
//
// Concatenate elements from the selected side
//
u32string LCSFormatText::Select(shared_ptr<Delta> deltas,
  const u32string& s1, const u32string& s2, bool isright) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  u32string buffer;
  buffer.reserve(isright ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = isright ? next->begin2 : next->begin1;
    auto end = isright ? next->end2 : next->end1;
    auto& s = isright ? s2 : s1;
    for (auto index = begin; index < end; index++)
      buffer.push_back(s[index]);
  }
  return buffer;
}

uint32_t LCSFormatText::Show(shared_ptr<Delta> deltas,
  const u32string& s1, const u32string& s2,
  const string& label1, const string& label2) {
  uint32_t ndelta = 0;                  // # of deltas
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    Series(ndelta,
      label1, s1, next->begin1, next->end1,
      label2, s2, next->begin2, next->end2);
    ndelta++;
  }
  return ndelta;
}

// Write both sides of a Delta in series
void LCSFormatText::Series(uint32_t counter,
  const string& label1, const u32string& s1, uint32_t begin1, uint32_t end1,
  const string& label2, const u32string& s2, uint32_t begin2, uint32_t end2) {
  Side("<<<<<", counter, label1, s1, begin1, end1);
  Side(">>>>>", counter, label2, s2, begin2, end2);
}

void LCSFormatText::Side(string emblem, uint32_t counter, const string& label,
  const u32string& s, uint32_t begin, uint32_t end) {
  if (begin < end) {
    Head(emblem, counter, label, begin, end);
    Body(s, begin, end);
  }
}

void LCSFormatText::Head(string emblem, uint32_t counter, const string& label,
  uint32_t begin, uint32_t end) {
  if (begin < end) {
    cout << format(
      "{} {} {} [{}:{}] {}\n",
      emblem, counter + 1, label, begin, end, emblem);
  }
}

void LCSFormatText::Body(const u32string& s, uint32_t begin, uint32_t end) {
  cout << LCSEncoding::ToUtf8(s.substr(begin, end - begin)) << '\n';
}
