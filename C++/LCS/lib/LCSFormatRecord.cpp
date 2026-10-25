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
// 2026-10-24 CNHume  Delegate layout to LCSFormat subclasses
// 2022-07-04 CNHume  Created LCSFormatRecord class
//
#include "LCSFormatRecord.h"

using namespace std;
using namespace lcs;

//
// Delta Formatters
//
// Concatenate elements from the selected side
//
LCSRecord::RECORDS LCSFormatRecord::Select(shared_ptr<Delta> deltas,
  const RECORDS& r1, const RECORDS& r2, bool isright) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  RECORDS list;
  list.reserve(isright ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = isright ? next->begin2 : next->begin1;
    auto end = isright ? next->end2 : next->end1;
    auto& records = isright ? r2 : r1;
    for (auto index = begin; index < end; index++)
      list.push_back(records[index]);
  }
  return list;
}

uint32_t LCSFormatRecord::Show(LCSFormat& format,
  shared_ptr<Delta> deltas,
  const RECORDS& r1, const RECORDS& r2,
  const string& label1, const string& label2) {
  uint32_t ndelta = 0;                  // # of deltas
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto side1 = MakeSpan(label1, r1, next->begin1, next->end1);
    auto side2 = MakeSpan(label2, r2, next->begin2, next->end2);
    format.Layout(ndelta, side1, side2);
    ndelta++;
  }
  return ndelta;
}

SideSpan LCSFormatRecord::MakeSpan(const string& label,
  const RECORDS& records, uint32_t begin, uint32_t end) {
  SideSpan span;
  span.label = label;
  span.begin = begin;
  span.end = end;
  span.lines.assign(records.begin() + begin, records.begin() + end);
  return span;
}
