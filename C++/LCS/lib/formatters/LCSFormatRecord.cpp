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
// 2026-10-24 CNHume  Delegate layout to Formatter subclasses
// 2022-07-04 CNHume  Created RecordDriver class
//
#include "formatters/LCSFormatRecord.h"

using namespace std;
using namespace lcs;

namespace lcs::formatters {

//
// Delta Formatters
//
// Concatenate elements from the selected side
//
modes::Record::RECORDS RecordDriver::Select(
  shared_ptr<Delta> deltas, const RECORDS& r1, const RECORDS& r2,
  bool isright) {
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

uint32_t RecordDriver::Show(
  Formatter& format, shared_ptr<Delta> deltas,
  const RECORDS& r1, const RECORDS& r2,
  const string& label1, const string& label2) {
  format.BeginResult(label1, label2, (uint32_t)r1.size(), (uint32_t)r2.size());
  uint32_t ndelta = 0;                  // # of deltas
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto side1 = MakeSpan(label1, r1, next->begin1, next->end1);
    auto side2 = MakeSpan(label2, r2, next->begin2, next->end2);
    format.Layout(ndelta, side1, side2);
    ndelta++;
  }
  format.EndResult();
  return ndelta;
}

uint32_t RecordDriver::ShowEdit(
  Edit& edit, const EDIT_INTERVALS& intervals,
  const RECORDS& r1, const RECORDS& r2,
  const string& label1, const string& label2) {
  edit.BeginResult(label1, label2, (uint32_t)r1.size(), (uint32_t)r2.size());
  uint32_t ndelta = 0;                  // # of runs
  for (const auto& interval : intervals) {
    edit.BeginInterval(
      interval.front()->begin1, interval.back()->end1,
      interval.front()->begin2, interval.back()->end2);
    for (const auto& run : interval) {
      auto side1 = MakeSpan(label1, r1, run->begin1, run->end1);
      auto side2 = MakeSpan(label2, r2, run->begin2, run->end2);
      edit.Layout(ndelta, side1, side2, run->kind);
      ndelta++;
    }
  }
  edit.EndResult();
  return ndelta;
}

SideSpan RecordDriver::MakeSpan(
  const string& label, const RECORDS& records,
  uint32_t begin, uint32_t end) {
  SideSpan span;
  span.label = label;
  span.begin = begin;
  span.end = end;
  span.lines.assign(records.begin() + begin, records.begin() + end);
  return span;
}

} // namespace lcs::formatters
