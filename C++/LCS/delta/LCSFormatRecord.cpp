// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2022-07-04 CNHume  Created LCSFormat subclass
//
#include "LCSFormatRecord.h"

//
// Delta Formatters
//
// Concatenate elements from the selected side
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
    for (auto index = begin; index <= end; index++)
      list.push_back(records[index]);
  }
  return list;
}

uint32_t LCSFormatRecord::Show(shared_ptr<Delta> deltas,
  const RECORDS& r1, const RECORDS& r2,
  const string& label1, const string& label2) {
  uint32_t ndelta = 0;                  // # of deltas
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    Series(ndelta,
      label1, r1, next->begin1, next->end1,
      label2, r2, next->begin2, next->end2);
    ndelta++;
  }
  return ndelta;
}

// Write both sides of a Delta in series
void LCSFormatRecord::Series(uint32_t counter,
  const string& label1, const RECORDS& r1, uint32_t begin1, uint32_t end1,
  const string& label2, const RECORDS& r2, uint32_t begin2, uint32_t end2) {
  Side("<<<<<", counter, label1, r1, begin1, end1);
  Side(">>>>>", counter, label2, r2, begin2, end2);
}

void LCSFormatRecord::Side(string emblem, uint32_t counter, const string& label,
  const RECORDS& list, uint32_t begin, uint32_t end) {
  Head(emblem, counter, label, begin, end);
  Body(list, begin, end);
}

void LCSFormatRecord::Head(string emblem, uint32_t counter, const string& label,
  uint32_t begin, uint32_t end) {
  if (begin < end) {
    cout << format(
      "{} {} {} [{}:{}] {}\n",
      emblem, counter + 1, label, begin, end, emblem);
  }
}

void LCSFormatRecord::Body(const RECORDS& records, uint32_t begin, uint32_t end) {
  for (auto index = begin; index < end; index++)
    cout << records[index] << endl;
}
