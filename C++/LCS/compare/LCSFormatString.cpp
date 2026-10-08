// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-08 CNHume  Created LCSFormatString class
//
#include "LCSFormatString.h"

//
// Delta Formatters
//
// Concatenate elements from the selected side
//
string LCSFormatString::Select(shared_ptr<Delta> deltas,
  const string& s1, const string& s2, bool isright) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  string buffer;
  buffer.reserve(isright ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = isright ? next->begin2 : next->begin1;
    auto end = isright ? next->end2 : next->end1;
    auto& s = isright ? s2 : s1;
    for (auto index = begin; index <= end; index++)
      buffer.push_back(s[index]);
  }
  return buffer;
}
