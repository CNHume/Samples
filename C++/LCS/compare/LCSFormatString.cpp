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
u32string LCSFormatString::Select(shared_ptr<Delta> deltas,
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

uint32_t LCSFormatString::Show(shared_ptr<Delta> deltas,
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
void LCSFormatString::Series(uint32_t counter,
  const string& label1, const u32string& s1, uint32_t begin1, uint32_t end1,
  const string& label2, const u32string& s2, uint32_t begin2, uint32_t end2) {
  Side("<<<<<", counter, label1, s1, begin1, end1);
  Side(">>>>>", counter, label2, s2, begin2, end2);
}

void LCSFormatString::Side(string emblem, uint32_t counter, const string& label,
  const u32string& s, uint32_t begin, uint32_t end) {
  Head(emblem, counter, label, begin, end);
  Body(s, begin, end);
}

void LCSFormatString::Head(string emblem, uint32_t counter, const string& label,
  uint32_t begin, uint32_t end) {
  if (begin < end) {
    cout << format(
      "{} {} {} [{}:{}] {}\n",
      emblem, counter + 1, label, begin, end, emblem);
  }
}

void LCSFormatString::Body(const u32string& s, uint32_t begin, uint32_t end) {
  cout << LCSEncoding::ToUtf8(s.substr(begin, end - begin)) << endl;
}
