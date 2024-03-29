// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2022-07-04 CNHume  Created LCSFormat subclass
//
#include "LCSFormat.h"

//
// Delta formatter
//
uint32_t LCSFormat::Show(shared_ptr<Delta> deltas,
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
void LCSFormat::Series(uint32_t counter,
  const string& label1, const RECORDS& r1, uint32_t begin1, uint32_t end1,
  const string& label2, const RECORDS& r2, uint32_t begin2, uint32_t end2) {
  Side("<<<<<", counter, label1, r1, begin1, end1);
  Side(">>>>>", counter, label2, r2, begin2, end2);
}

void LCSFormat::Side(string emblem, uint32_t counter, const string& label,
  const RECORDS& list, uint32_t begin, uint32_t end) {
  Head(emblem, counter, label, begin, end);
  Body(list, begin, end);
}

void LCSFormat::Head(string emblem, uint32_t counter, const string& label,
  uint32_t begin, uint32_t end) {
  if (begin < end) {
    cout << emblem << " " << counter + 1 << " " << label
      << " [" << begin << ":" << end << "] "
      << emblem << endl;
  }
}

void LCSFormat::Body(const RECORDS& records, uint32_t begin, uint32_t end) {
  for (auto index = begin; index < end; index++)
    cout << records[index] << endl;
}
