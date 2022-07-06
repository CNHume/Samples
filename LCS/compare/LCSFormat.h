// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2022-07-04 CNHume  Created LCSFormat subclass
//
#pragma once

#include "LCSRecord.h"

using namespace std;

class LCSFormat : protected LCSRecord {
protected:
  static uint32_t Show(shared_ptr<Delta> deltas,
    const RECORDS& r1, const RECORDS& r2,
    const string& label1, const string& label2);
  static void Series(uint32_t counter,
    const string& label1, const RECORDS& r1, uint32_t begin1, uint32_t end1,
    const string& label2, const RECORDS& r2, uint32_t begin2, uint32_t end2);
  static void Side(string emblem, uint32_t counter, const string& label,
    const RECORDS& list, uint32_t begin, uint32_t end);
  static void Head(string emblem, uint32_t counter, const string& label,
    uint32_t begin, uint32_t end);
  static void Body(const RECORDS& records, uint32_t begin, uint32_t end);
};
