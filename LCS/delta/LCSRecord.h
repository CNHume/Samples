// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
//
#pragma once

#include "LCS.h"

using namespace std;

class LCSRecord : LCS {
public:
  typedef vector<string> RECORDS;

protected:
  uint32_t Match(STRING2INDEXES& indexes, MATCHES& matches,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
#if false
  static RECORDS Select(shared_ptr<Delta> deltas, bool right,
    const RECORDS& r1, const RECORDS& r2);
#endif
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

public:
#if false
  RECORDS Difference(const RECORDS& r1, const RECORDS& r2,
    bool isjoin = false, uint32_t join = 0,
    bool ignorecase = false, bool ignorespace = false);
#endif
  shared_ptr<Delta> Compare(const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
};
