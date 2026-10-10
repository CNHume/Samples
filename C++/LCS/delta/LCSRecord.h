// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
//
#pragma once

#include "LCS.h"
#include "LCSEncoding.h"
#include "LCSNormal.h"

using namespace std;

class LCSRecord : public LCS {
public:
  typedef vector<string> RECORDS;

  static uint32_t Correspondence(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false, bool isjoin = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0);
  static uint32_t Difference(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false, bool isjoin = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0);

  static uint32_t Compare(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);

  static RECORDS Read(const string& filename, bool isword);

protected:
  static uint32_t Match(
    MATCHES& indexesOf2MatchedByIndex1,
    STRING_TO_INDEXES_MAP& indexesOf2MatchedByString,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
};
