// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
//
#pragma once

#include "LCS.h"

using namespace std;

class LCSRecord : protected LCS {
public:
  typedef vector<string> RECORDS;

protected:
  uint32_t Match(STRING_TO_INDEXES_MAP& indexesOf2MatchedByString, MATCHES& indexesOf2MatchedByIndex1,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);

  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
  static void NormalCase(string& input);
  static void NormalSpace(const string& input, string& output);

  static RECORDS Select(shared_ptr<Delta> deltas, bool right,
    const RECORDS& r1, const RECORDS& r2);

public:
  RECORDS Correspondence(const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
  RECORDS Difference(const RECORDS& r1, const RECORDS& r2,
    bool isjoin = false, uint32_t join = 0,
    bool ignorecase = false, bool ignorespace = false);

  shared_ptr<Delta> Compare(const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
};
