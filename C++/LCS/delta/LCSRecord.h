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
  static uint32_t Match(
    STRING_TO_INDEXES_MAP& indexesOf2MatchedByString, MATCHES& indexesOf2MatchedByIndex1,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);

  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
  static void NormalCase(string& input);
  static void NormalSpace(const string& input, string& output);

public:
  static shared_ptr<Delta> Difference(const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false, bool isjoin = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0);

  static shared_ptr<Delta> Compare(const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
};
