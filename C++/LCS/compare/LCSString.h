// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
//
#pragma once

#include "LCS.h"
#include "LCSNormal.h"

using namespace std;

class LCSString : protected LCS {
  //
  // String Methods
  //
protected:
  typedef unordered_map<char, INDEXES> CHAR_TO_INDEXES_MAP;

  static uint32_t Match(
    MATCHES& indexesOf2MatchedByIndex1,
    CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
    const string& s1, const string& s2,
    bool ignorecase, bool ignorespace);
  static string Select(shared_ptr<Delta> deltas,
    const string& s1, const string& s2, bool isright = false);

public:
  static uint32_t Correspondence(shared_ptr<Delta>* intervals,
    const string& s1, const string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);
  static uint32_t Difference(shared_ptr<Delta>* intervals,
    const string& s1, const string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);

  static uint32_t Compare(shared_ptr<Delta>* intervals,
    const string& s1, const string& s2,
    bool ignorecase = false, bool ignorespace = false);
};
