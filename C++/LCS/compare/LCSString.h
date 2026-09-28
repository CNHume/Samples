// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
//
#pragma once

#include "LCS.h"

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
    const string& s1, const string& s2);
  static string Select(shared_ptr<Delta> deltas,
    const string& s1, const string& s2, bool isright = false);

public:
  static shared_ptr<Delta> Correspondence(const string& s1, const string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);
  static shared_ptr<Delta> Difference(const string& s1, const string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);

  static uint32_t Compare(shared_ptr<Delta>* deltas, const string& s1, const string& s2);
};
