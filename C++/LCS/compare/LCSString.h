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
    CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar, MATCHES& indexesOf2MatchedByIndex1,
    const string& s1, const string& s2);
  static string Select(shared_ptr<Delta> deltas, bool side,
    const string& s1, const string& s2);

public:
  static string Correspondence(const string& s1, const string& s2);
  static shared_ptr<Delta> Compare(const string& s1, const string& s2);
};
