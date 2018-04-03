// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
//
#pragma once

#include "LCS.h"

using namespace std;

class LCSString : LCS {
  //
  // String Methods
  //
protected:
  typedef map<char, INDEXES> CHAR2INDEXES;

  uint32_t Match(CHAR2INDEXES& indexes, MATCHES& matches,
    const string& s1, const string& s2);
  static string Select(shared_ptr<Delta> deltas, bool side,
    const string& s1, const string& s2);

public:
  string Correspondence(const string& s1, const string& s2);
  shared_ptr<Delta> Compare(const string& s1, const string& s2);
};
