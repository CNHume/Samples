// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Moved methods to LCSRecord and LCSString subclasses
// 2017-06-29 CNHume  Added Command class
// 2015-01-19 CNHume  Refactored Show() and Delta::Complement()
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2015-01-16 CNHume  Avoid construction of superfluous Pairs
// 2015-01-15 CNHume  Settled on LCS as the name of the class
// 2015-01-06 CNHume  Added prefix and suffix support to Complement()
// 2015-01-02 CNHume  Implemented the Normal() method
// 2014-12-19 CNHume  Created file
//
#pragma once
#define FILTER_PAIRS
//#define SHOW_MATCHES
//#define SHOW_MATCH_COUNT              // See Match() in LCSFile, LCSRecord, and LCSString
//#define SHOW_PAIRS
//#define SHOW_PREFIXENDS

#include "Delta.h"
#include "join.h"

#include <deque>
#include <iostream>                     // for cout
#include <stdint.h>
#include <string>
#include <vector>
#include <unordered_map>                //[C++11]

using namespace std;

class LCS {
public:
  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> INDEXES;
  typedef unordered_map<string, INDEXES> STRING_TO_INDEXES_MAP;
  typedef deque<INDEXES*> MATCHES;
};
