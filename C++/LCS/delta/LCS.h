// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
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
//#define SHOW_PAIRS
//#define SHOW_COUNTS
//#define SHOW_DELTAS
//#define SHOW_MATCHES
//#define SHOW_THRESHOLDS
#define FILTER_PAIRS

#include "Delta.h"
#include "join.h"

#include <stdint.h>
#include <string>
#include <deque>
#include <vector>
#include <unordered_map>                //[C++11]
#include <iostream>                     // for cout

using namespace std;

class LCS {
protected:
  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> INDEXES;
  typedef unordered_map<string, INDEXES> STRING_TO_INDEXES_MAP;
  typedef deque<INDEXES*> MATCHES;

  static shared_ptr<Pair> pushPair(
    PAIRS& chains, const ptrdiff_t& index3, uint32_t& index1, uint32_t& index2);
  static uint32_t FindLCS(MATCHES& indexesOf2MatchedByIndex1, shared_ptr<Pair>* pairs);
};
