// (C) Copyright 2015, Christopher N. Hume.  All rights reserved.
//
// 2017-06-30 CNHume  Added Command class
// 2014-12-19 CNHume  Created file
//
#pragma once
//#define SHOW_COUNTS
#include "Command.h"
#include "Delta.h"

#include <stdint.h>
#include <string>
#include <deque>
#include <vector>
#include <map>
#include <iostream>                     // for cout
#include <fstream>

using namespace std;

class LCS {
public:
  typedef vector<string> RECORDS;

protected:
  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> THRESHOLD;
  typedef deque<uint32_t> INDEXES;
  typedef map<string, INDEXES> STRING2INDEXES;
  typedef deque<INDEXES*> MATCHES;

  uint32_t Match(STRING2INDEXES& indexes, MATCHES& matches,
    const RECORDS& records1, const RECORDS& records2,
    bool ignorecase = false, bool ignorespace = false);
  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
  uint64_t Pairs(MATCHES& matches, shared_ptr<Pair> *pairs);

  static RECORDS Read(const string& filename);
  static uint32_t Show(shared_ptr<Delta> deltas,
    const RECORDS& records1, const RECORDS& records2,
    const string& label1, const string& label2);
  static void Series(uint32_t counter,
    const string& label1, const RECORDS& records1, uint32_t begin1, uint32_t end1,
    const string& label2, const RECORDS& records2, uint32_t begin2, uint32_t end2);
  static void Side(string emblem, uint32_t counter, const string& label,
    const RECORDS& list, uint32_t begin, uint32_t end);
  static void Head(string emblem, uint32_t counter, const string& label,
    uint32_t begin, uint32_t end);
  static void Body(const RECORDS& records, uint32_t begin, uint32_t end);

public:
  shared_ptr<Delta> Compare(const RECORDS& records1, const RECORDS& records2,
    bool ignorecase = false, bool ignorespace = false);
  void Difference(const Command command);
};
