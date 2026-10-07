// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
// 2017-06-29 CNHume  Added Command class
// 2015-01-19 CNHume  Created file
//
#include "LCSRecord.h"

//
// Find Matches
//
// Match() avoids m*n comparisons by using STRING_TO_INDEXES_MAP to
// achieve O(m+n) performance, where m and n are the input lengths.
//
// The lookup time can be assumed constant in the case of characters.
// The symbol space is larger in the case of records; but the lookup
// time will be O(log(m+n)), at most.
//
uint32_t LCSRecord::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  STRING_TO_INDEXES_MAP& indexesOf2MatchedByString,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  uint32_t count = 0;
  uint32_t index = 0;
  string buffer;
  for (const auto& it : r2) {
    LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
    indexesOf2MatchedByString[buffer].push_back(index++);
  }

  for (const auto& it : r1) {
    LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
    auto& dq2 = indexesOf2MatchedByString[buffer];
    indexesOf2MatchedByIndex1.push_back(&dq2);
    count += dq2.size();
  }
#ifdef SHOW_MATCH_COUNT
  cout << format("count = {} of indexesOf2MatchedByIndex1\n", count);
#endif
  return count;
}

uint32_t LCSRecord::Difference(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
#ifdef SHOW_INTERVALS
  Delta::List(deltas);
#endif
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas) : deltas;
  return length;
}

uint32_t LCSRecord::Compare(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  STRING_TO_INDEXES_MAP indexesOf2MatchedByString;
  MATCHES indexesOf2MatchedByIndex1;    // indexesOf2MatchedByIndex1 holds references into indexesOf2MatchedByString
  auto count = Match(indexesOf2MatchedByIndex1, indexesOf2MatchedByString, r1, r2, ignorecase, ignorespace);
  shared_ptr<Pair> pairs;
  auto ppairs = intervals != nullptr ? &pairs : nullptr;
  auto length = FindLCS(ppairs, indexesOf2MatchedByIndex1);
  if (intervals != nullptr)
    *intervals = Delta::Coalesce(pairs);
  return length;
}
