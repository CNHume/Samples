// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
// 2015-01-19 CNHume  Created file
//
#include "LCSString.h"

//
// Compare with STRING_TO_INDEXES_MAP used for RECORDS
//
uint32_t LCSString::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
  const string& s1, const string& s2) {
  uint32_t count = 0;
  uint32_t index = 0;
  for (const auto& it : s2)
    indexesOf2MatchedByChar[it].push_back(index++);

  for (const auto& it : s1) {
    auto& dq2 = indexesOf2MatchedByChar[it];
    indexesOf2MatchedByIndex1.push_back(&dq2);
    count += dq2.size();
  }

  return count;
}

// Concatenate elements from the selected side
string LCSString::Select(shared_ptr<Delta> deltas,
  const string& s1, const string& s2, bool isright) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  string buffer;
  buffer.reserve(isright ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = isright ? next->begin2 : next->begin1;
    auto end = isright ? next->end2 : next->end1;
    auto& s = isright ? s2 : s1;
    for (auto index = begin; index <= end; index++)
      buffer.push_back(s[index]);
  }
  return buffer;
}

uint32_t LCSString::Correspondence(shared_ptr<Delta>* intervals,
  const string& s1, const string& s2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  auto length = Compare(intervals, s1, s2);
#ifdef SHOW_INTERVALS
  Delta::List(intervals);
#endif
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t LCSString::Difference(shared_ptr<Delta>* intervals,
  const string& s1, const string& s2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  auto length = Compare(intervals, s1, s2);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
#ifdef SHOW_INTERVALS
  Delta::List(deltas);
#endif
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas) : deltas;
  return length;
}

uint32_t LCSString::Compare(shared_ptr<Delta>* deltas,
  const string& s1, const string& s2) {
  CHAR_TO_INDEXES_MAP indexesOf2MatchedByChar;
  MATCHES indexesOf2MatchedByIndex1;    // indexesOf2MatchedByIndex1 holds references into indexesOf2MatchedByChar
  auto count = Match(indexesOf2MatchedByIndex1, indexesOf2MatchedByChar, s1, s2);
#ifdef SHOW_MATCH_COUNT
  cout << format("count = {} of indexesOf2MatchedByIndex1\n", count);
#endif
  shared_ptr<Pair> pairs;
  auto ppairs = deltas != nullptr ? &pairs : nullptr;
  auto length = FindLCS(ppairs, indexesOf2MatchedByIndex1);
  if (deltas != nullptr)
    *deltas = Delta::Coalesce(pairs);
  return length;
}
