// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
// 2015-01-19 CNHume  Created file
//
#include "LCSString.h"

string LCSString::Correspondence(const string& s1, const string& s2) {
  auto intervals = Compare(s1, s2);
  return Select(intervals, false, s1, s2);
}

shared_ptr<Delta> LCSString::Compare(const string& s1, const string& s2) {
  CHAR2INDEXES indexes;
  MATCHES matches;                      // matches holds references into indexes
  auto count = Match(indexes, matches, s1, s2);
#ifdef SHOW_COUNTS
  cout << count << " matches" << endl;
#endif
  shared_ptr<Pair> pairs;
  auto length = Pairs(matches, &pairs);
  return Delta::Coalesce(pairs);
}

//
// See the STRING2INDEXES overload used for RECORDS
//
uint32_t LCSString::Match(CHAR2INDEXES& indexes, MATCHES& matches,
  const string& s1, const string& s2) {
  uint32_t count = 0;
  uint32_t index = 0;
  for (const auto& it : s2)
    indexes[it].push_front(index++);

  for (const auto& it : s1) {
    auto& dq2 = indexes[it];
    matches.push_back(&dq2);
    count += dq2.size();
  }

  return count;
}

// Concatenate elements from the selected side
string LCSString::Select(shared_ptr<Delta> deltas, bool right,
  const string& s1, const string& s2) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  string buffer;
  buffer.reserve(right ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = right ? next->begin2 : next->begin1;
    auto end = right ? next->end2 : next->end1;
    for (auto index = begin; index <= end; index++) {
      auto c = right ? s2[index] : s1[index];
      buffer.push_back(c);
    }
  }
  return buffer;
}
