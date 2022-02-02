// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
// 2017-06-30 CNHume  Added Command class
// 2015-01-19 CNHume  Created file
//
#include "LCSRecord.h"
#if false
LCSRecord::RECORDS LCSRecord::Difference(const RECORDS& r1, const RECORDS& r2,
  bool isjoin, uint32_t join,
  bool ignorecase, bool ignorespace) {
  auto intervals = Compare(r1, r2, ignorecase, ignorespace);

  auto size1 = r1.size();           // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(intervals, size1, size2);
#ifdef SHOW_DELTAS
  Delta::List(deltas);
#endif
  //[ToDo]Delta::Context(deltas, size1, size2, prefix, suffix);
  auto joins = Delta::Coalesce(deltas, join);
  return Select(joins, false, r1, r2);
}
#endif

shared_ptr<Delta> LCSRecord::Compare(const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  STRING2INDEXES indexes;
  MATCHES matches;                      // matches holds references into indexes
  auto count = Match(indexes, matches, r1, r2, ignorecase, ignorespace);
#ifdef SHOW_COUNTS
  cout << count << " matches" << endl;
#endif
  shared_ptr<Pair> pairs;
  auto length = Pairs(matches, &pairs);
  return Delta::Coalesce(pairs);
}

//
// Find Matches
//
// Match() avoids incurring m*n comparisons by using the associative
// memory implemented by STRING2INDEXES to achieve O(m+n) performance,
// where m and n are the input lengths.
//
// The lookup time can be assumed constant in the case of characters.
// The symbol space is larger in the case of records; but the lookup
// time will be O(log(m+n)), at most.
//
uint32_t LCSRecord::Match(STRING2INDEXES& indexes, MATCHES& matches,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  uint32_t count = 0;
  uint32_t index = 0;
  string buffer;
  for (const auto& it : r2) {
    Normal(it, buffer, ignorecase, ignorespace);
    indexes[buffer].push_back(index++);
  }

  for (const auto& it : r1) {
    Normal(it, buffer, ignorecase, ignorespace);
    auto& dq2 = indexes[buffer];
    matches.push_back(&dq2);
    count += dq2.size();
  }

  return count;
}

//
// Normal() applies the ignorecase and ignorespace options,
// normalizing records prior to their comparison in Match()
//
void LCSRecord::Normal(const string& input, string& output,
  bool ignorecase, bool ignorespace) {
  if (ignorespace) {
    // outer (right) trim
    auto end = 0;
    for (auto index = input.size() - 1; index != string::npos; index--) {
      if (!isspace(input[index])) {
        end = index + 1;
        break;
      }
    }

    output.clear();
    output.reserve(input.size());
    // inner (left) trims
    bool spaced = true;
    for (auto index = 0; index < end; index++) {
      auto c = input[index];
      if (isspace(c)) {
        if (!spaced) {
          // normal space
          output.push_back(' ');
          spaced = true;
        }
      }
      else {
        output.push_back(c);
        spaced = false;
      }
    }
  }
  else
    output = input;

  if (ignorecase)
    for (auto& c : output)
      c = tolower(c);                   // normal case
}
#if false
// Concatenate elements from the selected side
LCSRecord::RECORDS LCSRecord::Select(shared_ptr<Delta> deltas, bool right,
  const RECORDS& r1, const RECORDS& r2) {
  uint32_t length1, length2;
  Delta::Lengths(deltas, length1, length2);
  RECORDS list;
  list.reserve(right ? length2 : length1);
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin = right ? next->begin2 : next->begin1;
    auto end = right ? next->end2 : next->end1;
    for (auto index = begin; index <= end; index++) {
      auto record = right ? r2[index] : r1[index];
      list.push_back(record);
    }
  }
  return list;
}
#endif