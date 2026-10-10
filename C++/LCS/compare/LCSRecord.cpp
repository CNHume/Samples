// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
// 2015-01-19 CNHume  Created file
//
#include "LCSRecord.h"
#include "LCSHuntSzymanski.h"
#include "LCSRick.h"

#include <utility>                      // for swap

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

uint32_t LCSRecord::Correspondence(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix, bool isrick) {
  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace, isrick);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t LCSRecord::Difference(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix, bool isrick) {
  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace, isrick);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSRecord::Compare(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isrick) {
  // Swap (and un-swap below) so the shorter sequence is scanned and the
  // longer is tabled, as Rick's time bound assumes m <= n.
  auto swapped = r2.size() < r1.size();
  const auto& shorter = swapped ? r2 : r1;
  const auto& longer = swapped ? r1 : r2;

  STRING_TO_INDEXES_MAP indexesOf2MatchedByString;
  MATCHES indexesOf2MatchedByIndex1;    // holds references into indexesOf2MatchedByString
  [[maybe_unused]] auto count = Match(
    indexesOf2MatchedByIndex1, indexesOf2MatchedByString, shorter, longer, ignorecase, ignorespace);

  shared_ptr<Pair> pairs;
  auto ppairs = intervals != nullptr ? &pairs : nullptr;
  auto length = isrick ?
    LCSRick::Find(
      ppairs, indexesOf2MatchedByIndex1,
      (uint32_t)shorter.size(), (uint32_t)longer.size()) :
    LCSHuntSzymanski::Find(ppairs, indexesOf2MatchedByIndex1);

  if (intervals != nullptr) {
    if (swapped)
      for (auto pair = pairs; pair != nullptr; pair = pair->next)
        swap(pair->begin1, pair->begin2);
    *intervals = Delta::Coalesce(pairs);
  }
  return length;
}

//
// RECORDS Reader
//
LCSRecord::RECORDS LCSRecord::Read(const string& filename, bool isword) {
  auto [encoding, bomLength] = LCSEncoding::PeekEncoding(filename);

  if (LCSEncoding::IsWide(encoding))
    return LCSEncoding::ReadWide(filename, encoding, isword);

  ifstream input(filename, ios::binary);

  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }

  RECORDS records;
  string buffer;
  auto count = 0;
  while (getline(input, buffer)) {
    if (!buffer.empty() && buffer.back() == '\r')
      buffer.pop_back();                // Strip CRLF terminator.
    auto record = buffer;
    if (count == 0 && bomLength > 0)
      record = &buffer[bomLength];

    if (isword) {
      istringstream iss(record);
      string token;
      while (!iss.eof()) {
        iss >> token;
        records.push_back(token);
      }
    }
    else
      records.push_back(record);
    count++;
  }

  input.close();
#ifdef SHOW_MATCH_COUNT
  cout << format(
    "{} records read from {}\n",
    records.size(), filename);
#endif
  return records;
}
