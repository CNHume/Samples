// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile class
// 2017-07-04 CNHume  Created LCSRecord class
// 2015-01-19 CNHume  Created file
//
#include "LCSRecord.h"
#include "Duration.h"
#include "LCSEncoding.h"
#include "LCSNormal.h"

#include <format>                       // for format
#include <fstream>                      // for ifstream
#include <iostream>                     // for cout
#include <sstream>                      // for istringstream
#include <stdexcept>                    // for runtime_error

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
  if (intervals == nullptr)
    return Compare(nullptr, r1, r2, ignorecase, ignorespace);

  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSRecord::Compare(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
#ifdef SHOW_DURATION
  Duration duration{ "LCSRecord::Compare", (long)(r1.size() + r2.size()) };
#endif
  STRING_TO_INDEXES_MAP indexesOf2MatchedByString;
  MATCHES indexesOf2MatchedByIndex1;    // indexesOf2MatchedByIndex1 holds references into indexesOf2MatchedByString
  [[maybe_unused]] auto count = Match(
    indexesOf2MatchedByIndex1, indexesOf2MatchedByString, r1, r2, ignorecase, ignorespace);
  shared_ptr<Pair> pairs;
  auto ppairs = intervals != nullptr ? &pairs : nullptr;
  auto length = FindLCS(ppairs, indexesOf2MatchedByIndex1);
  if (intervals != nullptr)
    *intervals = Delta::Coalesce(pairs);
#ifdef SHOW_DURATION
  duration.Show();
#endif
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
