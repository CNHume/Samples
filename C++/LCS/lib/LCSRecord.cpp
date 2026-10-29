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
#include "Diagnostic.h"
#include "Duration.h"
#include "LCSEncoding.h"
#include "LCSHuntSzymanski.h"
#include "LCSMyers.h"
#include "LCSNormal.h"
#include "LCSRickSpace.h"
#include "LCSRickTime.h"

#include <algorithm>                    // for min
#include <format>                       // for format
#include <fstream>                      // for ifstream
#include <iostream>                     // for cout
#include <sstream>                      // for istringstream
#include <stdexcept>                    // for runtime_error
#include <unordered_map>                // for symbol counts
#include <utility>                      // for swap

using namespace std;
using namespace lcs;

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
uint64_t LCSRecord::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  STRING_TO_INDEXES_MAP& indexesOf2MatchedByString,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  uint64_t count = 0;
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
  cout << format(
    std::locale(), "count = {:L} of indexesOf2MatchedByIndex1\n", count);
#endif
  return count;
}

uint32_t LCSRecord::Correspondence(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const RECORDS& r1, const RECORDS& r2,
  LCS::Algorithm algorithm,
  bool ignorecase, bool ignorespace, bool isjoin, bool isverbose,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  if (intervals == nullptr)
    return Compare(
      nullptr, statistics, r1, r2, algorithm, ignorecase, ignorespace, isverbose);

  auto length = Compare(intervals, statistics, r1, r2, algorithm, ignorecase, ignorespace, isverbose);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t LCSRecord::Difference(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const RECORDS& r1, const RECORDS& r2,
  LCS::Algorithm algorithm,
  bool ignorecase, bool ignorespace, bool isjoin, bool isverbose,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  if (intervals == nullptr)
    return Compare(
      nullptr, statistics, r1, r2, algorithm, ignorecase, ignorespace, isverbose);

  auto length = Compare(
    intervals, statistics, r1, r2, algorithm, ignorecase, ignorespace, isverbose);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSRecord::Compare(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const RECORDS& r1, const RECORDS& r2,
  LCS::Algorithm algorithm,
  bool ignorecase, bool ignorespace, bool isverbose) {
  if (isverbose)
    cerr << format("Started at {}\n", Duration::WallClock());
  Duration duration;                    // time the algorithm
  shared_ptr<Pair> pairs;
  auto ppairs = intervals != nullptr ? &pairs : nullptr;
  uint32_t length;
  bool isswapped = false;

  if (algorithm == LCS::Algorithm::Automatic) {
    //
    // Heuristic dispatch over records: same decision tree as the text path,
    // with record strings as the symbols.
    //
    unordered_map<string, uint64_t> count1, count2;
    for (const auto& rec : r1)
      count1[rec]++;
    for (const auto& rec : r2)
      count2[rec]++;
    uint64_t symbols = 0, matches = 0, sumMin = 0;
    for (const auto& [symbol, count] : count1) {
      symbols++;
      auto found = count2.find(symbol);
      if (found != count2.end()) {
        matches += count * found->second;
        sumMin += min(count, found->second);
      }
    }
    for (const auto& [symbol, count] : count2)
      if (count1.find(symbol) == count1.end())
        symbols++;
    auto editLowerBound = r1.size() + r2.size() - 2 * sumMin;
    string reason;
    algorithm = Diagnostic::SelectAutomatic(
      r1.size(), r2.size(), matches, symbols, editLowerBound, reason);
    if (isverbose)
      cerr << format("Automatic: selected {} because {}\n",
        Statistics::NameOf(algorithm), reason);
  }

  switch (algorithm) {
  case LCS::Algorithm::EditDistance: {
    //
    // Myers is symmetric and needs no match lists; normalize each record for
    // case/space, then run the edit-graph sweep directly.  Normalization
    // preserves record indices, so the positions map 1:1 back to r1/r2.
    //
    RECORDS nr1, nr2;
    nr1.reserve(r1.size());
    nr2.reserve(r2.size());
    string buffer;
    for (const auto& it : r1) {
      LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
      nr1.push_back(buffer);
    }
    for (const auto& it : r2) {
      LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
      nr2.push_back(buffer);
    }
    length = LCSMyers::Find(ppairs, statistics, nr1, nr2);
    break;
  }
  case LCS::Algorithm::Matches:
  case LCS::Algorithm::RickSpace:
  case LCS::Algorithm::RickTime: {
    //
    // The Rick variants assume m <= n, so swap for them; Hunt-Szymanski is
    // symmetric.  Match() consumes the (possibly swapped) inputs, so the swap
    // must precede the shared match-list build.
    //
    isswapped =
      algorithm != LCS::Algorithm::Matches && r2.size() < r1.size();
    const auto& shorter = isswapped ? r2 : r1;
    const auto& longer = isswapped ? r1 : r2;
    STRING_TO_INDEXES_MAP indexesOf2MatchedByString;
    MATCHES indexesOf2MatchedByIndex1;  // holds references into indexesOf2MatchedByString
    auto count = Match(
      indexesOf2MatchedByIndex1, indexesOf2MatchedByString,
      shorter, longer, ignorecase, ignorespace);
    if (algorithm == LCS::Algorithm::Matches) {
      Diagnostic::WarnMatchCount(count, shorter.size(), longer.size());
      length = LCSHuntSzymanski::Find(ppairs, indexesOf2MatchedByIndex1);
      if (statistics) {
        statistics->algorithm = LCS::Algorithm::Matches;
        statistics->matches = count;
      }
    }
    else if (algorithm == LCS::Algorithm::RickSpace) {
      length = LCSRickSpace::Find(
        ppairs, statistics, indexesOf2MatchedByIndex1,
        (uint32_t)shorter.size(), (uint32_t)longer.size());
    }
    else {
      length = LCSRickTime::Find(
        ppairs, statistics, indexesOf2MatchedByIndex1,
        (uint32_t)shorter.size(), (uint32_t)longer.size());
    }
    break;
  }
  default:
    throw runtime_error("Unknown LCS algorithm");
  }

  if (intervals != nullptr) {
    if (isswapped)
      for (auto pair = pairs; pair != nullptr; pair = pair->next)
        swap(pair->begin1, pair->begin2);
    *intervals = Delta::Coalesce(pairs);
  }
  duration.Stop();
  if (statistics)
    statistics->elapsedMicroseconds = (uint64_t)duration.ElapsedMicroseconds();
  if (isverbose)
    cerr << format("Finished at {}\n", Duration::WallClock());
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
