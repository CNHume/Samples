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
// 2017-07-04 CNHume  Created Text class
// 2015-01-19 CNHume  Created file
//
#include "modes/Text.h"
#include "Diagnostic.h"
#include "Duration.h"
#include "Encoding.h"
#include "algorithms/HuntSzymanski.h"
#include "algorithms/Myers.h"
#include "Normal.h"
#include "algorithms/RickSpace.h"
#include "algorithms/RickTime.h"

#include <algorithm>                    // for min
#include <format>                       // for format
#include <iostream>                     // for cout
#include <stdexcept>                    // for runtime_error
#include <unordered_map>                // for symbol counts
#include <utility>                      // for swap

using namespace std;
using namespace lcs;

namespace lcs::modes {

//
// Compare with STRING_TO_INDEXES_MAP used for RECORDS
//
uint64_t Text::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace) {
  uint64_t count = 0;
  uint32_t index = 0;
  u32string buffer;
  Normal::Normalize(s2, buffer, ignorecase, ignorespace);
  for (const auto& it : buffer)
    indexesOf2MatchedByChar[it].push_back(index++);

  Normal::Normalize(s1, buffer, ignorecase, ignorespace);
  for (const auto& it : buffer) {
    auto& dq2 = indexesOf2MatchedByChar[it];
    indexesOf2MatchedByIndex1.push_back(&dq2);
    count += dq2.size();
  }
#ifdef SHOW_MATCH_COUNT
  cout << format("count = {} of indexesOf2MatchedByIndex1\n", count);
#endif
  return count;
}

uint32_t Text::Correspondence(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const u32string& s1, const u32string& s2,
  Algorithm algorithm,
  bool ignorecase, bool ignorespace, bool isjoin, bool isverbose,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  //
  // A null interval pointer requests length only: skip the span/context
  // construction and return the length directly.
  //
  if (intervals == nullptr)
    return Compare(
      nullptr, statistics, s1, s2, algorithm, ignorecase, ignorespace, isverbose);

  if (ignorespace) {
    //
    // Compare whitespace-collapsed strings, then expand the reported
    // spans back to the original strings.  The number of code points
    // changes under collapse, so indices must be remapped.
    //
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    Normal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    Normal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, statistics, t1, t2, algorithm, ignorecase, false, isverbose);
    auto size1 = t1.size();
    auto size2 = t2.size();
    Delta::Context(*intervals, size1, size2, prefix, suffix);
    if (isjoin)
      *intervals = Delta::Coalesce(*intervals, join);
    MapDeltas(
      *intervals, mapBegin1, mapEnd1, mapBegin2, mapEnd2, s1.size(), s2.size());
    return length;
  }

  auto length = Compare(
    intervals, statistics, s1, s2, algorithm, ignorecase, ignorespace, isverbose);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t Text::Difference(shared_ptr<Delta>* intervals, Statistics* statistics,
  const u32string& s1, const u32string& s2,
  Algorithm algorithm,
  bool ignorecase, bool ignorespace,
  bool isjoin, bool isverbose,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  // A null interval pointer requests length only.
  if (intervals == nullptr)
    return Compare(
      nullptr, statistics, s1, s2, algorithm, ignorecase, ignorespace, isverbose);

  if (ignorespace) {
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    Normal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    Normal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, statistics, t1, t2, algorithm, ignorecase, false, isverbose);
    auto size1 = t1.size();
    auto size2 = t2.size();
    auto deltas = Delta::Complement(*intervals, size1, size2);
    Delta::Context(deltas, size1, size2, prefix, suffix);
    deltas = isjoin ? Delta::Coalesce(deltas, join) : deltas;
    MapDeltas(
      deltas, mapBegin1, mapEnd1, mapBegin2, mapEnd2, s1.size(), s2.size());
    *intervals = deltas;
    return length;
  }

  auto length = Compare(
    intervals, statistics, s1, s2, algorithm, ignorecase, ignorespace, isverbose);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t Text::Compare(
  shared_ptr<Delta>* deltas, Statistics* statistics,
  const u32string& s1, const u32string& s2,
  Algorithm algorithm,
  bool ignorecase, bool ignorespace, bool isverbose) {
  if (isverbose)
    cerr << format("Started at {}\n", Duration::WallClock());
  Duration duration;                    // time the algorithm
  //
  // A null delta pointer is the length-only flag: forward it to the
  // algorithm as a null pairs pointer so it skips reconstruction.
  //
  shared_ptr<Pair> pairs;
  auto ppairs = deltas != nullptr ? &pairs : nullptr;
  uint32_t length;

  if (algorithm == Algorithm::Automatic) {
    //
    // Heuristic dispatch: estimate the edit-distance lower bound and the match
    // count from the symbol counts, then select per LCSAlgorithmSelection.md.
    // Report the choice and its reason under verbose.
    //
    unordered_map<char32_t, uint64_t> count1, count2;
    for (auto c : s1)
      count1[c]++;
    for (auto c : s2)
      count2[c]++;
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
    auto editLowerBound = s1.size() + s2.size() - 2 * sumMin;
    string reason;
    algorithm = Diagnostic::SelectAutomatic(
      s1.size(), s2.size(), matches, symbols, editLowerBound, reason);
    if (isverbose)
      cerr << format("Automatic: selected {} because {}\n",
        Statistics::NameOf(algorithm), reason);
  }

  switch (algorithm) {
  case Algorithm::EditDistance: {
    //
    // Myers is symmetric in its two strings, so no shorter-first swap and no
    // occurrence tables; normalize for case/space, then run the edit-graph
    // sweep.  Find() guards the D budget and aborts on a dissimilar input.
    //
    u32string ns1, ns2;
    Normal::Normalize(s1, ns1, ignorecase, ignorespace);
    Normal::Normalize(s2, ns2, ignorecase, ignorespace);
    length = algorithms::Myers::Find(ppairs, statistics, ns1, ns2);
    break;
  }
  case Algorithm::Matches: {
    CHAR_TO_INDEXES_MAP indexesOf2MatchedByChar;
    MATCHES indexesOf2MatchedByIndex1;  // holds references into indexesOf2MatchedByChar
    auto count = Match(
      indexesOf2MatchedByIndex1, indexesOf2MatchedByChar, s1, s2, ignorecase, ignorespace);
    Diagnostic::WarnMatchCount(count, s1.size(), s2.size());
    length = algorithms::HuntSzymanski::Find(ppairs, indexesOf2MatchedByIndex1);
    if (statistics) {
      statistics->algorithm = Algorithm::Matches;
      statistics->matches = count;
    }
    break;
  }
  case Algorithm::RickSpace:
  case Algorithm::RickTime: {
    //
    // Both Rick variants assume m <= n: scan the shorter string, table the longer.
    // Normalize both strings first, then swap on the normalized lengths because
    // whitespace collapse can change which is shorter.
    //
    u32string ns1, ns2;
    Normal::Normalize(s1, ns1, ignorecase, ignorespace);
    Normal::Normalize(s2, ns2, ignorecase, ignorespace);
    auto isswapped = ns2.size() < ns1.size();
    const auto& shorter = isswapped ? ns2 : ns1;
    const auto& longer = isswapped ? ns1 : ns2;
    length = algorithm == Algorithm::RickTime ?
      algorithms::RickTime::Find(ppairs, statistics, shorter, longer) :
      algorithms::RickSpace::Find(ppairs, statistics, shorter, longer);
    if (algorithm == Algorithm::RickSpace && statistics)
      //
      // RickSpace is memory-safe but slow on near-identical inputs, the
      // regime where RickTime is ~2.6x faster when it fits.
      //
      Diagnostic::SuggestRickTime(
        length, shorter.size(), longer.size(),
        statistics->candidates, statistics->symbols);
    if (isswapped)
      for (auto pair = pairs; pair != nullptr; pair = pair->next)
        swap(pair->begin1, pair->begin2);
    break;
  }
  default:
    throw runtime_error("Unknown LCS algorithm");
  }

  if (deltas != nullptr)
    *deltas = Delta::Coalesce(pairs);
  duration.Stop();
  if (statistics)
    statistics->elapsedMicroseconds = (uint64_t)duration.ElapsedMicroseconds();
  if (isverbose)
    cerr << format("Finished at {}\n", Duration::WallClock());
  return length;
}

u32string Text::Read(const string& filename) {
  auto bytes = Encoding::ReadBytes(filename);
  auto [encoding, bomLength] = Encoding::GetEncoding(bytes);

  u32string codePoints;
  if (Encoding::IsWide(encoding))
    codePoints = Encoding::DecodeCodePoints(bytes, encoding);
  else {
    if (bomLength > 0)
      bytes.erase(0, (size_t)bomLength);
    codePoints = Encoding::DecodeUtf8(bytes);
  }
#ifdef KEEP_CRLF
  //
  // Treat file as one long string without end-of-line normalization.
  // CR and LF can be ignored via -b.
  //
  return codePoints;
#else
  //
  // Normalize CRLF to LF so -t text will ignore end-of-line differences
  // as is done for Record::Read()
  //
  return Encoding::NormalizeCrlf(codePoints);
#endif
}

//
// StripFasta() removes FASTA header lines, which begin with '>' in column
// zero, and joins the remaining sequence lines into a single string with
// whitespace removed.  Wrapped sequence is thereby unrolled into one
// unbroken sequence, and multi-FASTA records are concatenated.
//
u32string Text::StripFasta(const u32string& s) {
  u32string buffer;
  buffer.reserve(s.size());
  auto n = s.size();
  size_t i = 0;
  while (i < n) {
    auto j = s.find(U'\n', i);
    if (j == u32string::npos)
      j = n;
    if (i < j && s[i] != U'>') {        // sequence line
      for (auto k = i; k < j; k++) {
        auto c = s[k];
        if (c != U' ' && c != U'\t' && c != U'\r')
          buffer.push_back(c);
      }
    }

    // advance past the line terminator
    i = j < n ?
      j + 1 : n;
  }
  return buffer;
}

void Text::MapDeltas(shared_ptr<Delta> deltas,
  const vector<uint32_t>& mapBegin1, const vector<uint32_t>& mapEnd1,
  const vector<uint32_t>& mapBegin2, const vector<uint32_t>& mapEnd2,
  size_t size1, size_t size2) {
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    MapSide(next->begin1, next->end1, mapBegin1, mapEnd1, size1);
    MapSide(next->begin2, next->end2, mapBegin2, mapEnd2, size2);
  }
}

void Text::MapSide(uint32_t& begin, uint32_t& end,
  const vector<uint32_t>& mapBegin, const vector<uint32_t>& mapEnd,
  size_t size) {
  if (begin < end) {
    begin = mapBegin[begin];
    end = mapEnd[end - 1];
  }
  else {
    // Empty interval: map to the corresponding original position.
    auto pos = begin < mapBegin.size() ?
      mapBegin[begin] : (uint32_t)size;
    begin = pos;
    end = pos;
  }
}

} // namespace lcs::modes
