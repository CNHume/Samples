// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString class
// 2015-01-19 CNHume  Created file
//
#include "LCSString.h"
#include "Duration.h"
#include "LCSEncoding.h"
#include "LCSHuntSzymanski.h"
#include "LCSNormal.h"
#include "LCSRickSpace.h"

#include <format>                       // for format
#include <iostream>                     // for cout
#include <stdexcept>                    // for runtime_error
#include <utility>                      // for swap

//
// Compare with STRING_TO_INDEXES_MAP used for RECORDS
//
uint32_t LCSString::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace) {
  uint32_t count = 0;
  uint32_t index = 0;
  u32string buffer;
  LCSNormal::Normal(s2, buffer, ignorecase, ignorespace);
  for (const auto& it : buffer)
    indexesOf2MatchedByChar[it].push_back(index++);

  LCSNormal::Normal(s1, buffer, ignorecase, ignorespace);
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

uint32_t LCSString::Correspondence(shared_ptr<Delta>* intervals,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix, LCS::Algorithm algorithm) {
  if (ignorespace) {
    // Compare whitespace-collapsed strings, then expand the reported
    // spans back to the original strings.  The number of code points
    // changes under collapse, so indices must be remapped.
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    LCSNormal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    LCSNormal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, t1, t2, ignorecase, false, algorithm);
    auto size1 = t1.size();
    auto size2 = t2.size();
    Delta::Context(*intervals, size1, size2, prefix, suffix);
    if (isjoin)
      *intervals = Delta::Coalesce(*intervals, join);
    MapDeltas(*intervals, mapBegin1, mapEnd1, mapBegin2, mapEnd2,
      s1.size(), s2.size());
    return length;
  }

  auto length = Compare(intervals, s1, s2, ignorecase, ignorespace, algorithm);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t LCSString::Difference(shared_ptr<Delta>* intervals,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix, LCS::Algorithm algorithm) {
  if (ignorespace) {
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    LCSNormal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    LCSNormal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, t1, t2, ignorecase, false, algorithm);
    auto size1 = t1.size();
    auto size2 = t2.size();
    auto deltas = Delta::Complement(*intervals, size1, size2);
    Delta::Context(deltas, size1, size2, prefix, suffix);
    deltas = isjoin ? Delta::Coalesce(deltas, join) : deltas;
    MapDeltas(deltas, mapBegin1, mapEnd1, mapBegin2, mapEnd2,
      s1.size(), s2.size());
    *intervals = deltas;
    return length;
  }

  auto length = Compare(intervals, s1, s2, ignorecase, ignorespace, algorithm);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSString::Compare(shared_ptr<Delta>* deltas,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace, LCS::Algorithm algorithm) {
#if defined(SHOW_DURATION) || defined(DEMO_STOPWATCH)
  Duration duration{ "LCSString::Compare", 1 << 16 };
#endif
#ifdef SHOW_DURATION
  duration.Show();
#endif
#ifdef DEMO_STOPWATCH
  // Demonstrates the Stopwatch with a fixed 5 ms sleep.  Isolated under its
  // own macro so test/fuzz runs do not pay the sleep on every Compare() call.
  duration.DemoStopwatch();
#endif
  shared_ptr<Pair> pairs;
  auto ppairs = deltas != nullptr ? &pairs : nullptr;
  uint32_t length;

  switch (algorithm) {
  case LCS::Algorithm::HuntSzymanski: {
    CHAR_TO_INDEXES_MAP indexesOf2MatchedByChar;
    MATCHES indexesOf2MatchedByIndex1;  // holds references into indexesOf2MatchedByChar
    [[maybe_unused]] auto count = Match(
      indexesOf2MatchedByIndex1, indexesOf2MatchedByChar, s1, s2, ignorecase, ignorespace);
    length = LCSHuntSzymanski::Find(ppairs, indexesOf2MatchedByIndex1);
    break;
  }
  case LCS::Algorithm::RickSpace: {
    // LCSRickSpace::Find() assumes m <= n: scan the shorter string, table the
    // longer.  Swap when needed and un-swap the reported positions here.
    auto isswapped = s2.size() < s1.size();
    const auto& shorter = isswapped ? s2 : s1;
    const auto& longer = isswapped ? s1 : s2;
    // Normalize exactly as Match() would, then build O(ns) occurrence tables
    // directly from the code points, bypassing the O(r) match-list
    // materialization so very long strings stay feasible.
    u32string ns1, ns2;
    LCSNormal::Normal(shorter, ns1, ignorecase, ignorespace);
    LCSNormal::Normal(longer, ns2, ignorecase, ignorespace);
    length = LCSRickSpace::Find(ppairs, ns1, ns2);
    if (isswapped)
      for (auto pair = pairs; pair != nullptr; pair = pair->next)
        swap(pair->begin1, pair->begin2);
    break;
  }
  case LCS::Algorithm::RickTime:
    throw runtime_error("LCSRickTime is not yet implemented");
  default:
    throw runtime_error("unknown LCS algorithm");
  }

  if (deltas != nullptr)
    *deltas = Delta::Coalesce(pairs);
  return length;
}

u32string LCSString::Read(const string& filename) {
  auto bytes = LCSEncoding::ReadBytes(filename);
  auto [encoding, bomLength] = LCSEncoding::GetEncoding(bytes);

  u32string codePoints;
  if (LCSEncoding::IsWide(encoding))
    codePoints = LCSEncoding::DecodeCodePoints(bytes, encoding);
  else {
    if (bomLength > 0)
      bytes.erase(0, (size_t)bomLength);
    codePoints = LCSEncoding::DecodeUtf8(bytes);
  }
#ifdef KEEP_CRLF
  // Treat file as one long string without end-of-line normalization.
  // CR and LF can be ignored via -b.
  return codePoints;
#else
  // Normalize CRLF to LF so -t text will ignore end-of-line differences
  // as is done for LCSRecord::Read()
  return LCSEncoding::NormalizeCrlf(codePoints);
#endif
}

void LCSString::MapDeltas(shared_ptr<Delta> deltas,
  const vector<uint32_t>& mapBegin1, const vector<uint32_t>& mapEnd1,
  const vector<uint32_t>& mapBegin2, const vector<uint32_t>& mapEnd2,
  size_t size1, size_t size2) {
  for (auto next = deltas; next != nullptr;
    next = dynamic_pointer_cast<Delta>(next->next)) {
    MapSide(next->begin1, next->end1, mapBegin1, mapEnd1, size1);
    MapSide(next->begin2, next->end2, mapBegin2, mapEnd2, size2);
  }
}

void LCSString::MapSide(uint32_t& begin, uint32_t& end,
  const vector<uint32_t>& mapBegin, const vector<uint32_t>& mapEnd,
  size_t size) {
  if (begin < end) {
    begin = mapBegin[begin];
    end = mapEnd[end - 1];
  }
  else {
    // Empty interval: map to the corresponding original position.
    auto pos = begin < mapBegin.size() ? mapBegin[begin] : (uint32_t)size;
    begin = pos;
    end = pos;
  }
}
