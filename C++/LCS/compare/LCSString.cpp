// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
// 2015-01-19 CNHume  Created file
//
#include "LCSString.h"
#include "LCSHuntSzymanski.h"
#include "LCSRick.h"

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
  uint32_t join, uint32_t prefix, uint32_t suffix, bool isrick) {
  if (ignorespace) {
    // Compare whitespace-collapsed strings, then expand the reported
    // spans back to the original strings.  The number of code points
    // changes under collapse, so indices must be remapped.
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    LCSNormal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    LCSNormal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, t1, t2, ignorecase, false, isrick);
    auto size1 = t1.size();
    auto size2 = t2.size();
    Delta::Context(*intervals, size1, size2, prefix, suffix);
    if (isjoin)
      *intervals = Delta::Coalesce(*intervals, join);
    MapDeltas(*intervals, mapBegin1, mapEnd1, mapBegin2, mapEnd2,
      s1.size(), s2.size());
    return length;
  }

  auto length = Compare(intervals, s1, s2, ignorecase, ignorespace, isrick);
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
  uint32_t join, uint32_t prefix, uint32_t suffix, bool isrick) {
  if (ignorespace) {
    u32string t1, t2;
    vector<uint32_t> mapBegin1, mapEnd1, mapBegin2, mapEnd2;
    LCSNormal::NormalSpace(s1, t1, mapBegin1, mapEnd1);
    LCSNormal::NormalSpace(s2, t2, mapBegin2, mapEnd2);
    auto length = Compare(intervals, t1, t2, ignorecase, false, isrick);
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

  auto length = Compare(intervals, s1, s2, ignorecase, ignorespace, isrick);
  auto size1 = s1.size();               // empty final delta
  auto size2 = s2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSString::Compare(shared_ptr<Delta>* deltas,
  const u32string& s1, const u32string& s2,
  bool ignorecase, bool ignorespace, bool isrick) {
  // Swap (and un-swap below) so the shorter sequence is scanned and the
  // longer is tabled, as Rick's time bound assumes m <= n.
  auto swapped = s2.size() < s1.size();
  const auto& shorter = swapped ? s2 : s1;
  const auto& longer = swapped ? s1 : s2;

  CHAR_TO_INDEXES_MAP indexesOf2MatchedByChar;
  MATCHES indexesOf2MatchedByIndex1;    // holds references into indexesOf2MatchedByChar
  [[maybe_unused]] auto count = Match(
    indexesOf2MatchedByIndex1, indexesOf2MatchedByChar, shorter, longer, ignorecase, ignorespace);

  shared_ptr<Pair> pairs;
  auto ppairs = deltas != nullptr ? &pairs : nullptr;
  auto length = isrick ?
    LCSRick::Find(
      ppairs, indexesOf2MatchedByIndex1,
        (uint32_t)shorter.size(), (uint32_t)longer.size()) :
    LCSHuntSzymanski::Find(ppairs, indexesOf2MatchedByIndex1);

  if (deltas != nullptr) {
    if (swapped)
      for (auto pair = pairs; pair != nullptr; pair = pair->next)
        swap(pair->begin1, pair->begin2);
    *deltas = Delta::Coalesce(pairs);
  }
  return length;
}

u32string LCSString::Read(const string& filename) {
  auto [encoding, bomLength] = LCSEncoding::PeekEncoding(filename);
  ifstream input(filename, ios::binary);
  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }
  string bytes((istreambuf_iterator<char>(input)), istreambuf_iterator<char>());
  input.close();

  u32string codePoints;
  if (LCSEncoding::IsWide(encoding))
    codePoints = LCSEncoding::DecodeCodePoints(bytes, encoding);
  else {
    if (bomLength > 0)
      bytes.erase(0, (size_t)bomLength);
    codePoints = LCSEncoding::DecodeUtf8(bytes);
  }
#ifdef KEEP_CRLF
  // Treat the file as one large string: leave end-of-line characters
  // untouched.  They are non-printing, so -b can ignore them if desired.
  return codePoints;
#else
  // Normalize CRLF to LF so -t text comparison ignores line-ending
  // differences, matching the line and word record readers.
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
