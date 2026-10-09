// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-04 CNHume  Created LCSString subclass
//
#pragma once

#include "LCS.h"
#include "LCSEncoding.h"
#include "LCSNormal.h"

#include <cstdint>                      // for uint32_t
#include <vector>

using namespace std;

class LCSString : protected LCS {
  //
  // String Methods compare individual Unicode code points rather than
  // UTF-8 bytes, so multi-byte characters are never split or partially
  // matched during the comparison.
  //
protected:
  typedef unordered_map<char32_t, INDEXES> CHAR_TO_INDEXES_MAP;

  static uint32_t Match(
    MATCHES& indexesOf2MatchedByIndex1,
    CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
    const u32string& s1, const u32string& s2,
    bool ignorecase, bool ignorespace);

public:
  static uint32_t Correspondence(shared_ptr<Delta>* intervals,
    const u32string& s1, const u32string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);
  static uint32_t Difference(shared_ptr<Delta>* intervals,
    const u32string& s1, const u32string& s2,
    bool ignorecase, bool ignorespace, bool isjoin,
    uint32_t join, uint32_t prefix, uint32_t suffix);

  static uint32_t Compare(shared_ptr<Delta>* intervals,
    const u32string& s1, const u32string& s2,
    bool ignorecase = false, bool ignorespace = false);

  static u32string Read(const string& filename);

private:
  // Map normalized-space Delta intervals back to original-space spans.
  static void MapDeltas(shared_ptr<Delta> deltas,
    const vector<uint32_t>& mapBegin1, const vector<uint32_t>& mapEnd1,
    const vector<uint32_t>& mapBegin2, const vector<uint32_t>& mapEnd2,
    size_t size1, size_t size2);
  static void MapSide(uint32_t& begin, uint32_t& end,
    const vector<uint32_t>& mapBegin, const vector<uint32_t>& mapEnd,
    size_t size);
};
