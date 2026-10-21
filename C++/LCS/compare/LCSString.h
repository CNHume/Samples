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
// 2017-07-04 CNHume  Created LCSString class
//
#pragma once

#include "LCS.h"
#include "Statistics.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string, u32string
#include <unordered_map>                //[C++11]
#include <vector>

using namespace std;

class LCSString : public LCS {
public:
  static uint32_t Correspondence(
    shared_ptr<Delta>* intervals, Statistics* statistics,
    const u32string& s1, const u32string& s2,
    LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
    bool ignorecase = false, bool ignorespace = false,
    bool isjoin = false, bool isverbose = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0);

  static uint32_t Difference(
    shared_ptr<Delta>* intervals, Statistics* statistics,
    const u32string& s1, const u32string& s2,
    LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
    bool ignorecase = false, bool ignorespace = false,
    bool isjoin = false, bool isverbose = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0);

  static uint32_t Compare(
    shared_ptr<Delta>* intervals, Statistics* statistics,
    const u32string& s1, const u32string& s2,
    LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
    bool ignorecase = false, bool ignorespace = false,
    bool isverbose = false);

  static u32string Read(const string& filename);
  static u32string StripFasta(const u32string& s);

  //
  // String Methods compare individual Unicode code points rather than
  // UTF-8 bytes, so multi-byte characters are never split or partially
  // matched during the comparison.
  //
protected:
  typedef unordered_map<char32_t, INDEXES> CHAR_TO_INDEXES_MAP;

  static uint64_t Match(
    MATCHES& indexesOf2MatchedByIndex1,
    CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
    const u32string& s1, const u32string& s2,
    bool ignorecase, bool ignorespace);

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
