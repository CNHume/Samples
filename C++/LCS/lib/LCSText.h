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
// 2026-10-24 CNHume  Renamed LCSString to LCSText
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

namespace lcs {

  class LCSText : public LCS {
  public:
    static std::uint32_t Correspondence(
      std::shared_ptr<Delta>* intervals, Statistics* statistics,
      const std::u32string& s1, const std::u32string& s2,
      LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
      bool ignorecase = false, bool ignorespace = false,
      bool isjoin = false, bool isverbose = false,
      std::uint32_t join = 0, std::uint32_t prefix = 0, std::uint32_t suffix = 0);

    static std::uint32_t Difference(
      std::shared_ptr<Delta>* intervals, Statistics* statistics,
      const std::u32string& s1, const std::u32string& s2,
      LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
      bool ignorecase = false, bool ignorespace = false,
      bool isjoin = false, bool isverbose = false,
      std::uint32_t join = 0, std::uint32_t prefix = 0, std::uint32_t suffix = 0);

    static std::uint32_t Compare(
      std::shared_ptr<Delta>* intervals, Statistics* statistics,
      const std::u32string& s1, const std::u32string& s2,
      LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski,
      bool ignorecase = false, bool ignorespace = false,
      bool isverbose = false);

    static std::u32string Read(const std::string& filename);
    static std::u32string StripFasta(const std::u32string& s);

    //
    // String Methods compare individual Unicode code points rather than
    // UTF-8 bytes, so multi-byte characters are never split or partially
    // matched during the comparison.
    //
  protected:
    typedef std::unordered_map<char32_t, INDEXES> CHAR_TO_INDEXES_MAP;

    static std::uint64_t Match(
      MATCHES& indexesOf2MatchedByIndex1,
      CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar,
      const std::u32string& s1, const std::u32string& s2,
      bool ignorecase, bool ignorespace);

  private:
    // Map normalized-space Delta intervals back to original-space spans.
    static void MapDeltas(std::shared_ptr<Delta> deltas,
      const std::vector<std::uint32_t>& mapBegin1, const std::vector<std::uint32_t>& mapEnd1,
      const std::vector<std::uint32_t>& mapBegin2, const std::vector<std::uint32_t>& mapEnd2,
      std::size_t size1, std::size_t size2);
    static void MapSide(std::uint32_t& begin, std::uint32_t& end,
      const std::vector<std::uint32_t>& mapBegin, const std::vector<std::uint32_t>& mapEnd,
      std::size_t size);
  };

} // namespace lcs
