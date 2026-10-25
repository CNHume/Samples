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
// 2017-07-04 CNHume  Moved methods to LCSRecord and LCSString subclasses
// 2017-06-29 CNHume  Added Command class
// 2015-01-19 CNHume  Refactored Show() and Delta::Complement()
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2015-01-16 CNHume  Avoid construction of superfluous Pairs
// 2015-01-15 CNHume  Settled on LCS as the name of the class
// 2015-01-06 CNHume  Added prefix and suffix support to Complement()
// 2015-01-02 CNHume  Implemented the Normal() method
// 2014-12-19 CNHume  Created file
//
#pragma once
#define FILTER_PAIRS
//#define SHOW_MATCHES
//#define SHOW_MATCH_COUNT              // See Match() in LCSFile, LCSRecord, and LCSText
//#define SHOW_PAIRS
//#define SHOW_PREFIXENDS

#include "Delta.h"

#include <cstdint>                      // for uint32_t
#include <deque>
#include <memory>                       // for shared_ptr
#include <string>
#include <unordered_map>                //[C++11]

namespace lcs {

class LCS {
public:
  // The LCS algorithm selected at run time.
  enum class Algorithm {
    HuntSzymanski,      // Dynamic Programming over match lists
    RickSpace,          // Rick 2000, contours + Hirschberg, linear space
    RickTime,           // Rick 1995, diagonal sweep, min{pm, p(n-p)} time
  };

  // The output format selected at run time.
  enum class Format {
    Series,             // sequential: one side under the other
    Parallel,           // side-by-side columns
    CIGAR,              // CIGAR alignment string (reserved)
  };

  typedef std::deque<std::shared_ptr<Pair>> PAIRS;
  typedef std::deque<std::uint32_t> INDEXES;
  typedef std::unordered_map<std::string, INDEXES> STRING_TO_INDEXES_MAP;
  typedef std::deque<INDEXES*> MATCHES;
};

} // namespace lcs
