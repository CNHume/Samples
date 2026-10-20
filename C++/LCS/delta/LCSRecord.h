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
//
#pragma once

#include "LCS.h"
#include "Statistics.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string
#include <vector>                       // for vector

using namespace std;

class LCSRecord : public LCS {
public:
  typedef vector<string> RECORDS;

  static uint32_t Correspondence(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false, bool isjoin = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0,
    Statistics* statistics = nullptr);
  static uint32_t Difference(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false, bool isjoin = false,
    uint32_t join = 0, uint32_t prefix = 0, uint32_t suffix = 0,
    Statistics* statistics = nullptr);

  static uint32_t Compare(shared_ptr<Delta>* intervals,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false,
    Statistics* statistics = nullptr);

  static RECORDS Read(const string& filename, bool isword);

protected:
  static uint32_t Match(
    MATCHES& indexesOf2MatchedByIndex1,
    STRING_TO_INDEXES_MAP& indexesOf2MatchedByString,
    const RECORDS& r1, const RECORDS& r2,
    bool ignorecase = false, bool ignorespace = false);
};
