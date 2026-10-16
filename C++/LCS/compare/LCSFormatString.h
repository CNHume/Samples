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
// 2026-10-08 CNHume  Created LCSFormatString class
//
#pragma once

#include "LCSString.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string, u32string

using namespace std;

class LCSFormatString : public LCSString {
public:
  static u32string Select(shared_ptr<Delta> deltas,
    const u32string& s1, const u32string& s2, bool isright = false);
  static uint32_t Show(shared_ptr<Delta> deltas,
    const u32string& s1, const u32string& s2,
    const string& label1, const string& label2);

protected:
  static void Series(uint32_t counter,
    const string& label1, const u32string& s1, uint32_t begin1, uint32_t end1,
    const string& label2, const u32string& s2, uint32_t begin2, uint32_t end2);
  static void Side(string emblem, uint32_t counter, const string& label,
    const u32string& s, uint32_t begin, uint32_t end);
  static void Head(string emblem, uint32_t counter, const string& label,
    uint32_t begin, uint32_t end);
  static void Body(const u32string& s, uint32_t begin, uint32_t end);
};
