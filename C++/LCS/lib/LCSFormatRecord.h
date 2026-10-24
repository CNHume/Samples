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
// 2022-07-04 CNHume  Created LCSFormatRecord class
//
#pragma once

#include "LCSRecord.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string

namespace lcs {

class LCSFormatRecord : public LCSRecord {
public:
  static RECORDS Select(std::shared_ptr<Delta> deltas,
    const RECORDS& r1, const RECORDS& r2, bool isright = false);
  static std::uint32_t Show(std::shared_ptr<Delta> deltas,
    const RECORDS& r1, const RECORDS& r2,
    const std::string& label1, const std::string& label2);

protected:
  static void Series(std::uint32_t counter,
    const std::string& label1, const RECORDS& r1, std::uint32_t begin1, std::uint32_t end1,
    const std::string& label2, const RECORDS& r2, std::uint32_t begin2, std::uint32_t end2);
  static void Side(std::string emblem, std::uint32_t counter, const std::string& label,
    const RECORDS& list, std::uint32_t begin, std::uint32_t end);
  static void Head(std::string emblem, std::uint32_t counter, const std::string& label,
    std::uint32_t begin, std::uint32_t end);
  static void Body(const RECORDS& records, std::uint32_t begin, std::uint32_t end);
};

} // namespace lcs
