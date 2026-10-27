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
// 2026-10-24 CNHume  Created LCSOptions class
//
#pragma once

#include "LCS.h"

#include <cstdint>                      // for uint32_t
#include <string>

namespace lcs {

  //
  // LCSOptions carries the comparison-relevant command-line parameters, so
  // LCSFile can accept options without depending on any tool's switch set.
  // Command-line tools derive their Command class from it and add only the
  // switches that are tool concerns.
  //
  class LCSOptions {
  public:
    LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski;
    LCS::Format format = LCS::Format::Sequential;
    std::string file1;
    std::string file2;
    bool ignorecase = false;
    bool ignorespace = false;
    bool isfasta = false;
    bool isjoin = false;
    bool isquote = false;
    bool istext = false;
    bool isverbose = false;
    bool isword = false;
    std::uint32_t join = 0;
    std::uint32_t prefix = 0;
    std::uint32_t suffix = 0;
    // The narrowest parallel heading "[1:2]" fits in five columns; widths
    // below this minimum are rejected rather than silently clamped.
    static constexpr std::uint32_t MIN_WIDTH = 5;
    std::uint32_t width = 80;
    std::uint32_t tabsize = 8;
  };

} // namespace lcs
