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
// 2026-10-02 CNHume  Created LCSNormal class
//
#pragma once

#include <cstdint>                      // for uint32_t
#include <string>
#include <vector>

namespace lcs {

  class LCSNormal {
  public:
    static void Normal(const std::string& input, std::string& output,
      bool ignorecase = false, bool ignorespace = false);
    static void NormalCase(std::string& input);
    static void NormalSpace(const std::string& input, std::string& output);
    static bool IsSpace(char32_t c);

    // Code-point overloads used by LCSText character comparison.
    static void Normal(const std::u32string& input, std::u32string& output,
      bool ignorecase = false, bool ignorespace = false);
    static void NormalCase(std::u32string& input);
    static void NormalSpace(const std::u32string& input, std::u32string& output);
    static void NormalSpace(const std::u32string& input, std::u32string& output,
      std::vector<std::uint32_t>& mapBegin, std::vector<std::uint32_t>& mapEnd);
    static char32_t ToLower(char32_t c);
  };

} // namespace lcs
