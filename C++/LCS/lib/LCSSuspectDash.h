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
// 2026-10-25 CNHume  Created LCSSuspectDash helper
//
#pragma once

#include <format>                       // for format
#include <string>
#include <string_view>

namespace lcs {

  //
  // SuspectDashMessage() returns a diagnostic for a command-line argument
  // that begins with a dash-like Unicode character instead of the ASCII
  // hyphen-minus (-) that introduces a switch.  Returns an empty string when
  // the argument does not begin with one of these characters.
  //
  inline std::string SuspectDashMessage(const char* token) {
    // UTF-8 encodings of the dash-like characters we recognize.
    constexpr std::string_view dashes[] = {
      "\xE2\x88\x92",   // U+2212 MINUS SIGN
      "\xE2\x80\x93",   // U+2013 EN DASH
      "\xE2\x80\x94",   // U+2014 EM DASH
      "\xE2\x80\x90",   // U+2010 HYPHEN
    };
    const std::string_view view(token);
    auto suspect = false;
    for (auto dash : dashes)
      if (view.starts_with(dash)) {
        suspect = true;
        break;
      }
    if (!suspect)
      return {};
    return std::format("Error: Unknown argument '{}'.", token);
  }

} // namespace lcs
