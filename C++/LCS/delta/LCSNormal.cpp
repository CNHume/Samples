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
#include "LCSNormal.h"

#include <cctype>                       // for isspace, tolower

//
// Normal() applies the ignorecase and ignorespace options,
// normalizing records prior to their comparison in Match()
//
void LCSNormal::Normal(const string& input, string& output,
  bool ignorecase, bool ignorespace) {
  if (ignorespace)
    NormalSpace(input, output);
  else
    output = input;

  if (ignorecase)
    NormalCase(output);
}

void LCSNormal::NormalCase(string& input) {
  for (auto& c : input)
    c = tolower(c);                     // normal case
}

void LCSNormal::NormalSpace(const string& input, string& output) {
  // outer (right) trim
  auto end = 0;
  for (auto it = input.rbegin(); it != input.rend(); it++)
    if (!isspace(*it)) {
      end = input.rend() - it;
      break;
    }

  output.clear();
  output.reserve(input.size());
  // inner (left) trims
  bool allowSpace = false;
  for (auto index = 0; index < end; index++) {
    auto c = input[index];

    if (!isspace(c)) {
      output.push_back(c);
      allowSpace = true;
    }
    else if (allowSpace) {
      // normalized space
      output.push_back(' ');
      allowSpace = false;
    }
  }
}

bool LCSNormal::IsSpace(char32_t c) {
  return c == U' ' || (c >= U'\t' && c <= U'\r') || c == U'\u00A0';
}
