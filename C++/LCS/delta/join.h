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
// 2014-12-06 CNHume  Created file
//
#pragma once

#include <sstream>
#include <string>

using namespace std;

template <typename T>
string join(const T& list, const string& delim = " ") {
  ostringstream ss;

  auto delimit = false;
  for (const auto& it : list) {
    if (delimit)
      ss << delim;
    else
      delimit = true;

    ss << it;
  }

  return ss.str();
}
