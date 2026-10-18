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
// 2026-10-17 CNHume  Created Version class
//
#pragma once

#include <string>

//
// Version renders the program's GNU-style version banner.
//
class Version {
public:
  static std::string ToString();

private:
  static constexpr const char* programName = "compare";
  static constexpr const char* programVersion = "1.0";
  static constexpr const char* copyrightYears = "2017-2026";
  static constexpr const char* author = "Christopher N. Hume";
};
