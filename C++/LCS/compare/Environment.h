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
// 2026-10-19 CNHume  Created Environment class
//
#pragma once

#include <cstdint>                      // for uint64_t
#include <string>

//
// Environment renders a report of the host: the locale, processor type and
// speed, and total physical memory, for both Windows and Linux.
//
class Environment {
public:
  static std::string ToString();
  // Available physical memory in bytes, or 0 when unknown.
  static uint64_t AvailableBytes();
};
