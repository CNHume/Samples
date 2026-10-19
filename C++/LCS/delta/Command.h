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
// 2017-06-29 CNHume  Created file
//
#pragma once

#include <cstdint>                      // for uint32_t
#include <string>

using namespace std;

class Command {
public:
  string file1;
  string file2;
  bool isjoin;
  uint32_t join;
  uint32_t prefix;
  uint32_t suffix;
  bool ignorecase;
  bool ignorespace;
  bool islength;
  bool isversion;
  bool isenvironment;
  bool isword;

protected:
  static const uint32_t joinDefault = 0, affixDefault = 0;

  static string arg_msg(const string& s, const string& name = "argument");
  static uint32_t parse_uint32(const char* s, const string& name = "argument");

public:
  Command() :
    isjoin(false),
    join(joinDefault),
    prefix(affixDefault),
    suffix(affixDefault),
    ignorecase(false),
    ignorespace(false),
    islength(false),
    isversion(false),
    isenvironment(false),
    isword(false) {
  }

  void Parse(int argc, char* argv[]);
};
