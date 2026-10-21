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

#include "LCS.h"                        // for LCS::Algorithm

#include <cstdint>                      // for uint32_t
#include <string>

using namespace std;

class Command {
public:
  LCS::Algorithm algorithm;
  string file1;
  string file2;
  bool ignorecase;
  bool ignorespace;
  bool isscorrespondence;
  bool isenvironment;
  bool isfasta;
  bool ishelp;
  bool isjoin;
  bool islength;
  bool isquote;
  bool istext;
  bool isverbose;
  bool isversion;
  bool isword;
  uint32_t join;
  uint32_t prefix;
  uint32_t suffix;

protected:
  static const uint32_t joinDefault = 0, affixDefault = 0;

  static string arg_msg(const string& s, const string& name = "argument");
  static uint32_t parse_uint32(const char* s, const string& name = "argument");

public:
  Command() :
    algorithm(LCS::Algorithm::HuntSzymanski),
    ignorecase(false),
    ignorespace(false),
    isscorrespondence(false),
    isenvironment(false),
    isfasta(false),
    ishelp(false),
    isjoin(false),
    islength(false),
    isquote(false),
    istext(false),
    isverbose(false),
    isversion(false),
    isword(false),
    join(joinDefault),
    prefix(affixDefault),
    suffix(affixDefault) {}

  void Parse(int argc, char* argv[]);
};
