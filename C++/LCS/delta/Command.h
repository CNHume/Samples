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
// 2026-10-24 CNHume  Derive from LCSOptions; library parameters move to the base
// 2017-06-29 CNHume  Created file
//
#pragma once

#include "LCSOptions.h"

#include <cstdint>                      // for uint32_t
#include <string>

//
// Command parses the delta tool's command line.  The comparison parameters
// are inherited from lcs::LCSOptions; only the tool-specific switches are
// declared here.  delta is Hunt-Szymanski and record-only, so it pins the
// algorithm to Matches even though the base class now defaults to Automatic.
//
class Command : public lcs::LCSOptions {
public:
  Command() {
    algorithm = lcs::LCS::Algorithm::Matches;
  }

  bool isenvironment = false;
  bool ishelp = false;
  bool islength = false;
  bool isversion = false;

  void Parse(int argc, char* argv[]);

protected:
  static std::string arg_msg(const std::string& s, const std::string& name = "argument");
  static std::uint32_t parse_uint32(const char* s, const std::string& name = "argument");
};
