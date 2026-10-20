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
// 2017-07-09 CNHume  Created LCSFile class
// 2017-07-04 CNHume  Created LCSRecord class
// 2017-06-29 CNHume  Added Command class
// 2015-04-29 CNHume  Added support for isjoin
// 2015-04-25 CNHume  Repurposed infix as join
// 2015-04-18 CNHume  Added parse_uint32()
// 2014-12-18 CNHume  Created file
//
// Purpose:
//
// This program implements a solution to the Longest Common Subsequence
// (LCS) Problem based on the Hunt and Szymanski algorithm.  Please see
// the overview provided in "Doc/LCS Overview.md"
//
#include "LCSFile.h"
#include "Environment.h"
#include "Statistics.h"

#include <cstdlib>                      // for EXIT_FAILURE, EXIT_SUCCESS
#include <format>                       // for format
#include <iostream>                     // for cout
#include <locale>

//
// command line application
//
int main(int argc, char* argv[]) {
  auto errorLevel = EXIT_FAILURE;
  // Force the global runtime and standard streams to use UTF-8
  // ".UTF-8", and "en_US.UTF-8" configure UTF-8 encoding
  try {
    std::locale::global(std::locale(".UTF-8"));
    std::cout.imbue(std::locale());
  }
  catch (const std::exception&) {
    // Fallback for environments with strict minimal locale installations
    std::locale::global(std::locale(""));
    std::cout.imbue(std::locale());
  }

  try {
    Command command;
    command.Parse(argc, argv);
    if (command.isenvironment)
      cout << Environment::ToString();

    shared_ptr<Delta> intervals;
    auto pintervals = command.islength ?
      nullptr : &intervals;

    Statistics statistics;
    auto length = LCSFile::Difference(pintervals, &statistics, command);

    cout << format(std::locale(), "LCS Length = {:L}\n", length);
#ifdef SHOW_DURATION
    statistics.ShowDuration();
#endif
    errorLevel = EXIT_SUCCESS;
  }
  catch (exception& ex) {
    cerr << ex.what() << '\n';
  }
  catch (...) {
    cout << "exception" << '\n';
    throw;
  }
#ifdef _DEBUG
  cout << "Press Enter";
  char c;
  cin.getline(&c, 1);
#endif
  return errorLevel;
}
