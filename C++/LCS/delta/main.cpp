// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
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
#ifdef _DEBUG
    std::locale current_locale;
    cout << format("locale = {}\n", current_locale.name());
#endif
    Command command;
    command.Parse(argc, argv);

    shared_ptr<Delta> intervals;
    [[maybe_unused]] auto length = LCSFile::Difference(&intervals, command);
#ifdef _DEBUG
    cout << format("LCS Length = {}\n", length);
#endif
    errorLevel = EXIT_SUCCESS;
  }
  catch (exception& ex) {
    cout << ex.what() << endl;
  }
  catch (...) {
    cout << "exception" << endl;
    throw;
  }
#ifdef _DEBUG
  cout << "Press Enter";
  char c;
  cin.getline(&c, 1);
#endif
  return errorLevel;
}
