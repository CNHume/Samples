// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-09 CNHume  Created LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
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
// Usage:
//
// delta [-w] [-b] [-i] [-j <join>] [-p <prefix>] [-s <suffix>] file1 file2
//
#include "LCSFile.h"
#include <iostream>                     // for cout

//
// command line application
//
int main(int argc, char* argv[]) {
  auto errorLevel = EXIT_FAILURE;

  try {
    setlocale(LC_ALL, "");              // Set preferred locale vs. minimal C locale
#ifdef _DEBUG
    auto locale = setlocale(LC_ALL, NULL);
    cout << "locale: " << locale << endl;
#endif
    Command command;
    command.Parse(argc, argv);

    LCSFile lcs;
    lcs.Difference(command);
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
#ifdef SHOW_COUNTS
  cout << "# Pairs = " << Pair::Pairs << endl;
#endif
  cout << "Press Enter";
  char c;
  cin.getline(&c, 1);
#endif
  return errorLevel;
}
