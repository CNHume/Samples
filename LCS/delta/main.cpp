// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-09 CNHume  Created LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
// 2017-06-30 CNHume  Added Command class
// 2014-12-18 CNHume  Created file
//
// Purpose:
//
// Compares two files to find their Longest Common Subsequence (LCS)
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
