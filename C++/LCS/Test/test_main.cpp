// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created the LCS test application entry point.
//
// The test application dispatches on its first argument:
//
//   test                 run the doctest regression suite (default)
//   test stress [N] [S]  randomized text stress: N trials, seed S
//   test recordstress    randomized record stress: N trials, seed S
//   test perf <which>    performance benchmarks (rick | measure | filterpairs)
//
// Regression tests use doctest; stress and perf are standalone entry points
// (test_stress.cpp and test_perf.cpp) that share the same library sources.
//
#define DOCTEST_CONFIG_IMPLEMENT
#include "doctest/doctest.h"

#include <cstring>                      // for strcmp
#include <iostream>                     // for cerr
#include <streambuf>

using namespace std;

namespace {
  //
  // The LCS library writes CLI diagnostics (such as Diagnostic::SuggestRickTime)
  // to std::cerr.  Redirect them to a null buffer so they do not flood the
  // test report; test failures still surface through doctest on std::cout.
  //
  class NullBuffer : public std::streambuf {
  protected:
    int_type overflow(int_type ch) override { return ch; }
  };

  class CerrRedirect {
    NullBuffer buffer;
    std::streambuf* prior;
  public:
    CerrRedirect() : prior(std::cerr.rdbuf(&buffer)) {}
    ~CerrRedirect() { std::cerr.rdbuf(prior); }
  };

  CerrRedirect redirectCerr;
}

// Subcommand entry points, defined in test_stress.cpp and test_perf.cpp.
int runStress(int argc, char* argv[]);
int runRecordStress(int argc, char* argv[]);
int runPerf(int argc, char* argv[]);

int main(int argc, char* argv[]) {
  //
  // Dispatch on the first argument.  Each subcommand receives the remainder
  // of the argument vector, with its own name as argv[0].
  //
  if (argc >= 2 && strcmp(argv[1], "stress") == 0)
    return runStress(argc - 1, argv + 1);
  if (argc >= 2 && strcmp(argv[1], "recordstress") == 0)
    return runRecordStress(argc - 1, argv + 1);
  if (argc >= 2 && strcmp(argv[1], "perf") == 0)
    return runPerf(argc - 1, argv + 1);

  // Default: run the doctest regression suite.
  doctest::Context context;
  context.applyCommandLine(argc, argv);
  auto result = context.run();
  if (context.shouldExit())
    return result;
  return result;
}
