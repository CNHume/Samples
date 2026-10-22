// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created the LCS regression test application entry point.
//
// doctest entry point: define the implementation and main() once, here.
// Other test translation units only include <doctest/doctest.h>.
//
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest/doctest.h"

#include <iostream>                     // for cerr
#include <streambuf>

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
