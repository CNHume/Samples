// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created encoding regression tests.
//
// The TankNew/TankOld fixtures hold the same source text re-encoded as
// UTF-8 (with and without BOM), UTF-16 LE/BE, and UTF-32 LE/BE.  Every
// variant must decode to the identical sequence of code points.
//
#include "doctest/doctest.h"
#include "LCSString.h"

#include <string>
#include <vector>

using namespace std;

namespace {
  string fixture(const string& name) {
    return string(TEST_DATA_DIR) + "/Test/" + name;
  }

  void checkVariantsDecodeIdentically(const vector<string>& names) {
    u32string reference;
    bool first = true;
    for (auto& name : names) {
      auto codePoints = LCSString::Read(fixture(name));
      CAPTURE(name);
      if (first) {
        reference = codePoints;
        first = false;
      }
      else
        CHECK(codePoints == reference);
    }
    CHECK(!reference.empty());
  }
}

TEST_CASE("TankNew encoding variants decode to identical code points") {
  checkVariantsDecodeIdentically({
    "Encoding/TankNew.cs",
    "Encoding/TankNew_UTF-8.cs",
    "Encoding/TankNew_UTF-16-BE.cs",
    "Encoding/TankNew_UTF-16-LE.cs",
    "Encoding/TankNew_UTF-32-BE.cs",
    "Encoding/TankNew_UTF-32-LE.cs",
  });
}

TEST_CASE("TankOld encoding variants decode to identical code points") {
  checkVariantsDecodeIdentically({
    "Encoding/TankOld.cs",
    "Encoding/TankOld_UTF-8.cs",
  });
}
