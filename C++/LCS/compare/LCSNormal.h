// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-02 CNHume  Created LCSNormal class
//
#pragma once

#include <string>

using namespace std;

class LCSNormal {
public:
  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
  static void NormalCase(string& input);
  static void NormalSpace(const string& input, string& output);
};
