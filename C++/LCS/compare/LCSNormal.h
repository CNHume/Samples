// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-02 CNHume  Created LCSNormal class
//
#pragma once

#include <cstdint>                      // for uint32_t
#include <string>
#include <vector>

using namespace std;

class LCSNormal {
public:
  static void Normal(const string& input, string& output,
    bool ignorecase = false, bool ignorespace = false);
  static void NormalCase(string& input);
  static void NormalSpace(const string& input, string& output);

  // Code-point overloads used by LCSString character comparison.
  static void Normal(const u32string& input, u32string& output,
    bool ignorecase = false, bool ignorespace = false);
  static void NormalCase(u32string& input);
  static void NormalSpace(const u32string& input, u32string& output);
  static void NormalSpace(const u32string& input, u32string& output,
    vector<uint32_t>& mapBegin, vector<uint32_t>& mapEnd);
  static char32_t ToLower(char32_t c);
  static bool IsSpace(char32_t c);
};
