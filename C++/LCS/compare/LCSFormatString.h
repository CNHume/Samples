// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-08 CNHume  Created LCSFormatString class
//
#pragma once

#include "LCSString.h"

#include <format>                       //[C++20]

using namespace std;

class LCSFormatString : public LCSString {
public:
  static u32string Select(shared_ptr<Delta> deltas,
    const u32string& s1, const u32string& s2, bool isright = false);
  static uint32_t Show(shared_ptr<Delta> deltas,
    const u32string& s1, const u32string& s2,
    const string& label1, const string& label2);

protected:
  static void Series(uint32_t counter,
    const string& label1, const u32string& s1, uint32_t begin1, uint32_t end1,
    const string& label2, const u32string& s2, uint32_t begin2, uint32_t end2);
  static void Side(string emblem, uint32_t counter, const string& label,
    const u32string& s, uint32_t begin, uint32_t end);
  static void Head(string emblem, uint32_t counter, const string& label,
    uint32_t begin, uint32_t end);
  static void Body(const u32string& s, uint32_t begin, uint32_t end);
};
