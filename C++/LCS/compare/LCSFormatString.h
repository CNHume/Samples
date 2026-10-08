// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-08 CNHume  Created LCSFormatString class
//
#pragma once

#include "LCSString.h"

#include <format>                       //[C++20]

using namespace std;

class LCSFormatString : protected LCSString {
protected:
  static string Select(shared_ptr<Delta> deltas,
    const string& s1, const string& s2, bool isright = false);
};
