// (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
//
// 2014-12-06 CNHume  Created file
//
#pragma once
#include <string>
#include <sstream>

using namespace std;

template <typename T>
string join(const T& list, const string& delim = " ") {
  ostringstream ss;

  auto delimit = false;
  for (const auto& it : list) {
    if (delimit)
      ss << delim;
    else
      delimit = true;

    ss << it;
  }

  return ss.str();
}
