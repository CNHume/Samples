// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#pragma once

#include "Command.h"
#include "LCSFormat.h"
#include <fstream>
#include <sstream>
#include <iterator>

using namespace std;

class LCSFile : protected LCSFormat {
public:
  void Difference(const Command command);

protected:
  static RECORDS Read(const string& filename, bool isword);
};
