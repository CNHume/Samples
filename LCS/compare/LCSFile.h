// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#pragma once

#include "Command.h"
#include "LCSRecord.h"
#include <fstream>
#include <sstream>
#include <iterator>

using namespace std;

class LCSFile : LCSRecord {
public:
  void Correspondence(const Command command);
  void Difference(const Command command);

  static RECORDS Read(const string& filename, bool isword);
};
