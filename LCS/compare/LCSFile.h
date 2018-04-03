// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Created LCSFile subclass
//
#pragma once

#include "Command.h"
#include "LCSRecord.h"
#include <fstream>

using namespace std;

class LCSFile : LCSRecord {
public:
  void Correspondence(const Command command);
  void Difference(const Command command);

  static RECORDS Read(const string& filename);
};
