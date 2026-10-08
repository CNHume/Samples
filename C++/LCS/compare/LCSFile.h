// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#pragma once

#include "Command.h"
#include "LCSEncoding.h"
#include "LCSFormat.h"

#include <fstream>
#include <sstream>
#include <iterator>

using namespace std;

class LCSFile : protected LCSFormat {
public:
  static uint32_t  Correspondence(
    shared_ptr<Delta>* intervals, const Command command);
  static uint32_t  Difference(
    shared_ptr<Delta>* intervals, const Command command);

protected:
  static RECORDS Read(const string& filename, bool isword);
};
