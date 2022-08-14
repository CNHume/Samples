// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-05-16 CNHume  Added Processor class
//
#pragma once

#include "Command.h"
#include "Tree.h"

#include <stdint.h>
#include <fstream>
#include <iostream>                     // for cout
#include <chrono>

using namespace std;
using namespace chrono;

class Processor {
protected:
  void normalize(string& buffer);
  Tree::STRINGSET read(const string& filename);

public:
  void Execute(Command commmand);
};
