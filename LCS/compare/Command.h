//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-06-29 CNHume  Created file
//
#pragma once

#include <stdint.h>
#include <string>

using namespace std;

class Command {
public:
  string f1;
  string f2;
  bool isjoin;
  uint32_t join;
  uint32_t prefix;
  uint32_t suffix;
  bool ignorecase;
  bool ignorespace;
  bool isword;

protected:
  static const uint32_t joinDefault = 0, affixDefault = 0;

  static string arg_msg(const string& s, const string& name = "argument");
  static uint32_t parse_uint32(const char* s, const string& name = "argument");

public:
  Command() :
    join(joinDefault),
    prefix(affixDefault),
    suffix(affixDefault),
    isjoin(false),
    ignorecase(false),
    ignorespace(false),
    isword(false) {
  }

  void Parse(int argc, char* argv[]);
};
