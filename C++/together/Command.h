// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-05-16 CNHume  Added Command class
//
#pragma once

#include <stdint.h>
#include <string>

using namespace std;

class Command {
public:
  //
  // command line parameters to be parsed:
  //
  string filename;
  uint32_t display;
  bool test;

protected:
  static const uint32_t displayDefault = 0;

  static string arg_msg(const string& s, const string& name = "argument");
  static uint32_t parse_uint32(const char* s, const string& name = "argument");

public:
  Command() :
    display(displayDefault),
    test(false) {
  }

  void Parse(int argc, char* argv[]);
};
