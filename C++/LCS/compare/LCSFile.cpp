// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#include "LCSFile.h"

uint32_t  LCSFile::Correspondence(
  shared_ptr<Delta>* intervals, const Command command) {
  if (command.istext) {
    auto s1 = LCSString::Read(command.file1);
    auto s2 = LCSString::Read(command.file2);
    auto length = LCSString::Correspondence(intervals, s1, s2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.isrick);
    LCSFormatString::Show(*intervals, s1, s2, command.file1, command.file2);
    return length;
  }

  auto r1 = Read(command.file1, command.isword);
  auto r2 = Read(command.file2, command.isword);
  auto length = LCSRecord::Correspondence(intervals, r1, r2,
    command.ignorecase, command.ignorespace, command.isjoin,
    command.join, command.prefix, command.suffix);
  Show(*intervals, r1, r2, command.file1, command.file2);
  return length;
}

uint32_t  LCSFile::Difference(
  shared_ptr<Delta>* intervals, const Command command) {
  if (command.istext) {
    auto s1 = LCSString::Read(command.file1);
    auto s2 = LCSString::Read(command.file2);
    auto length = LCSString::Difference(intervals, s1, s2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.isrick);
    LCSFormatString::Show(*intervals, s1, s2, command.file1, command.file2);
    return length;
  }

  auto r1 = Read(command.file1, command.isword);
  auto r2 = Read(command.file2, command.isword);
  auto length = LCSRecord::Difference(intervals, r1, r2,
    command.ignorecase, command.ignorespace, command.isjoin,
    command.join, command.prefix, command.suffix);
  Show(*intervals, r1, r2, command.file1, command.file2);
  return length;
}
