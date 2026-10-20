// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile class
//
#include "LCSFile.h"
#include "LCSEncoding.h"
#include "LCSFormatRecord.h"
#include "LCSFormatString.h"
#include "LCSRecord.h"
#include "LCSString.h"

uint32_t LCSFile::Correspondence(
  shared_ptr<Delta>* intervals, const Command command,
  Statistics* statistics) {
  uint32_t length(0);
  if (command.istext) {
    u32string s1, s2;
    if (command.isquote) {
      // Treat filename arguments as literal strings.
      s1 = LCSEncoding::DecodeUtf8(command.file1);
      s2 = LCSEncoding::DecodeUtf8(command.file2);
    }
    else {
      s1 = LCSString::Read(command.file1);
      s2 = LCSString::Read(command.file2);
      if (command.isfasta) {
        s1 = LCSString::StripFasta(s1);
        s2 = LCSString::StripFasta(s2);
      }
    }
    length = LCSString::Correspondence(intervals, s1, s2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.algorithm, statistics);
    if (intervals != nullptr)
      LCSFormatString::Show(*intervals, s1, s2, command.file1, command.file2);
  }
  else {
    auto r1 = LCSRecord::Read(command.file1, command.isword);
    auto r2 = LCSRecord::Read(command.file2, command.isword);
    length = LCSRecord::Correspondence(intervals, r1, r2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.algorithm, statistics);
    if (intervals != nullptr)
      LCSFormatRecord::Show(*intervals, r1, r2, command.file1, command.file2);
  }
  return length;
}

uint32_t LCSFile::Difference(
  shared_ptr<Delta>* intervals, const Command command,
  Statistics* statistics) {
  uint32_t length(0);
  if (command.istext) {
    u32string s1, s2;
    if (command.isquote) {
      // Treat filename arguments as literal strings.
      s1 = LCSEncoding::DecodeUtf8(command.file1);
      s2 = LCSEncoding::DecodeUtf8(command.file2);
    }
    else {
      s1 = LCSString::Read(command.file1);
      s2 = LCSString::Read(command.file2);
      if (command.isfasta) {
        s1 = LCSString::StripFasta(s1);
        s2 = LCSString::StripFasta(s2);
      }
    }
    length = LCSString::Difference(intervals, s1, s2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.algorithm, statistics);
    if (intervals != nullptr)
      LCSFormatString::Show(*intervals, s1, s2, command.file1, command.file2);
  }
  else {
    auto r1 = LCSRecord::Read(command.file1, command.isword);
    auto r2 = LCSRecord::Read(command.file2, command.isword);
    length = LCSRecord::Difference(intervals, r1, r2,
      command.ignorecase, command.ignorespace, command.isjoin,
      command.join, command.prefix, command.suffix, command.algorithm, statistics);
    if (intervals != nullptr)
      LCSFormatRecord::Show(*intervals, r1, r2, command.file1, command.file2);
  }
  return length;
}
