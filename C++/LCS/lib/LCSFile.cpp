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
#include "LCSFormatText.h"
#include "LCSRecord.h"
#include "LCSText.h"
#include "LCSSequential.h"

using namespace std;
using namespace lcs;

uint32_t LCSFile::Correspondence(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const LCSOptions& options) {
  uint32_t length(0);
  LCSSequential format;
  if (options.istext) {
    u32string s1, s2;
    if (options.isquote) {
      // Treat filename arguments as literal strings.
      s1 = LCSEncoding::DecodeUtf8(options.file1);
      s2 = LCSEncoding::DecodeUtf8(options.file2);
    }
    else {
      s1 = LCSText::Read(options.file1);
      s2 = LCSText::Read(options.file2);
      if (options.isfasta) {
        s1 = LCSText::StripFasta(s1);
        s2 = LCSText::StripFasta(s2);
      }
    }
    length = LCSText::Correspondence(
      intervals, statistics, s1, s2,
      options.algorithm, options.ignorecase, options.ignorespace,
      options.isjoin, options.isverbose,
      options.join, options.prefix, options.suffix);
    if (intervals != nullptr)
      LCSFormatText::Show(
        format, *intervals, s1, s2, options.file1, options.file2);
  }
  else {
    auto r1 = LCSRecord::Read(options.file1, options.isword);
    auto r2 = LCSRecord::Read(options.file2, options.isword);
    length = LCSRecord::Correspondence(
      intervals, statistics, r1, r2,
      options.algorithm, options.ignorecase, options.ignorespace,
      options.isjoin, options.isverbose,
      options.join, options.prefix, options.suffix);
    if (intervals != nullptr)
      LCSFormatRecord::Show(
        format, *intervals, r1, r2, options.file1, options.file2);
  }
  return length;
}

uint32_t LCSFile::Difference(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const LCSOptions& options) {
  uint32_t length(0);
  LCSSequential format;
  if (options.istext) {
    u32string s1, s2;
    if (options.isquote) {
      // Treat filename arguments as literal strings.
      s1 = LCSEncoding::DecodeUtf8(options.file1);
      s2 = LCSEncoding::DecodeUtf8(options.file2);
    }
    else {
      s1 = LCSText::Read(options.file1);
      s2 = LCSText::Read(options.file2);
      if (options.isfasta) {
        s1 = LCSText::StripFasta(s1);
        s2 = LCSText::StripFasta(s2);
      }
    }
    length = LCSText::Difference(
      intervals, statistics, s1, s2,
      options.algorithm, options.ignorecase, options.ignorespace,
      options.isjoin, options.isverbose,
      options.join, options.prefix, options.suffix);
    if (intervals != nullptr)
      LCSFormatText::Show(
        format, *intervals, s1, s2, options.file1, options.file2);
  }
  else {
    auto r1 = LCSRecord::Read(options.file1, options.isword);
    auto r2 = LCSRecord::Read(options.file2, options.isword);
    length = LCSRecord::Difference(
      intervals, statistics, r1, r2,
      options.algorithm, options.ignorecase, options.ignorespace,
      options.isjoin, options.isverbose,
      options.join, options.prefix, options.suffix);
    if (intervals != nullptr)
      LCSFormatRecord::Show(
        format, *intervals, r1, r2, options.file1, options.file2);
  }
  return length;
}
