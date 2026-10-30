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
#include "formatters/LCSEdit.h"
#include "formatters/LCSFormatRecord.h"
#include "formatters/LCSFormatText.h"
#include "modes/LCSRecord.h"
#include "modes/LCSText.h"
#include "formatters/LCSCigar.h"
#include "formatters/LCSParallel.h"
#include "formatters/LCSSequential.h"

#include <stdexcept>                    // for runtime_error

using namespace std;
using namespace lcs;

namespace {
  //
  // Build the formatter selected by the format specifier.
  //
  unique_ptr<LCSFormat> MakeFormat(
    LCS::Format format, uint32_t width, uint32_t tabsize) {
    unique_ptr<LCSFormat> formatter;
    switch (format) {
    case LCS::Format::CIGAR:
      formatter = make_unique<LCSCigar>();
      break;
    case LCS::Format::Parallel:
      formatter = make_unique<LCSParallel>(width, tabsize);
      break;
    case LCS::Format::Sequential:
      formatter = make_unique<LCSSequential>();
      break;
    default:
      throw runtime_error("Unknown format");
    }
    return formatter;
  }
}

uint32_t LCSFile::Correspondence(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const LCSOptions& options) {
  uint32_t length(0);
  auto format = MakeFormat(options.format, options.width, options.tabsize);
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
        *format, *intervals, s1, s2, options.file1, options.file2);
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
        *format, *intervals, r1, r2, options.file1, options.file2);
  }
  return length;
}

uint32_t LCSFile::Difference(
  shared_ptr<Delta>* intervals, Statistics* statistics,
  const LCSOptions& options) {
  uint32_t length(0);
  if (options.istext) {
    auto format = MakeFormat(options.format, options.width, options.tabsize);
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
        *format, *intervals, s1, s2, options.file1, options.file2);
  }
  else {
    auto r1 = LCSRecord::Read(options.file1, options.isword);
    auto r2 = LCSRecord::Read(options.file2, options.isword);
    if (options.format == LCS::Format::Edit) {
      shared_ptr<Delta> matches;
      length = LCSRecord::Compare(
        &matches, statistics, r1, r2,
        options.algorithm, options.ignorecase, options.ignorespace,
        options.isverbose);
      if (intervals != nullptr) {
        auto differences = Delta::Complement(matches, r1.size(), r2.size());
        auto editIntervals = LCSEdit::BuildIntervals(
          matches, differences, options.prefix, options.suffix);
        if (!editIntervals.empty()) {   // identical files: emit nothing, like diff -u
          LCSEdit edit;
          LCSFormatRecord::ShowEdit(
            edit, editIntervals, r1, r2, options.file1, options.file2);
        }
      }
    }
    else {
      auto format = MakeFormat(options.format, options.width, options.tabsize);
      length = LCSRecord::Difference(
        intervals, statistics, r1, r2,
        options.algorithm, options.ignorecase, options.ignorespace,
        options.isjoin, options.isverbose,
        options.join, options.prefix, options.suffix);
      if (intervals != nullptr)
        LCSFormatRecord::Show(
          *format, *intervals, r1, r2, options.file1, options.file2);
    }
  }
  return length;
}
