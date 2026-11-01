// Copyright (C) 2026, Christopher N. Hume.  All rights reserved.
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
// 2026-10-30 CNHume  Created ProgressBar class
//
#include "ProgressBar.h"

#include <algorithm>                    // for min
#include <cstdio>                       // for stderr
#include <format>                       // for format
#include <iostream>                     // for cerr
#include <string>                       // for string

#if defined(_WIN32)
  #include <io.h>                       // for _fileno, _isatty
#else
  #include <unistd.h>                   // for fileno, isatty
#endif

using namespace std;
using namespace lcs;

namespace {
  constexpr size_t progressFrameCount = 7;
  constexpr size_t progressGlyphCount = 100;    // 1% per glyph
  constexpr int64_t progressUpdateMilliseconds = 750;

  bool isTerminal() {
#if defined(_WIN32)
    return _isatty(_fileno(stderr)) != 0;
#else
    return isatty(fileno(stderr)) != 0;
#endif
  }
}

ProgressBar::ProgressBar() {
  stopwatch_.start();
}

ProgressBar::~ProgressBar() {
  Finish();                             // safety net if Finish() was never called
}

void ProgressBar::Update(double fraction) {
  latest_ = fraction;
  if (isTerminal()) {
    auto isdraw =
      stopwatch_.elapsed_milliseconds() >= (double)progressUpdateMilliseconds;
    if (isdraw) {
      Draw(latest_);
      stopwatch_.start();               // re-arm the update interval
    }
  }
}

void ProgressBar::Finish() {
  if (isTerminal()) {
    // Overwrite the bar line and return to the column start.
    auto barLength = progressGlyphCount + progressFrameCount;
    cerr << '\r' << string(barLength, ' ') << '\r' << flush;
  }
}

void ProgressBar::Draw(double fraction) const {
  auto filled = (size_t)(fraction * (double)progressGlyphCount + 0.5);
  filled = min(filled, progressGlyphCount);
  auto percent = (int)(fraction * 100.0 + 0.5);
  percent = min(percent, 100);
  cerr << '\r' << '[' << string(filled, '#')
    << string(progressGlyphCount - filled, '-')
    << "] " << format("{:3d}%", percent) << flush;
}
