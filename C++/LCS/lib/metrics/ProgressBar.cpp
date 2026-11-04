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
#include "metrics/ProgressBar.h"

#include <algorithm>                    // for min
#include <cmath>                        // for lround
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
using namespace lcs::metrics;

namespace {
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
  if (isTerminal() && drawnLength_ > 0) {
    // Overwrite the bar line and return to the column start.
    cerr << '\r' << string(drawnLength_, ' ') << '\r' << flush;
    drawnLength_ = 0;
  }
}

void ProgressBar::Draw(double fraction) {
  auto filled = (size_t)lround(fraction * (double)progressGlyphCount);
  filled = min(filled, progressGlyphCount);
  auto percent = (int)lround(fraction * 100.0);
  percent = min(percent, 100);
  auto bar = format("\r[{}{}] {:3d}%",
    string(filled, '#'), string(progressGlyphCount - filled, '-'),
    percent);
  drawnLength_ = bar.size() - 1;        // visible length, excluding the '\r'
  cerr << bar << flush;
}
