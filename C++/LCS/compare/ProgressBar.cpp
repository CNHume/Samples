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
  constexpr size_t progressGlyphCount = 100;    // 1% per glyph
  constexpr int64_t progressUpdateMilliseconds = 750;
  constexpr double progressRateAlpha = 0.5;     // EMA weight for the fraction rate

  bool isTerminal() {
#if defined(_WIN32)
    return _isatty(_fileno(stderr)) != 0;
#else
    return isatty(fileno(stderr)) != 0;
#endif
  }
}

ProgressBar::ProgressBar(bool showTime)
  : showTime_(showTime) {
  stopwatch_.start();
  elapsed_.start();
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
  auto filled = (size_t)(fraction * (double)progressGlyphCount + 0.5);
  filled = min(filled, progressGlyphCount);
  auto percent = (int)(fraction * 100.0 + 0.5);
  percent = min(percent, 100);

  string time;
  if (showTime_ && fraction > 0.0 && fraction < 1.0) {
    //
    // Estimate the remaining time from a smoothed fraction rate rather than
    // the all-time average, so the ETA tracks the current contour rate and
    // stays steadier as that rate slows.
    //
    auto micros = elapsed_.elapsed_microseconds();
    auto deltaFraction = fraction - lastFraction_;
    auto deltaMicros = micros - lastMicros_;
    if (deltaMicros > 0 && deltaFraction > 0.0) {
      auto instant = deltaFraction / (double)deltaMicros;  // fraction per µs
      rate_ = rate_ > 0.0 ?
        progressRateAlpha * instant + (1.0 - progressRateAlpha) * rate_ : instant;
    }
    lastFraction_ = fraction;
    lastMicros_ = micros;
    if (rate_ > 0.0) {
      auto remaining = (int64_t)((1.0 - fraction) / rate_);
      time = format(" {}", metrics::Duration::Elapsed(remaining));
    }
  }

  auto bar = format("\r[{}{}] {:3d}%{}",
    string(filled, '#'), string(progressGlyphCount - filled, '-'),
    percent, time);
  auto length = bar.size() - 1;         // visible length, excluding the '\r'
  if (length < drawnLength_) {
    bar.append(drawnLength_ - length, ' ');  // overwrite any stale tail
  }
  else {
    drawnLength_ = length;
  }
  cerr << bar << flush;
}
