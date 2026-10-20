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
// 2026-10-11 CNHume  Created Duration class
// 2026-10-15 CNHume  Completed Duration as an RAII performance timer
//
#pragma once

#include "Stopwatch.h"

#include <string>

//
// Duration measures the elapsed time of a named operation.  Construct it at
// the start of the region to time and call Show(count) at the end to stop the
// stopwatch and print the measurement, where count is the number of operations
// performed in the measured region.
//
class Duration {
private:
  Stopwatch sw_;
  std::string name_;

public:
  Duration(std::string name);
  ~Duration();

  // Current local wall-clock time as "YYYY-MM-DD HH:MM:SS.cc".
  static std::string WallClock();

  void Stop();                          // Stop the stopwatch
  void Show(long count);                // Stop and report the measurement
};
