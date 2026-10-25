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

namespace lcs {

  //
  // Duration measures the elapsed time of an operation.  Construct it at the
  // start of the region to time, call Stop() at the end, and read the elapsed
  // with ElapsedMicroseconds().  ToString() formats a report line from raw
  // values so the caller can print it where it chooses.
  //
  class Duration {
  private:
    Stopwatch sw_;

  public:
    Duration();
    ~Duration();

    // Current local wall-clock time as "YYYY-MM-DD HH:MM:SS.cc".
    static std::string WallClock();

    void Stop();                          // Stop the stopwatch
    std::int64_t ElapsedMicroseconds() const;  // Elapsed time in microseconds

    // Format a duration report from raw values (name, operation count, elapsed).
    static std::string ToString(
      const char* name, long count, std::int64_t microseconds);
  };

} // namespace lcs
