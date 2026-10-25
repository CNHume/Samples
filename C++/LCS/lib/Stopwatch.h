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
// 2026-10-13 CNHume  Created Stopwatch class
//
#pragma once

#include <chrono>
#include <cstdint>                      // for int64_t
#include <string>

namespace lcs {

  class Stopwatch {
  private:
    std::chrono::steady_clock::time_point start_time_;
    std::chrono::steady_clock::time_point stop_time_;
    bool running_ = false;

  public:
    void start();
    void stop();
    void reset();
    double elapsed_seconds() const;
    double elapsed_milliseconds() const;
    std::int64_t elapsed_microseconds() const;
    std::string format_seconds() const;
    std::string format_milliseconds() const;
    std::string format_microseconds() const;
  };

} // namespace lcs
