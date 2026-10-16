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

#include <nlohmann/json_fwd.hpp>        // Forward declaration header
#include <string>

//
// Duration measures the elapsed time of a named operation and reports
// the item rate over the number of items included in the operation.
// Construct it at the start of the region to time and call Show() at
// the end to stop the stopwatch and print the measurement.
//
class Duration {
private:
  Stopwatch sw_;
  std::string name_;
  long count_ = 0;                      // Operation count

public:
  Duration(std::string name, long count = 0);
  ~Duration();

  // Serialization/Deserialization
  void from_json(const nlohmann::json& json);
  nlohmann::json to_json() const;

  void Stop();                          // Stop the stopwatch
  void Show();                          // Stop and report the measurement
};
