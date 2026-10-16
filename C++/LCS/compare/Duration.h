// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
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
  Stopwatch stopwatch_;
  std::string name_;
  long item_count_ = 0;                 // number of items included in the operation

public:
  Duration(std::string name, long item_count = 0);
  ~Duration();

  // The compiler only needs to know 'json' exists to look at these signatures:
  void from_json(const nlohmann::json& json);
  nlohmann::json to_json() const;

  void Stop();                          // stop the stopwatch
  void Show();                          // stop and report the measurement
};
