// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-13 CNHume  Created Stopwatch class
//
#include "Stopwatch.h"

void Stopwatch::start() {
  start_time_ = std::chrono::steady_clock::now();
  running_ = true;
}

void Stopwatch::stop() {
  if (running_) {
    stop_time_ = std::chrono::steady_clock::now();
    running_ = false;
  }
}

void Stopwatch::reset() {
  running_ = false;
  start_time_ = std::chrono::steady_clock::time_point{};
  stop_time_ = std::chrono::steady_clock::time_point{};
}

int64_t Stopwatch::elapsed_microseconds() const {
  std::chrono::steady_clock::time_point end_point;
  if (running_) {
    end_point = std::chrono::steady_clock::now();
  }
  else {
    end_point = stop_time_;
  }

  auto duration = std::chrono::duration_cast<std::chrono::microseconds>(
    end_point - start_time_);
  return duration.count();
}
