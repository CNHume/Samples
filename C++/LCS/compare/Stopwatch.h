// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-13 CNHume  Created Stopwatch class
//
#pragma once

#include <chrono>
#include <cstdint>                      // for int64_t
#include <iostream>
#include <thread>

class Stopwatch {
private:
  std::chrono::steady_clock::time_point start_time_;
  std::chrono::steady_clock::time_point stop_time_;
  bool running_ = false;

public:
  void start();
  void stop();
  void reset();
  int64_t elapsed_microseconds() const;
};
