// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-13 CNHume  Created Stopwatch class
//
#include "Stopwatch.h"

#include <format>

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

double Stopwatch::elapsed_seconds() const {
  return (double)elapsed_microseconds() / 1e6;
}

double Stopwatch::elapsed_milliseconds() const {
  return (double)elapsed_microseconds() / 1e3;
}

int64_t Stopwatch::elapsed_microseconds() const {
  auto end_point = running_ ?
    std::chrono::steady_clock::now() :
    stop_time_;

  auto duration = std::chrono::duration_cast<std::chrono::microseconds>(
    end_point - start_time_);
  return duration.count();
}

std::string Stopwatch::format_seconds() const {
  auto microseconds = elapsed_microseconds();
  auto fp = microseconds % 1000000;
  auto ip = microseconds / 1000000;
  // Trim trailing zeros, keeping one fractional digit.
  auto digits = 6;
  while (fp % 10 == 0 && digits > 1) {
    fp /= 10;
    digits--;
  }
  return std::format("{}.{:0{}}", ip, fp, digits);
}

std::string Stopwatch::format_milliseconds() const {
  auto microseconds = elapsed_microseconds();
  auto fp = microseconds % 1000;
  auto ip = microseconds / 1000;
  // Trim trailing zeros, keeping one fractional digit.
  auto digits = 3;
  while (fp % 10 == 0 && digits > 1) {
    fp /= 10;
    digits--;
  }
  return std::format("{}.{:0{}}", ip, fp, digits);
}

std::string Stopwatch::format_microseconds() const {
  return std::format("{}", elapsed_microseconds());
}
