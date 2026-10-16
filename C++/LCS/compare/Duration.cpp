// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-11 CNHume  Created Duration class
// 2026-10-15 CNHume  Completed Duration as an RAII performance timer
//
#include "Duration.h"

#include <format>                       // for format
#include <iostream>                     // for cout
#include <limits>                       // for numeric_limits
#include <nlohmann/json.hpp>            // Full implementation header
#include <utility>                      // for move

Duration::Duration(std::string name, long count)
  : name_(std::move(name)), count_(count) {
  sw_.start();
}

Duration::~Duration() {
  sw_.stop();
}

void Duration::from_json(const nlohmann::json& j) {
  name_ = j.at("name").get<std::string>();
  count_ = j.at("count").get<long>();
}

nlohmann::json Duration::to_json() const {
  // Snapshot the elapsed time once so rate and elapsedSeconds agree, even
  // when to_json() is called on a still-running stopwatch.
  auto microseconds = sw_.elapsed_microseconds();
  auto seconds = (double)microseconds / 1e6;
  auto rate = seconds > 0 ?
    (double)count_ / seconds :
    std::numeric_limits<double>::quiet_NaN();
  return nlohmann::json{
    { "name", name_ },
    { "count", count_ },
    { "elapsedMicroseconds", microseconds },
    { "elapsedSeconds", seconds },
    { "rate", rate }
  };
}

void Duration::Stop() {
  sw_.stop();
}

void Duration::Show() {
  sw_.stop();
  auto hasrate = sw_.elapsed_seconds() > 0;
  auto rate =  hasrate ?
    (double)count_ / sw_.elapsed_seconds() :
    std::numeric_limits<double>::quiet_NaN();
  std::cout << std::format(
    "{}: {} items in {} sec @{:.0f} Hz\n",
    name_, count_, sw_.format_seconds(), rate);
}
