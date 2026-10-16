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
  auto hasrate = microseconds > 0;
  auto rateMHz = hasrate ?
    (double)count_ / microseconds :
    std::numeric_limits<double>::quiet_NaN();
  auto j = nlohmann::json{
    { "name", name_ },
    { "count", count_ },
    { "elapsedMicroseconds", microseconds },
    { "elapsedSeconds", seconds },
    { "rateMHz", rateMHz }
  };
  return j;
}

void Duration::Stop() {
  sw_.stop();
}

void Duration::Show() {
  sw_.stop();
  auto microseconds = sw_.elapsed_microseconds();
  auto hasrate = microseconds > 0;
  auto rateMHz = hasrate ?
    (double)count_ / microseconds :
    std::numeric_limits<double>::quiet_NaN();
  std::cout << std::format(
    "{}: {} operations in {} µs @{:.3g} MHz\n",
    name_, count_, sw_.format_microseconds(), rateMHz);
}
