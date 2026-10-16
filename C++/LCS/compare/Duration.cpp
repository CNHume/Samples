// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-11 CNHume  Created Duration class
// 2026-10-15 CNHume  Completed Duration as an RAII performance timer
//
#include "Duration.h"

#include <format>                       // for format
#include <iostream>                     // for cout
#include <nlohmann/json.hpp>            // Full implementation header
#include <utility>                      // for move

Duration::Duration(std::string name, long item_count)
  : name_(std::move(name)), item_count_(item_count) {
  stopwatch_.start();
}

Duration::~Duration() {
  stopwatch_.stop();
}

void Duration::from_json(const nlohmann::json& j) {
  name_ = j.at("Name").get<std::string>();
  item_count_ = j.at("ItemCount").get<long>();
}

nlohmann::json Duration::to_json() const {
  auto seconds = stopwatch_.elapsed_seconds();
  auto item_rate = seconds > 0 ? (double)item_count_ / seconds : 0.0;
  return nlohmann::json{
    { "Name", name_ },
    { "ItemCount", item_count_ },
    { "ElapsedMicroseconds", stopwatch_.elapsed_microseconds() },
    { "Seconds", seconds },
    { "ItemRate", item_rate }
  };
}

void Duration::Stop() {
  stopwatch_.stop();
}

void Duration::Show() {
  stopwatch_.stop();
  auto seconds = stopwatch_.elapsed_seconds();
  auto item_rate = seconds > 0 ? (double)item_count_ / seconds : 0.0;
  std::cout << std::format(
    "{}: {} sec for {} items ({:.0f} items/sec)\n",
    name_, stopwatch_.format_seconds(), item_count_, item_rate);
}
