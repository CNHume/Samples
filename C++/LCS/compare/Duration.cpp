// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-11 CNHume  Created Duration class
//
#include "Duration.h"
#include <nlohmann/json.hpp>            // Full implementation header

#include <format>
#include <iostream>                     // for cout
#include <string>

void Duration::from_json(const nlohmann::json& j) {
  name = j.at("Name").get<std::string>();
  included = j.at("Included").get<long>();
}

nlohmann::json Duration::to_json() const {
  return nlohmann::json{
    { "Name", name },
    { "Included", included }
  };
}

void Duration::Show() {
  auto j = to_json();
  std::cout << std::format(
    "duration = {}\n", j.dump(2));
}

void Duration::DemoStopwatch() {
  Stopwatch sw;

  sw.start();

  // Sleep for testing: 5 ms = 5000 microseconds
  std::this_thread::sleep_for(std::chrono::milliseconds(5));

  sw.stop();

  std::cout << std::format(
    "Elapsed time: {} sec\n", sw.format_seconds());
}
