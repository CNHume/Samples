// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-11 CNHume  Created Duration class
//
#pragma once

#include <nlohmann/json_fwd.hpp>        // Forward declaration header
#include <string>

class Duration {
public:
  std::string name;
  long included;

  void from_json(const nlohmann::json& json);
  nlohmann::json to_json() const;

  void Show();
  void DemoStopwatch();
};
