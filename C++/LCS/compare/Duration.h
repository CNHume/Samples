// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-11 CNHume  Created Duration class
//
#pragma once
#include <string>
#include <nlohmann/json_fwd.hpp>        // Forward declaration header

class Duration {
public:
  std::string name;
  long included;

  // The compiler only needs to know 'json' exists to look at these signatures:
  void from_json(const nlohmann::json& json);

  nlohmann::json to_json() const;

  void Show();
};
