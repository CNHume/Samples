// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-02 CNHume  Created 2026-10-02 CNHume  Created LCSNormal class
//
#include "LCSNormal.h"

//
// Normal() applies the ignorecase and ignorespace options,
// normalizing records prior to their comparison in Match()
//
void LCSNormal::Normal(const string& input, string& output,
  bool ignorecase, bool ignorespace) {
  if (ignorespace)
    NormalSpace(input, output);
  else
    output = input;

  if (ignorecase)
    NormalCase(output);
}

void LCSNormal::NormalCase(string& input) {
  for (auto& c : input)
    c = tolower(c);                     // normal case
}

void LCSNormal::NormalSpace(const string& input, string& output) {
  // outer (right) trim
  auto end = 0;
  for (auto it = input.rbegin(); it != input.rend(); it++)
    if (!isspace(*it)) {
      end = input.rend() - it;
      break;
    }

  output.clear();
  output.reserve(input.size());
  // inner (left) trims
  bool allowSpace = false;
  for (auto index = 0; index < end; index++) {
    auto c = input[index];

    if (!isspace(c)) {
      output.push_back(c);
      allowSpace = true;
    }
    else if (allowSpace) {
      // normalized space
      output.push_back(' ');
      allowSpace = false;
    }
  }
}
