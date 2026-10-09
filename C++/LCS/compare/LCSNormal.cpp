// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-02 CNHume  Created LCSNormal class
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

//
// Code-point normalization for LCSString character comparison.
//

void LCSNormal::Normal(const u32string& input, u32string& output,
  bool ignorecase, bool ignorespace) {
  if (ignorespace)
    NormalSpace(input, output);
  else
    output = input;

  if (ignorecase)
    NormalCase(output);
}

char32_t LCSNormal::ToLower(char32_t c) {
  // ASCII and Latin-1 Supplement one-to-one case mappings.  Full Unicode
  // case folding requires a large table and is intentionally out of scope.
  if (c >= U'A' && c <= U'Z')
    return c + 0x20;
  if ((c >= U'\u00C0' && c <= U'\u00D6') ||
      (c >= U'\u00D8' && c <= U'\u00DE'))
    return c + 0x20;
  return c;
}

bool LCSNormal::IsSpace(char32_t c) {
  return c == U' ' || (c >= U'\t' && c <= U'\r') || c == U'\u00A0';
}

void LCSNormal::NormalCase(u32string& input) {
  for (auto& c : input)
    c = ToLower(c);
}

void LCSNormal::NormalSpace(const u32string& input, u32string& output) {
  vector<uint32_t> mapBegin, mapEnd;
  NormalSpace(input, output, mapBegin, mapEnd);
}

void LCSNormal::NormalSpace(const u32string& input, u32string& output,
  vector<uint32_t>& mapBegin, vector<uint32_t>& mapEnd) {
  // outer (right) trim
  size_t end = input.size();
  while (end > 0 && IsSpace(input[end - 1]))
    end--;

  output.clear();
  mapBegin.clear();
  mapEnd.clear();
  output.reserve(end);
  mapBegin.reserve(end);
  mapEnd.reserve(end);

  // Skip leading whitespace.
  size_t i = 0;
  while (i < end && IsSpace(input[i]))
    i++;

  while (i < end) {
    if (!IsSpace(input[i])) {
      output.push_back(input[i]);
      mapBegin.push_back((uint32_t)i);
      mapEnd.push_back((uint32_t)i + 1);
      i++;
    }
    else {
      // Collapse an internal whitespace run to a single space whose
      // original span is the whole run [runStart, i).
      auto runStart = i;
      while (i < end && IsSpace(input[i]))
        i++;
      output.push_back(U' ');
      mapBegin.push_back((uint32_t)runStart);
      mapEnd.push_back((uint32_t)i);
    }
  }
}
