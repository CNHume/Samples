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
// 2026-10-24 CNHume  Created LCSSequential class
//
#include "LCSSequential.h"

#include <format>                       //[C++20]
#include <iostream>                     // for cout

using namespace std;
using namespace lcs;

// Write both sides of a Delta in series
void LCSSequential::Layout(
  uint32_t counter, const SideSpan& side1, const SideSpan& side2) {
  Side(side1, "<<<<<", counter);
  Side(side2, ">>>>>", counter);
}

void LCSSequential::Side(
  const SideSpan& side, const string& emblem, uint32_t counter) {
  if (side.begin < side.end)
    cout << Head(side, emblem, counter) << Body(side);
}

std::string LCSSequential::Head(
  const SideSpan& side, const string& emblem, uint32_t counter) {
  return format(
    "{} {} {} [{}:{}] {}\n",
    emblem, counter + 1, side.label, side.begin, side.end, emblem);
}

std::string LCSSequential::Body(const SideSpan& side) {
  string text;
  for (const auto& line : side.lines) {
    text += line;
    text += '\n';
  }
  return text;
}
