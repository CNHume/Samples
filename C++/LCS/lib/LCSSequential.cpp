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
  Side("<<<<<", counter, side1);
  Side(">>>>>", counter, side2);
}

void LCSSequential::Side(
  const string& emblem, uint32_t counter, const SideSpan& side) {
  if (side.begin < side.end) {
    Head(emblem, counter, side);
    Body(side);
  }
}

void LCSSequential::Head(
  const string& emblem, uint32_t counter, const SideSpan& side) {
  cout << format(
    "{} {} {} [{}:{}] {}\n",
    emblem, counter + 1, side.label, side.begin, side.end, emblem);
}

void LCSSequential::Body(const SideSpan& side) {
  for (const auto& line : side.lines)
    cout << line << '\n';
}
