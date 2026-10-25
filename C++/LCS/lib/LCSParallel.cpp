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
// 2026-10-24 CNHume  Created LCSParallel class
//
#include "LCSParallel.h"

#include <algorithm>                    // for max
#include <format>                       //[C++20]
#include <iostream>                     // for cout

using namespace std;
using namespace lcs;

LCSParallel::LCSParallel(uint32_t width) : width_(max(width, 2u)) {}

uint32_t LCSParallel::leftWidth() const {
  return (width_ - 1) / 2;
}

uint32_t LCSParallel::rightWidth() const {
  return width_ - 1 - leftWidth();
}

void LCSParallel::Begin(const string& label1, const string& label2) {
  cout <<
    format("{:^{}} {:^{}}\n", label1, leftWidth(), label2, rightWidth());
}

void LCSParallel::End() {
  cout <<
    format("{:<{}}+{:-<{}}\n", "", leftWidth(), "", rightWidth());
}

void LCSParallel::Layout(
  uint32_t, const SideSpan& side1, const SideSpan& side2) {
  cout <<
    Head(side1, side2) << Body(side1, side2);
}

std::string LCSParallel::Head(const SideSpan& side1, const SideSpan& side2) const {
  return format(
    "{:<{}}+{:<{}}\n", Heading(side1), leftWidth(), Heading(side2), rightWidth());
}

std::string LCSParallel::Body(const SideSpan& side1, const SideSpan& side2) const {
  string text;
  auto rows = max(side1.lines.size(), side2.lines.size());
  for (size_t i = 0; i < rows; i++) {
    auto line1 = i < side1.lines.size() ? side1.lines[i] : string{};
    auto line2 = i < side2.lines.size() ? side2.lines[i] : string{};
    text +=
      format("{:<{}}|{:<{}}\n", line1, leftWidth(), line2, rightWidth());
  }
  return text;
}

std::string LCSParallel::Heading(const SideSpan& side) const {
  return side.begin < side.end ?
    format("------ [{}:{}] ------", side.begin, side.end) :
    format("======= [{}] =======", side.begin);
}
