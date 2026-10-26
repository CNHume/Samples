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

//#define TRUNCATE_LABELS             // Cut over-long labels instead of wrapping them

namespace {
  //
  // Return the label as one or more centered lines of at most width
  // characters.  By default a long label wraps so the full name remains
  // visible; with TRUNCATE_LABELS it is cut to the column width.
  //
  vector<string> Wrap(const string& text, uint32_t width) {
    vector<string> lines;
#ifdef TRUNCATE_LABELS
    lines.push_back(text.substr(0, width));
#else
    if (width == 0 || text.size() <= width) {
      lines.push_back(text);
      return lines;
    }
    lines.reserve((text.size() + width - 1) / width);
    for (size_t i = 0; i < text.size(); i += width)
      lines.push_back(text.substr(i, width));
#endif
    return lines;
  }

  //
  // Replace tabs with spaces to the next tabsize-column stop, so each
  // character occupies one display column and the truncated content
  // lines up with the border rule.
  //
  string ExpandTabs(const string& text, uint32_t tabsize) {
    string out;
    size_t column = 0;
    for (auto c : text) {
      if (c == '\t') {
        auto spaces = tabsize - column % tabsize;
        out.append(spaces, ' ');
        column += spaces;
      }
      else {
        out.push_back(c);
        column++;
      }
    }
    return out;
  }
}

LCSParallel::LCSParallel(uint32_t width, uint32_t tabsize) :
  width_(max(width, 2u)),
  tabsize_(max(tabsize, 1u)) {}

// Both sides share the same width for now; they may diverge later.
uint32_t LCSParallel::leftWidth() const {
  return width_;
}

uint32_t LCSParallel::rightWidth() const {
  return width_;
}

void LCSParallel::Begin(const string& label1, const string& label2) {
  auto lines1 = Wrap(label1, leftWidth());
  auto lines2 = Wrap(label2, rightWidth());
  auto rows = max(lines1.size(), lines2.size());
  for (size_t i = 0; i < rows; i++) {
    auto l1 = i < lines1.size() ? lines1[i] : string{};
    auto l2 = i < lines2.size() ? lines2[i] : string{};
    cout <<
      format("{:^{}} {:^{}}\n", l1, leftWidth(), l2, rightWidth());
  }
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
  return format("{}+{}\n",
    Heading(side1, leftWidth()), Heading(side2, rightWidth()));
}

std::string LCSParallel::Body(const SideSpan& side1, const SideSpan& side2) const {
  string text;
  auto rows = max(side1.lines.size(), side2.lines.size());
  for (size_t i = 0; i < rows; i++) {
    auto line1 = i < side1.lines.size() ?
      ExpandTabs(side1.lines[i], tabsize_) : string{};
    auto line2 = i < side2.lines.size() ?
      ExpandTabs(side2.lines[i], tabsize_) : string{};
    text +=
      format("{:<{}}|{:<{}}\n",
        line1.substr(0, leftWidth()), leftWidth(),
        line2.substr(0, rightWidth()), rightWidth());
  }
  return text;
}

std::string LCSParallel::Heading(const SideSpan& side, uint32_t width) const {
  return side.begin < side.end ?
    format("{:-^{}}", format("[{}:{}]", side.begin, side.end), width) :
    format("{:=^{}}", format("[{}]", side.begin), width);
}
