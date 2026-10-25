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
// 2026-10-24 CNHume  Created LCSFormat dispatch base
//
#pragma once

#include <cstdint>                      // for uint32_t
#include <string>
#include <vector>

namespace lcs {

  //
  // SideSpan is one side of a Delta, materialized for rendering: the label,
  // the half-open [begin, end) range, and the side's content as UTF-8 lines.
  //
  struct SideSpan {
    std::string label;
    std::uint32_t begin;
    std::uint32_t end;
    std::vector<std::string> lines;
  };

  //
  // LCSFormat is the dispatch base for output layouts.  The Format specifier
  // selects a concrete subclass; the content classes build SideSpans and call
  // Layout() to render each Delta.
  //
  class LCSFormat {
  public:
    virtual ~LCSFormat() = default;

    // Begin a rendering session (e.g. the parallel column labels).
    virtual void Begin(const std::string& label1, const std::string& label2) {}

    // End a rendering session (e.g. the parallel footer).
    virtual void End() {}

    // Lay out one Delta's two sides, given their pre-rendered SideSpans.
    virtual void Layout(
      std::uint32_t counter, const SideSpan& side1, const SideSpan& side2) = 0;
  };

} // namespace lcs
