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
// 2026-10-24 CNHume  Created Formatter dispatch base
//
#pragma once

#include "Delta.h"

#include <cstdint>                      // for uint32_t
#include <string>
#include <vector>

namespace lcs::formatters {

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
  // Formatter is the dispatch base for output layouts.  The Format specifier
  // selects a concrete subclass; the content classes build SideSpans and call
  // Layout() to render each Delta.
  //
  class Formatter {
  public:
    virtual ~Formatter() = default;

    // Begin a file-level result (e.g. the parallel column labels, or the
    // Edit format's ---/+++ file header), given the reference and query sizes.
    virtual void BeginResult(
      const std::string&, const std::string&, std::uint32_t, std::uint32_t) {}

    // Begin an interval within the result (the Edit format's @@ header).
    // Part of the protocol for every format; only Edit implements it.
    virtual void BeginInterval(
      std::uint32_t, std::uint32_t,
      std::uint32_t, std::uint32_t) {}

    // Lay out one Delta's two sides, given their pre-rendered SideSpans.
    virtual void Layout(
      std::uint32_t counter, const SideSpan& side1, const SideSpan& side2,
      EditKind kind = EditKind::None) = 0;

    // End a file-level result (e.g. the parallel footer).
    virtual void EndResult() {}

  protected:
    // Default glyphs shared by the output layouts.  The names are abstract
    // (roles, not characters) so a subclass may override the glyph later.
    static constexpr char horizontalEmpty = '=';  // empty-side rule fill
    static constexpr char horizontal = '-';       // ranged-side rule fill
    static constexpr char horizontalCenter = '+'; // rule-row separator
    static constexpr char contentCenter = '|';    // content-row separator
    static constexpr char leftEmblem = '<';       // sequential left marker
    static constexpr char rightEmblem = '>';      // sequential right marker
  };

} // namespace lcs::formatters
