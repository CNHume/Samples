// Copyright (C) 2026, Christopher N. Hume.  All rights reserved.
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
// 2026-10-29 CNHume  Created Edit class
//
#pragma once

#include "formatters/Formatter.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string
#include <vector>                       // for vector

namespace lcs::formatters {

  //
  // A result is a list of intervals; each interval is a list of tagged runs
  // (Deltas whose EditKind is Match or Change).
  //
  typedef std::vector<std::vector<std::shared_ptr<Delta>>> EDIT_INTERVALS;

  //
  // Edit renders the GNU unified diff patch (-u): a ---/+++ file header,
  // then per-interval @@ headers and the prefixed body lines.
  //
  class Edit : public Formatter {
  public:
    void BeginResult(
      const std::string& label1, const std::string& label2,
      std::uint32_t size1, std::uint32_t size2) override;
    void BeginInterval(
      std::uint32_t begin1, std::uint32_t end1,
      std::uint32_t begin2, std::uint32_t end2) override;
    void Layout(
      std::uint32_t, const SideSpan& side1, const SideSpan& side2,
      EditKind kind) override;
    void EndResult() override;

    //
    // Build the intervals from the match and difference runs: tag each Delta,
    // group differences into intervals with prefix/suffix context, and merge
    // consecutive differences when the matches between them number <=
    // max(prefix + suffix, join).
    //
    static EDIT_INTERVALS BuildIntervals(
      const std::shared_ptr<Delta>& matches,
      const std::shared_ptr<Delta>& differences,
      std::uint32_t prefix, std::uint32_t suffix,
      std::uint32_t join = 0);

  private:
    static std::string Range(std::uint32_t begin, std::uint32_t end);
  };

} // namespace lcs::formatters
