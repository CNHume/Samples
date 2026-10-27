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
// 2026-10-26 CNHume  Created LCSCigar class
//
#pragma once

#include "LCSFormat.h"

#include <cstdint>                      // for uint32_t
#include <string>

namespace lcs {

  //
  // LCSCigar emits the SAM CIGAR alignment trace of the query against the
  // reference: M for matched records, D for reference-only records, and I
  // for query-only records.
  //
  class LCSCigar : public LCSFormat {
  public:
    void Begin(
      const std::string&, const std::string&,
      std::uint32_t size1, std::uint32_t) override;
    void End() override;
    void Layout(
      std::uint32_t counter, const SideSpan& side1, const SideSpan& side2) override;

  private:
    std::uint32_t size1_ = 0;           // reference length
    std::uint32_t prevEnd1_ = 0;        // previous delta's reference end
    std::string cigar_;
  };

} // namespace lcs
