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
#pragma once

#include "LCSFormat.h"

#include <cstdint>                      // for uint32_t
#include <string>

namespace lcs {

  //
  // LCSSequential lays out each Delta's two sides one after the other,
  // preserving the historic <<<<< / >>>>> vertical format.
  //
  class LCSSequential : public LCSFormat {
  public:
    void Layout(
      std::uint32_t counter, const SideSpan& side1, const SideSpan& side2) override;

  private:
    void Side(
      const SideSpan& side, const std::string& emblem, std::uint32_t counter);
    std::string Head(
      const SideSpan& side, const std::string& emblem, std::uint32_t counter);
    std::string Body(const SideSpan& side);
  };

} // namespace lcs
