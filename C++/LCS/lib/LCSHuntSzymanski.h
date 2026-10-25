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
// 2026-10-09 CNHume  Created LCSHuntSzymanski class
//
// Hunt-Szymanski LCS: O((r + n) log n) using a threshold array over the
// match lists built by Match().  Parallel to LCSRickSpace::Find().
//
#pragma once

#include "LCS.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr

namespace lcs {

  class LCSHuntSzymanski : public LCS {
  public:
    static std::uint32_t Find(
      std::shared_ptr<Pair>* pairs, MATCHES& indexesOf2MatchedByIndex1);
  };

} // namespace lcs
