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
// 2026-10-24 CNHume  Changed to accept Options rather than Command
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created File class
//
#pragma once

#include "LCSOptions.h"
#include "Statistics.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr

namespace lcs {

  class File {
  public:
    static std::uint32_t Correspondence(
      std::shared_ptr<Delta>* intervals, Statistics* statistics,
      const Options& options);

    static std::uint32_t Difference(
      std::shared_ptr<Delta>* intervals, Statistics* statistics,
      const Options& options);
  };

} // namespace lcs
