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
// 2018-05-10 CNHume  Removed combined Complement()/Context() method.
// 2017-06-25 CNHume  Refactored Lengths()
// 2015-04-18 CNHume  Converted prefix, suffix and join to uint32
// 2015-01-25 CNHume  Added Delta::Count()
// 2015-01-24 CNHume  Converted length1/2 to end1/2 for clarity
// 2015-01-23 CNHume  Renamed class to Delta
// 2015-01-22 CNHume  Added Context()
// 2015-01-19 CNHume  Added Complement()
// 2015-01-18 CNHume  Renamed class to IntervalPair
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2015-01-04 CNHume  Added Coalesce()
// 2014-12-31 CNHume  Created file
//
#pragma once
//#define SHOW_INTERVALS                // Delta::Context()

#include "Pair.h"

#include <cstdint>                      // for int64_t, uint32_t
#include <memory>                       // for shared_ptr

namespace lcs {

class Delta : public Pair {
public:
  static std::int64_t Deltas;
  std::uint32_t end1 = 0;
  std::uint32_t end2 = 0;

  virtual ~Delta();
  Delta();
  Delta(std::uint32_t begin1, std::uint32_t begin2, std::uint32_t end1, std::uint32_t end2,
    std::shared_ptr<Delta> next = nullptr);
#ifdef COPY_SEMANTICS
  Delta(const Delta& other);
  Delta& operator=(const Delta& other);
#endif                                  // COPY_SEMANTICS
#ifdef MOVE_SEMANTICS
  Delta(Delta&& other) noexcept;
  Delta& operator=(Delta&& other) noexcept;
#endif
  static void Lengths(const std::shared_ptr<Delta> deltas, std::uint32_t& length1, std::uint32_t& length2);
  static void List(const std::shared_ptr<Delta> deltas);
  static std::shared_ptr<Delta> Copy(const std::shared_ptr<Delta> deltas);
  static std::shared_ptr<Delta> Coalesce(const std::shared_ptr<Pair> pairs);
  static std::shared_ptr<Delta> Coalesce(const std::shared_ptr<Delta> deltas, std::uint32_t join = 0);
  static std::shared_ptr<Delta> Complement(const std::shared_ptr<Delta> deltas,
    std::size_t size1, std::size_t size2);
  static void Context(std::shared_ptr<Delta> deltas,
    std::size_t size1, std::size_t size2, std::uint32_t prefix, std::uint32_t suffix);
};

} // namespace lcs
