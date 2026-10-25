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
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2014-12-29 CNHume  Created file
//
#pragma once
#define COPY_SEMANTICS
#define MOVE_SEMANTICS

#include <cstdint>                      // for int64_t, uint32_t
#include <iostream>                     // for ostream
#include <memory>                       // for shared_ptr<>

namespace lcs {

  class Pair {
  protected:
    friend std::ostream& operator<<(std::ostream&, const Pair&);

  public:
    static std::int64_t Count;
    std::uint32_t begin1 = 0;
    std::uint32_t begin2 = 0;
    std::shared_ptr<Pair> next;

    virtual ~Pair();
    Pair();
    Pair(std::uint32_t begin1, std::uint32_t begin2, std::shared_ptr<Pair> next = nullptr);
  #ifdef COPY_SEMANTICS
    Pair(const Pair& other);
    Pair& operator=(const Pair& other);
  #endif                                  // COPY_SEMANTICS
  #ifdef MOVE_SEMANTICS
    Pair(Pair&& other) noexcept;
    Pair& operator=(Pair&& other) noexcept;
  #endif
    static std::uint32_t Length(const std::shared_ptr<Pair> pairs);
    static void List(const std::shared_ptr<Pair> pairs);
    static std::shared_ptr<Pair> Copy(const std::shared_ptr<Pair> pairs);
    static std::shared_ptr<Pair> Reverse(const std::shared_ptr<Pair> pairs);
    static std::shared_ptr<Pair> MuReverse(std::shared_ptr<Pair> pairs);
  };

} // namespace lcs
