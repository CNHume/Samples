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

using namespace std;

class Pair {
protected:
  friend ostream& operator<<(ostream&, const Pair&);

public:
  static int64_t Count;
  uint32_t begin1 = 0;
  uint32_t begin2 = 0;
  shared_ptr<Pair> next;

  virtual ~Pair();
  Pair();
  Pair(uint32_t begin1, uint32_t begin2, shared_ptr<Pair> next = nullptr);
#ifdef COPY_SEMANTICS
  Pair(const Pair& other);
  Pair& operator=(const Pair& other);
#endif                                  // COPY_SEMANTICS
#ifdef MOVE_SEMANTICS
  Pair(Pair&& other) noexcept;
  Pair& operator=(Pair&& other) noexcept;
#endif
  static uint32_t Length(const shared_ptr<Pair> pairs);
  static void List(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> Copy(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> Reverse(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> MuReverse(shared_ptr<Pair> pairs);
};
