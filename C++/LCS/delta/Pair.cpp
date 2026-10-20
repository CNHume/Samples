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
#include "Pair.h"

#include <format>                       //[C++20]
#include <iostream>                     // for cout, ostream
#include <memory>                       // for make_shared
#include <utility>                      // for move

//
// Pair Class Implementation
//
// static initialization
//
int64_t Pair::Count = 0;

Pair::~Pair() {
  Count--;
  // Call destructor iteratively to prevent stack overflow when LCS is long
  auto node = std::move(next);
  while (node)
    node = std::move(node->next);
}

Pair::Pair() {
  Count++;
}

Pair::Pair(uint32_t begin1, uint32_t begin2, shared_ptr<Pair> next)
  : begin1(begin1), begin2(begin2), next(next) {
  Count++;
}

#ifdef COPY_SEMANTICS
Pair::Pair(const Pair& other)
  : Pair(other.begin1, other.begin2, Pair::Copy(other.next)) {
}

Pair& Pair::operator=(const Pair& other) {
  if (this != &other) {
    begin1 = other.begin1;
    begin2 = other.begin2;
    next = Pair::Copy(other.next);
  }
  return *this;
}
#endif                                  // COPY_SEMANTICS

#ifdef MOVE_SEMANTICS
Pair::Pair(Pair&& other) noexcept {
  *this = std::move(other);             // move ctor invokes move assignment
  Count++;
}

Pair& Pair::operator=(Pair&& other) noexcept {
  if (this != &other) {
    begin1 = other.begin1;
    begin2 = other.begin2;
    next = other.next;
  }
  return *this;
}
#endif                                  // MOVE_SEMANTICS

ostream& operator<<(ostream& strm, const Pair& pair) {
  return strm <<
    format(
      "({}:{})",
      pair.begin1, pair.begin2);
}

uint32_t Pair::Length(const shared_ptr<Pair> pairs) {
  uint32_t length = 0;
  for (auto next = pairs; next != nullptr; next = next->next)
    length++;
  return length;
}

void Pair::List(const shared_ptr<Pair> pairs) {
  for (auto next = pairs; next != nullptr; next = next->next)
    cout << *next << '\n';
}

shared_ptr<Pair> Pair::Copy(const shared_ptr<Pair> pairs) {
  shared_ptr<Pair> head = nullptr;
  shared_ptr<Pair> rest = nullptr;
  for (auto next = pairs; next != nullptr; next = next->next) {
    auto copy = make_shared<Pair>(next->begin1, next->begin2);
    rest = rest == nullptr ? head = copy : rest->next = copy;
  }
  return head;
}

// Non-mutating Reverse
shared_ptr<Pair> Pair::Reverse(const shared_ptr<Pair> pairs) {
  shared_ptr<Pair> head = nullptr;
  for (auto next = pairs; next != nullptr; next = next->next)
    head = make_shared<Pair>(next->begin1, next->begin2, head);
  return head;
}

// Mutating Reverse
shared_ptr<Pair> Pair::MuReverse(shared_ptr<Pair> pairs) {
  shared_ptr<Pair> last = nullptr;
  while (pairs != nullptr) {
    auto next = pairs->next;            // save next
    pairs->next = last;                 // update next
    last = pairs;                       // update last
    pairs = next;                       // advance
  }
  return last;
}
