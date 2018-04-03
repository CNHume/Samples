// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2014-12-29 CNHume  Created file
//
#include "Pair.h"

//
// Pair Class Implementation
//
// static initialization
//
int64_t Pair::Pairs = 0;

Pair::~Pair() {
  Pairs--;
}

Pair::Pair() {
  Pairs++;
}

Pair::Pair(uint32_t begin1, uint32_t begin2, shared_ptr<Pair> next)
  : begin1(begin1), begin2(begin2), next(next) {
  Pairs++;
}

#ifdef COPY_SEMANTICS
Pair::Pair(const Pair& other)
  : begin1(other.begin1), begin2(other.begin2), next(other.next) {
  //[ToDo]Perform a deep, recursive copy here
  Pairs++;
}

Pair& Pair::operator=(const Pair& other) {
  if (this != &other) {
    begin1 = other.begin1;
    begin2 = other.begin2;
    //[ToDo]Perform a deep, recursive copy here
    next = other.next;
  }
  return *this;
}
#endif                                  // COPY_SEMANTICS

#ifdef MOVE_SEMANTICS
Pair::Pair(Pair&& other) {
  *this = std::move(other);             // move ctor invokes move assignment
  Pairs++;
}

Pair& Pair::operator=(Pair&& other) {
  if (this != &other) {
    begin1 = other.begin1;
    begin2 = other.begin2;
    next = other.next;
  }
  return *this;
}
#endif                                  // MOVE_SEMANTICS

uint32_t Pair::Count(const shared_ptr<Pair> pairs) {
  uint32_t length = 0;
  for (auto next = pairs; next != nullptr; next = next->next)
    length++;
  return length;
}

void Pair::List(const shared_ptr<Pair> pairs) {
  for (auto next = pairs; next != nullptr; next = next->next)
    cout << "(" << next->begin1 << ", " << next->begin2 << ")" << endl;
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
