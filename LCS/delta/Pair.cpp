// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
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

// Non-mutating Reverse
shared_ptr<Pair> Pair::Reverse(const shared_ptr<Pair> pairs) {
  shared_ptr<Pair> head = nullptr;
  for (auto next = pairs; next != nullptr; next = next->next)
    head = make_shared<Pair>(next->begin1, next->begin2, head);
  return head;
}
