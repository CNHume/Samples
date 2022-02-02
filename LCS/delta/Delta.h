// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2017-06-25 CNHume  Refactored Lengths()
// 2014-12-31 CNHume  Created file
//
#pragma once

#include "Pair.h"
#include <stdint.h>

using namespace std;

class Delta : public Pair {
public:
  uint32_t end1 = 0;
  uint32_t end2 = 0;

  virtual ~Delta();
  Delta();
  Delta(uint32_t begin1, uint32_t begin2, uint32_t end1, uint32_t end2,
    shared_ptr<Delta> next = nullptr);

  static void Lengths(const shared_ptr<Delta> deltas, uint32_t& length1, uint32_t& length2);
  static shared_ptr<Delta> Coalesce(const shared_ptr<Pair> pairs);
  static shared_ptr<Delta> Coalesce(const shared_ptr<Delta> deltas, uint32_t join = 0);
  static shared_ptr<Delta> Complement(const shared_ptr<Delta> deltas,
    size_t size1, size_t size2);
  static void Context(shared_ptr<Delta> deltas,
    size_t size1, size_t size2, uint32_t prefix, uint32_t suffix);
};
