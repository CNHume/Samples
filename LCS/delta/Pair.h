// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2014-12-29 CNHume  Created file
//
#pragma once

#include <stdint.h>
#include <memory>                       // for shared_ptr<>

using namespace std;

class Pair {
public:
  static int64_t Pairs;
  uint32_t begin1 = 0;
  uint32_t begin2 = 0;
  shared_ptr<Pair> next;

  virtual ~Pair();
  Pair();
  Pair(uint32_t begin1, uint32_t begin2, shared_ptr<Pair> next = nullptr);

  static shared_ptr<Pair> Reverse(const shared_ptr<Pair> pairs);
};
