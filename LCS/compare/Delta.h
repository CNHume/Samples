// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
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

#include "Pair.h"
#include <stdint.h>
#include <iostream>                     // for cout

using namespace std;

class Delta : public Pair {
public:
  static int64_t Deltas;
  uint32_t end1;
  uint32_t end2;

  virtual ~Delta();
  Delta();
  Delta(uint32_t begin1, uint32_t begin2, uint32_t end1, uint32_t end2,
    shared_ptr<Delta> next = nullptr);
#ifdef COPY_SEMANTICS
  Delta(const Delta& other);
  Delta& operator=(const Delta& other);
#endif                                  // COPY_SEMANTICS
#ifdef MOVE_SEMANTICS
  Delta(Delta&& other);
  Delta& operator=(Delta&& other);
#endif
  static void Lengths(const shared_ptr<Delta> deltas, uint32_t& length1, uint32_t& length2);
  static void List(const shared_ptr<Delta> deltas);
  static shared_ptr<Delta> Copy(const shared_ptr<Delta> deltas);
  static shared_ptr<Delta> Coalesce(const shared_ptr<Pair> pairs);
  static shared_ptr<Delta> Coalesce(const shared_ptr<Delta> deltas, uint32_t join = 0);
  static shared_ptr<Delta> Complement(const shared_ptr<Delta> deltas,
    size_t size1, size_t size2);
  static void Context(shared_ptr<Delta> deltas,
    size_t size1, size_t size2, uint32_t prefix, uint32_t suffix);
};
