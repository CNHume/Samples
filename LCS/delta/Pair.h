// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2014-12-29 CNHume  Created file
//
#pragma once
#define COPY_SEMANTICS
#define MOVE_SEMANTICS

#include <stdint.h>
#include <memory>                       // for shared_ptr<>
#include <iostream>                     // for cout

using namespace std;

class Pair {
protected:
  friend ostream& operator<<(ostream&, const Pair&);

public:
  static int64_t Pairs;
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
  static uint32_t Count(const shared_ptr<Pair> pairs);
  static void List(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> Copy(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> Reverse(const shared_ptr<Pair> pairs);
  static shared_ptr<Pair> MuReverse(shared_ptr<Pair> pairs);
};
