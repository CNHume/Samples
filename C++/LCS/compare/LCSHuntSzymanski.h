// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-09 CNHume  Created LCSHuntSzymanski class
//
// Hunt-Szymanski LCS: O((r + n) log n) using a threshold array over the
// match lists built by Match().  Parallel to LCSRick::Find().
//
#pragma once

#include "LCS.h"

using namespace std;

class LCSHuntSzymanski : public LCS {
public:
  static uint32_t Find(
    shared_ptr<Pair>* pairs, MATCHES& indexesOf2MatchedByIndex1);

private:
  static shared_ptr<Pair> pushPair(
    PAIRS& chains, const ptrdiff_t& index3,
    uint32_t& index1, uint32_t& index2);
};
