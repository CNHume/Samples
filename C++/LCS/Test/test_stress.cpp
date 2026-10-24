// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created the stress subcommand (absorbed from scratch/fuzz.cpp).
//
// Randomized stress: verify all three LCS algorithms against an O(m*n)
// dynamic-programming reference over many small, mostly low-alphabet inputs.
// Each trial checks both the reported length and the validity of the
// reconstructed match trace, so a sub-optimal or corrupted trace is caught,
// not just a wrong length.
//
#include "LCSText.h"
#include "Delta.h"

#include <algorithm>                    // for equal, fill, max
#include <cstdint>                      // for uint32_t, uint64_t
#include <cstdio>                       // for fprintf, printf
#include <cstdlib>                      // for strtoul, strtoull
#include <string>
#include <vector>

using namespace lcs;

using namespace std;

namespace {
  // Rolling two-row O(m*n) DP LCS length: independent ground truth.
  uint32_t dpLength(const u32string& a, const u32string& b) {
    auto m = a.size();
    auto n = b.size();
    vector<uint32_t> prev(n + 1, 0), cur(n + 1, 0);
    for (size_t i = 1; i <= m; ++i) {
      for (size_t j = 1; j <= n; ++j)
        cur[j] = a[i - 1] == b[j - 1] ?
          prev[j - 1] + 1 : max(prev[j], cur[j - 1]);
      prev.swap(cur);
      fill(cur.begin(), cur.end(), 0);
    }
    return prev[n];
  }

  // The coalesced match Deltas must form a valid LCS trace of `length`.
  bool checkTrace(const shared_ptr<Delta> deltas, uint32_t length,
                  const u32string& s1, const u32string& s2, const char* algo) {
    uint32_t sum = 0;
    uint32_t priorEnd1 = 0;
    uint32_t priorEnd2 = 0;
    for (auto d = deltas; d != nullptr; d = dynamic_pointer_cast<Delta>(d->next)) {
      auto len1 = d->end1 - d->begin1;
      auto len2 = d->end2 - d->begin2;
      if (d->begin1 < priorEnd1 || d->begin2 < priorEnd2 ||
          len1 != len2 || d->end1 > s1.size() || d->end2 > s2.size() ||
          !equal(s1.begin() + d->begin1, s1.begin() + d->end1,
                 s2.begin() + d->begin2)) {
        fprintf(stderr, "[%s] invalid match trace\n", algo);
        return false;
      }
      sum += len1;
      priorEnd1 = d->end1;
      priorEnd2 = d->end2;
    }
    if (sum != length) {
      fprintf(stderr, "[%s] trace length %u != reported %u\n", algo, sum, length);
      return false;
    }
    return true;
  }

  const char* algoName(LCS::Algorithm algorithm) {
    switch (algorithm) {
    case LCS::Algorithm::HuntSzymanski:
      return "Hunt-Szymanski";
    case LCS::Algorithm::RickSpace:
      return "RickSpace";
    case LCS::Algorithm::RickTime:
      return "RickTime";
    }
    return "?";
  }

  bool checkOne(const u32string& s1, const u32string& s2, uint64_t trial) {
    auto expected = dpLength(s1, s2);
    const LCS::Algorithm algos[3] = {
      LCS::Algorithm::HuntSzymanski,
      LCS::Algorithm::RickSpace,
      LCS::Algorithm::RickTime,
    };
    for (auto algorithm : algos) {
      shared_ptr<Delta> deltas;
      auto length = LCSText::Correspondence(
        &deltas, nullptr, s1, s2, algorithm, false, false, false, false, 0, 0, 0);
      if (length != expected) {
        fprintf(stderr, "[trial %llu] %s length %u != DP %u\n",
          (unsigned long long)trial, algoName(algorithm), length, expected);
        return false;
      }
      if (!checkTrace(deltas, length, s1, s2, algoName(algorithm)))
        return false;
    }
    return true;
  }

  uint32_t rngState = 0x9E3779B9u;
  uint32_t nextRand() {
    uint32_t x = rngState;              // xorshift32
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return rngState = x;
  }

  u32string randomString(size_t len, size_t alphabetSize) {
    u32string s;
    s.reserve(len);
    for (size_t i = 0; i < len; ++i)
      s.push_back((char32_t)('A' + nextRand() % alphabetSize));
    return s;
  }
}

int runStress(int argc, char* argv[]) {
  auto trials = argc > 1 ? strtoull(argv[1], nullptr, 10) : 200000ull;
  auto seed = argc > 2 ? (uint32_t)strtoul(argv[2], nullptr, 10) : 1u;
  rngState = seed ? seed : 1u;

  // Small alphabet (nucleotide-like) stresses the dominant-match structure.
  for (uint64_t t = 0; t < trials; ++t) {
    auto alphabetSize = (size_t)(1 + nextRand() % 4);   // 1..4 symbols
    auto s1 = randomString(nextRand() % 40, alphabetSize);
    auto s2 = randomString(nextRand() % 40, alphabetSize);
    if (!checkOne(s1, s2, t))
      return 1;
  }

  // A few larger-alphabet runs exercise the sparse-match regime.
  for (uint64_t t = 0; t < trials / 10; ++t) {
    auto alphabetSize = (size_t)(1 + nextRand() % 26);  // 1..26 symbols
    auto s1 = randomString(nextRand() % 60, alphabetSize);
    auto s2 = randomString(nextRand() % 60, alphabetSize);
    if (!checkOne(s1, s2, t))
      return 1;
  }

  printf("stress: all %llu trials passed.\n", (unsigned long long)trials);
  return 0;
}
