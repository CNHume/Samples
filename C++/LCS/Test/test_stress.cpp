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
#include "modes/LCSText.h"
#include "Delta.h"
#include "modes/LCSRecord.h"

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

  const char* algoName(Algorithm algorithm) {
    switch (algorithm) {
    case Algorithm::Automatic:
      return "Automatic";
    case Algorithm::EditDistance:
      return "Myers";
    case Algorithm::Matches:
      return "Hunt-Szymanski";
    case Algorithm::RickSpace:
      return "RickSpace";
    case Algorithm::RickTime:
      return "RickTime";
    }
    return "?";
  }

  bool checkOne(const u32string& s1, const u32string& s2, uint64_t trial) {
    auto expected = dpLength(s1, s2);
    const Algorithm algos[4] = {
      Algorithm::EditDistance,
      Algorithm::Matches,
      Algorithm::RickSpace,
      Algorithm::RickTime,
    };
    for (auto algorithm : algos) {
      shared_ptr<Delta> deltas;
      auto length = modes::Text::Correspondence(
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

  //
  // Record path (line/word): mirror the text-path fuzz over sequences of
  // records.  Records are drawn from a pool of distinct strings, so the pool
  // size plays the role of the alphabet size and controls redundancy.
  //

  uint32_t dpLengthRecords(const vector<string>& a, const vector<string>& b) {
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

  bool checkRecordTrace(const shared_ptr<Delta> deltas, uint32_t length,
                        const vector<string>& r1, const vector<string>& r2,
                        const char* algo) {
    uint32_t sum = 0;
    uint32_t prior1 = 0;
    uint32_t prior2 = 0;
    for (auto d = deltas; d != nullptr; d = dynamic_pointer_cast<Delta>(d->next)) {
      auto len1 = d->end1 - d->begin1;
      auto len2 = d->end2 - d->begin2;
      if (d->begin1 < prior1 || d->begin2 < prior2 ||
          len1 != len2 || d->end1 > r1.size() || d->end2 > r2.size()) {
        fprintf(stderr, "[%s] invalid record match trace\n", algo);
        return false;
      }
      for (uint32_t k = 0; k < len1; ++k)
        if (r1[d->begin1 + k] != r2[d->begin2 + k]) {
          fprintf(stderr, "[%s] non-matching record in trace\n", algo);
          return false;
        }
      sum += len1;
      prior1 = d->end1;
      prior2 = d->end2;
    }
    if (sum != length) {
      fprintf(stderr, "[%s] record trace length %u != reported %u\n", algo, sum, length);
      return false;
    }
    return true;
  }

  bool checkOneRecord(const vector<string>& r1, const vector<string>& r2, uint64_t trial) {
    auto expected = dpLengthRecords(r1, r2);
    const Algorithm recordAlgos[4] = {
      Algorithm::EditDistance,
      Algorithm::Matches,
      Algorithm::RickSpace,
      Algorithm::RickTime,
    };
    for (auto algorithm : recordAlgos) {
      shared_ptr<Delta> deltas;
      auto length = modes::Record::Correspondence(
        &deltas, nullptr, r1, r2, algorithm, false, false, false, false, 0, 0, 0);
      if (length != expected) {
        fprintf(stderr, "[trial %llu] %s length %u != DP %u\n",
          (unsigned long long)trial, algoName(algorithm), length, expected);
        return false;
      }
      if (!checkRecordTrace(deltas, length, r1, r2, algoName(algorithm)))
        return false;
    }
    return true;
  }

  vector<string> randomRecords(size_t len, size_t poolSize) {
    vector<string> pool;
    pool.reserve(poolSize);
    for (size_t i = 0; i < poolSize; ++i)
      pool.push_back("w" + to_string(i));
    vector<string> r;
    r.reserve(len);
    for (size_t i = 0; i < len; ++i)
      r.push_back(pool[nextRand() % poolSize]);
    return r;
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

int runRecordStress(int argc, char* argv[]) {
  auto trials = argc > 1 ? strtoull(argv[1], nullptr, 10) : 100000ull;
  auto seed = argc > 2 ? (uint32_t)strtoul(argv[2], nullptr, 10) : 1u;
  rngState = seed ? seed : 1u;

  // Small record pool (redundant) stresses the match structure.
  for (uint64_t t = 0; t < trials; ++t) {
    auto poolSize = (size_t)(1 + nextRand() % 4);
    auto r1 = randomRecords(nextRand() % 40, poolSize);
    auto r2 = randomRecords(nextRand() % 40, poolSize);
    if (!checkOneRecord(r1, r2, t))
      return 1;
  }

  // Larger record pool (sparse) exercises the few-matches regime.
  for (uint64_t t = 0; t < trials / 10; ++t) {
    auto poolSize = (size_t)(1 + nextRand() % 26);
    auto r1 = randomRecords(nextRand() % 60, poolSize);
    auto r2 = randomRecords(nextRand() % 60, poolSize);
    if (!checkOneRecord(r1, r2, t))
      return 1;
  }

  printf("record stress: all %llu trials passed.\n", (unsigned long long)trials);
  return 0;
}
