// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created the perf subcommand (absorbed from scratch/bench.cpp,
// measure.cpp, and hsbench.cpp).
//
// Performance benchmarks.  These are informational (timing/counters), not
// pass/fail assertions:
//
//   test perf rick <space|time> file1 file2
//   test perf measure [N] [K]
//   test perf filterpairs [n] [s]
//
#include "LCSRickSpace.h"
#include "LCSRickTime.h"

#include <algorithm>                    // for lower_bound, swap
#include <chrono>
#include <cstdint>                      // for uint32_t, uint64_t
#include <cstdio>                       // for fprintf, printf
#include <cstdlib>                      // for atoi, strtoull
#include <cstring>                      // for strcmp
#include <deque>
#include <fstream>
#include <iterator>
#include <memory>                       // for make_shared, shared_ptr
#include <string>
#include <tuple>                        // for make_tuple
#include <unordered_map>
#include <utility>                      // for move, swap
#include <vector>

using namespace lcs;

using namespace std;
using Clock = chrono::steady_clock;

namespace {
  //
  // bench.cpp: time LCSRickSpace or LCSRickTime on two raw sequence files.
  //
  u32string loadFile(const char* path) {
    ifstream f(path, ios::binary);
    string s((istreambuf_iterator<char>(f)), istreambuf_iterator<char>());
    while (!s.empty() && (s.back() == '\n' || s.back() == '\r'))
      s.pop_back();
    u32string u;
    u.reserve(s.size());
    for (char c : s)
      u.push_back((char32_t)(unsigned char)c);
    return u;
  }

  int benchRick(int argc, char* argv[]) {
    if (argc < 3) {
      fprintf(stderr, "usage: test perf rick <space|time> file1 file2\n");
      return 2;
    }
    auto a = loadFile(argv[1]);
    auto b = loadFile(argv[2]);
    if (a.size() > b.size())
      swap(a, b);

    uint32_t p = 0;
    shared_ptr<Pair> pairs = nullptr;
    auto t0 = Clock::now();
    if (strcmp(argv[0], "time") == 0)
      p = LCSRickTime::Find(&pairs, nullptr, a, b);
    else
      p = LCSRickSpace::Find(&pairs, nullptr, a, b);
    auto t1 = Clock::now();
    printf("p=%u time_ms=%lld\n", p,
      (long long)chrono::duration_cast<chrono::milliseconds>(t1 - t0).count());
    return 0;
  }

  //
  // measure.cpp: LCSRickSpace contour work on a near-diagonal (long-LCS) input.
  //
  int benchMeasure(int argc, char* argv[]) {
    auto N = argc > 0 ? (size_t)strtoull(argv[0], nullptr, 10) : 100000;
    auto K = argc > 1 ? (size_t)strtoull(argv[1], nullptr, 10) : 100;
    const char* alphabet = "ACGTN";

    uint32_t rngState = 0x12345678u;
    auto nextRand = [&rngState]() {
      uint32_t x = rngState;
      x ^= x << 13; x ^= x >> 17; x ^= x << 5;
      return rngState = x;
    };

    u32string s2;
    s2.reserve(N);
    for (size_t j = 0; j < N; ++j)
      s2.push_back((char32_t)alphabet[nextRand() % 5]);

    auto s1 = s2;
    vector<size_t> pos(N);
    for (size_t i = 0; i < N; ++i)
      pos[i] = i;
    for (size_t i = N - 1; i > 0; --i) {
      auto r = nextRand() % (i + 1);
      swap(pos[i], pos[r]);
    }
    for (size_t k = 0; k < K && k < N; ++k) {
      auto p = pos[k];
      char32_t c;
      do { c = (char32_t)alphabet[nextRand() % 5]; } while (c == s1[p]);
      s1[p] = c;
    }

    LCSRickSpace::ResetCounters();
    shared_ptr<Pair> pairs = nullptr;
    auto p = LCSRickSpace::Find(&pairs, nullptr, s1, s2);

    auto m = s1.size(), n = s2.size();
    printf("m=%zu n=%zu p=%u m-p=%zd n-p=%zd Rows=%llu Candidates=%llu "
      "Rows/(p*(n-p))=%g\n",
      m, n, p, (ptrdiff_t)(m - p), (ptrdiff_t)(n - p),
      (unsigned long long)LCSRickSpace::Rows,
      (unsigned long long)LCSRickSpace::Candidates,
      (double)LCSRickSpace::Rows / (double)((uint64_t)p * (uint64_t)(n - p)));
    return 0;
  }

  //
  // hsbench.cpp: FILTER_PAIRS benefit.  A self-contained Hunt-Szymanski with a
  // runtime filter toggle measures the Pair-allocation and wall-clock savings
  // in one binary (the production filter is a compile-time #define).
  //
  struct BPair {
    static uint64_t Total;
    uint32_t index1 = 0;
    uint32_t index2 = 0;
    shared_ptr<BPair> next;
    BPair(uint32_t i1, uint32_t i2, shared_ptr<BPair> n = nullptr)
      : index1(i1), index2(i2), next(std::move(n)) { ++Total; }
  };
  uint64_t BPair::Total = 0;

  static shared_ptr<BPair> pushPair(deque<shared_ptr<BPair>>& chains,
    ptrdiff_t index3, uint32_t index1, uint32_t index2) {
    auto prefix = index3 > 0 ? chains[index3 - 1] : nullptr;
    return make_shared<BPair>(index1, index2, prefix);
  }

  static uint32_t findLCS(const vector<deque<uint32_t>*>& matches, bool filter) {
    deque<shared_ptr<BPair>> chains;
    deque<uint32_t> prefixEnd;
    uint32_t index1 = 0;
    for (const auto it1 : matches) {
      auto dq2 = *it1;
      auto limit = prefixEnd.end();
      for (auto it2 = dq2.rbegin(); it2 != dq2.rend(); ++it2) {
        auto index2 = *it2;
        limit = lower_bound(prefixEnd.begin(), limit, index2);
        if (filter) {
          auto preferNextIndex2 = next(it2) != dq2.rend() &&
            (limit == prefixEnd.begin() || *prev(limit) < *next(it2));
          if (preferNextIndex2) continue;
        }
        auto index3 = distance(prefixEnd.begin(), limit);
        if (limit == prefixEnd.end()) {
          prefixEnd.push_back(index2);
          limit = prev(prefixEnd.end());
          chains.push_back(pushPair(chains, index3, index1, index2));
        }
        else if (index2 < *limit) {
          *limit = index2;
          chains[index3] = pushPair(chains, index3, index1, index2);
        }
      }
      ++index1;
    }
    return (uint32_t)prefixEnd.size();
  }

  int benchFilterPairs(int argc, char* argv[]) {
    uint32_t n = argc > 0 ? (uint32_t)atoi(argv[0]) : 10000;
    uint32_t s = argc > 1 ? (uint32_t)atoi(argv[1]) : 4;
    if (s == 0 || n % s != 0) {
      fprintf(stderr, "require s > 0 and s | n\n");
      return 2;
    }
    auto k = n / s;

    // Block-structured identical strings: s1 = s2, redundancy = n/s.
    deque<uint32_t> s1;
    for (uint32_t sym = 0; sym < s; ++sym)
      for (uint32_t i = 0; i < k; ++i)
        s1.push_back(sym);

    unordered_map<uint32_t, deque<uint32_t>> symToIndexes;
    for (uint32_t j = 0; j < n; ++j)
      symToIndexes[s1[j]].push_back(j);
    vector<deque<uint32_t>*> matches;
    uint64_t r = 0;
    for (uint32_t i = 0; i < n; ++i) {
      auto& dq = symToIndexes[s1[i]];
      matches.push_back(&dq);
      r += dq.size();
    }

    auto run = [&](bool filter) {
      BPair::Total = 0;
      auto t0 = Clock::now();
      auto length = findLCS(matches, filter);
      auto t1 = Clock::now();
      return make_tuple(length, BPair::Total,
        chrono::duration<double>(t1 - t0).count());
    };

    auto [lengthF, pairsF, timeF] = run(true);
    auto [lengthN, pairsN, timeN] = run(false);
    printf("n=%u s=%u redundancy=%.3f r=%llu length=%u "
      "pairs_filt=%llu pairs_nofilt=%llu "
      "time_filt=%.6f time_nofilt=%.6f t_ratio=%.3f\n",
      n, s, (double)r / n, (unsigned long long)r, lengthF,
      (unsigned long long)pairsF, (unsigned long long)pairsN,
      timeF, timeN, timeN / timeF);
    return 0;
  }
}

int runPerf(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr,
      "usage: test perf <rick|measure|filterpairs> [args]\n");
    return 2;
  }
  const char* which = argv[1];
  if (strcmp(which, "rick") == 0)
    return benchRick(argc - 2, argv + 2);
  if (strcmp(which, "measure") == 0)
    return benchMeasure(argc - 2, argv + 2);
  if (strcmp(which, "filterpairs") == 0)
    return benchFilterPairs(argc - 2, argv + 2);

  fprintf(stderr, "unknown perf benchmark '%s'\n", which);
  return 2;
}
