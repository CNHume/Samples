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
#include "LCSText.h"

#include <algorithm>                    // for lower_bound, swap
#include <chrono>
#include <cmath>                        // for pow
#include <cstdint>                      // for uint32_t, uint64_t
#include <cstdio>                       // for fprintf, printf
#include <cstdlib>                      // for atoi, strtoull
#include <cstring>                      // for strcmp
#include <deque>
#include <exception>                    // for exception
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

  //
  // Region benchmark: time each algorithm over a grid of (input length,
  // alphabet size) on near-identical inputs.  The substitution count sets the
  // EditDistance (D = 2*subs for substitution-only edits); the skew parameter
  // decouples Redundancy from AlphabetSize (uniform gives rho ~ n/s, while a
  // Zipf-like skew concentrates frequency so rho stays high even at large s).
  //
  uint32_t regionRng = 0x12345678u;
  uint32_t regionRand() {
    uint32_t x = regionRng;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    return regionRng = x;
  }

  u32string regionReference(size_t n, size_t s, double skew) {
    // Zipf-like weights: symbol i has weight (i+1)^(-skew); skew 0 is uniform.
    vector<double> weight(s);
    double total = 0;
    for (size_t i = 0; i < s; ++i) {
      weight[i] = pow((double)(i + 1), -skew);
      total += weight[i];
    }
    vector<double> cum(s + 1, 0);
    for (size_t i = 0; i < s; ++i)
      cum[i + 1] = cum[i] + weight[i] / total;

    u32string r;
    r.reserve(n);
    for (size_t i = 0; i < n; ++i) {
      auto x = (double)regionRand() / (double)UINT32_MAX;
      auto it = upper_bound(cum.begin(), cum.end(), x);
      auto sym = (size_t)(it - cum.begin()) - 1;
      if (sym >= s)
        sym = s - 1;
      r.push_back((char32_t)('A' + sym));
    }
    return r;
  }

  u32string regionMutate(const u32string& ref, size_t s, size_t subs) {
    u32string q = ref;
    for (size_t k = 0; k < subs; ++k) {
      auto pos = regionRand() % q.size();
      char32_t c;
      do { c = (char32_t)('A' + regionRand() % s); } while (c == q[pos]);
      q[pos] = c;
    }
    return q;
  }

  double regionRedundancy(const u32string& a, const u32string& b, size_t s) {
    vector<uint64_t> c1(s, 0), c2(s, 0);
    for (auto c : a)
      c1[(size_t)(c - U'A')]++;
    for (auto c : b)
      c2[(size_t)(c - U'A')]++;
    uint64_t r = 0;
    for (size_t i = 0; i < s; ++i)
      r += c1[i] * c2[i];
    return (double)r / a.size();       // matches per position (rho)
  }

  double regionTime(LCS::Algorithm algo, const u32string& a, const u32string& b) {
    shared_ptr<Delta> deltas;
    auto t0 = Clock::now();
    try {
      LCSText::Correspondence(
        &deltas, nullptr, a, b, algo, false, false, false, false, 0, 0, 0);
    }
    catch (const exception&) {
      return -1.0;                       // unavailable (e.g., D exceeds budget)
    }
    auto t1 = Clock::now();
    return chrono::duration<double>(t1 - t0).count();
  }

  int benchRegion(int argc, char* argv[]) {
    auto subs = argc > 0 ? (size_t)strtoull(argv[0], nullptr, 10) : 100;
    auto skew = argc > 1 ? strtod(argv[1], nullptr) : 0.0;
    auto dissimilar = argc > 2 ? (atoi(argv[2]) != 0) : false;
    const size_t lengths[] = { 1000, 10000, 100000 };
    const size_t alphabets[] = { 4, 20, 100, 1000 };
    const LCS::Algorithm algos[4] = {
      LCS::Algorithm::EditDistance,
      LCS::Algorithm::Matches,
      LCS::Algorithm::RickSpace,
      LCS::Algorithm::RickTime,
    };

    printf("InputLength\tAlphabetSize\tRedundancy\tEditDistance\tH-S\tMyers\tRickSpace\tRickTime\n");
    for (auto n : lengths) {
      for (auto s : alphabets) {
        regionRng = 0x12345678u;
        auto a = regionReference(n, s, skew);
        size_t D;
        u32string b;
        if (dissimilar) {
          // Independent random second string: a large, uncontrolled D.
          b = regionReference(n, s, skew);
          auto p = LCSRickTime::FindFlexibleLength(a, b);
          D = 2 * n - 2 * p;
        }
        else {
          b = regionMutate(a, s, subs);
          D = 2 * subs;
        }
        auto rho = regionRedundancy(a, b, s);
        printf("%zu\t%zu\t%.2f\t%zu", 2 * n, s, rho, D);
        for (auto algo : algos) {
          auto t = regionTime(algo, a, b);
          if (t < 0)
            printf("\tN/A");
          else
            printf("\t%.3f", t);
        }
        printf("\n");
      }
    }
    return 0;
  }
}

int runPerf(int argc, char* argv[]) {
  if (argc < 2) {
    fprintf(stderr,
      "usage: test perf <rick|measure|filterpairs|region> [args]\n");
    return 2;
  }
  const char* which = argv[1];
  if (strcmp(which, "rick") == 0)
    return benchRick(argc - 2, argv + 2);
  if (strcmp(which, "measure") == 0)
    return benchMeasure(argc - 2, argv + 2);
  if (strcmp(which, "filterpairs") == 0)
    return benchFilterPairs(argc - 2, argv + 2);
  if (strcmp(which, "region") == 0)
    return benchRegion(argc - 2, argv + 2);

  fprintf(stderr, "unknown perf benchmark '%s'\n", which);
  return 2;
}
