// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// 2026-10-21 CNHume  Created reconstruction regression tests.
//
// The reconstruction oracle asserts two independent properties:
//
//   1. The algorithm's reported LCS length matches an independent O(n*m)
//      dynamic-programming length for small inputs.
//   2. The reconstructed match trace is a genuine LCS: its runs are ordered,
//      match equal symbols, and sum to the reported length.  Together these
//      prove the trace is a valid common subsequence of maximum length.
//
// Property 2 is the stronger check: it is what catches a trace whose length
// is right but whose reconstruction is incomplete (the Pair destructor bug).
//
#include "doctest/doctest.h"
#include "LCSText.h"
#include "Delta.h"
#include "LCSMyers.h"

#include <algorithm>                    // for equal, max
#include <cstdint>                      // for uint32_t
#include <random>                       // for mt19937
#include <stdexcept>                    // for runtime_error
#include <string>
#include <vector>

using namespace lcs;

using namespace std;

namespace {
  const LCS::Algorithm algos[4] = {
    LCS::Algorithm::HuntSzymanski,
    LCS::Algorithm::Myers,
    LCS::Algorithm::RickSpace,
    LCS::Algorithm::RickTime,
  };

  u32string toU32(const string& s) {
    return u32string(s.begin(), s.end());
  }

  // Reference O(n*m) DP LCS length: independent ground truth for small inputs.
  uint32_t dpLcsLength(const u32string& a, const u32string& b) {
    auto n = a.size();
    auto m = b.size();
    vector<vector<uint32_t>> d(n + 1, vector<uint32_t>(m + 1, 0));
    for (size_t i = 1; i <= n; ++i)
      for (size_t j = 1; j <= m; ++j)
        d[i][j] = a[i - 1] == b[j - 1] ?
          d[i - 1][j - 1] + 1 : max(d[i - 1][j], d[i][j - 1]);
    return d[n][m];
  }

  // Assert the coalesced match Deltas form a valid LCS trace of `length`.
  void checkTrace(const shared_ptr<Delta> deltas, uint32_t length,
                  const u32string& s1, const u32string& s2) {
    uint32_t sum = 0;
    uint32_t priorEnd1 = 0;
    uint32_t priorEnd2 = 0;
    for (auto d = deltas; d != nullptr; d = dynamic_pointer_cast<Delta>(d->next)) {
      auto len1 = d->end1 - d->begin1;
      auto len2 = d->end2 - d->begin2;
      REQUIRE(d->begin1 >= priorEnd1);
      REQUIRE(d->begin2 >= priorEnd2);
      REQUIRE(len1 == len2);
      REQUIRE(d->end1 <= s1.size());
      REQUIRE(d->end2 <= s2.size());
      REQUIRE(equal(
        s1.begin() + d->begin1, s1.begin() + d->end1,
        s2.begin() + d->begin2));
      sum += len1;
      priorEnd1 = d->end1;
      priorEnd2 = d->end2;
    }
    CHECK(sum == length);
  }

  // Run one (algorithm, s1, s2) case through the reconstruction oracle.
  void checkAlgo(LCS::Algorithm algo,
                 const u32string& s1, const u32string& s2,
                 uint32_t expected) {
    shared_ptr<Delta> deltas;
    auto length = LCSText::Correspondence(
      &deltas, nullptr, s1, s2, algo, false, false, false, false, 0, 0, 0);
    CAPTURE((int)algo);
    CHECK(length == expected);
    checkTrace(deltas, length, s1, s2);
  }
}

TEST_CASE("reconstruction is a valid LCS (fixed cases, DP cross-check)") {
  const vector<pair<string, string>> cases = {
    { "ACGTACGTACGT", "ACGTAGGTACGT" },  // the Pair destructor bug case
    { "ABCDEFGHIJ", "ABXDEFGHIJ" },      // clean substitution
    { "AAAAAA", "AAAAAA" },              // identical, maximal redundancy
    { "AAAAAA", "BBBBBB" },              // disjoint
    { "AABB", "ABAB" },                  // redundant
    { "A", "A" },
    { "A", "B" },
    { "", "" },
    { "", "A" },
    { "A", "" },
    { "ACGT", "ACGTACGT" },              // prefix
    { "TACGTACGT", "ACGTACGT" },         // shifted
    { "demanoeuvre", "reendeavour" },    // classic LCS example
    { "thisisatest", "testing123testing" },
  };
  for (auto& [s1, s2] : cases) {
    auto u1 = toU32(s1);
    auto u2 = toU32(s2);
    auto expected = dpLcsLength(u1, u2);
    for (auto algo : algos)
      checkAlgo(algo, u1, u2, expected);
  }
}

TEST_CASE("reconstruction valid on random low-alphabet strings") {
  mt19937 rng(12345);
  const string alphabets[] = { "ACGT", "AC", "AB" };
  for (int iter = 0; iter < 200; ++iter) {
    const string& ab = alphabets[rng() % 3];
    auto n1 = rng() % 25;
    auto n2 = rng() % 25;
    string s1, s2;
    for (auto i = 0u; i < n1; ++i) s1.push_back(ab[rng() % ab.size()]);
    for (auto i = 0u; i < n2; ++i) s2.push_back(ab[rng() % ab.size()]);
    auto u1 = toU32(s1);
    auto u2 = toU32(s2);
    auto expected = dpLcsLength(u1, u2);
    for (auto algo : algos)
      checkAlgo(algo, u1, u2, expected);
  }
}

TEST_CASE("algorithms agree on larger redundant inputs") {
  mt19937 rng(999);
  const string ab = "ACGT";
  for (int iter = 0; iter < 30; ++iter) {
    auto n1 = 100 + rng() % 200;
    auto n2 = 100 + rng() % 200;
    string s1, s2;
    for (auto i = 0u; i < n1; ++i) s1.push_back(ab[rng() % 4]);
    for (auto i = 0u; i < n2; ++i) s2.push_back(ab[rng() % 4]);
    auto u1 = toU32(s1);
    auto u2 = toU32(s2);
    uint32_t reference = 0;
    for (int k = 0; k < 4; ++k) {
      shared_ptr<Delta> deltas;
      auto length = LCSText::Correspondence(
        &deltas, nullptr, u1, u2, algos[k], false, false, false, false, 0, 0, 0);
      if (k == 0)
        reference = length;
      else
        CHECK(length == reference);
      checkTrace(deltas, length, u1, u2);
    }
  }
}

TEST_CASE("string fixtures load and reconstruct validly") {
  auto u1 = LCSText::Read(
    string(TEST_DATA_DIR) + "/Test/String/demanoeuvre.txt");
  auto u2 = LCSText::Read(
    string(TEST_DATA_DIR) + "/Test/String/reendeavour.txt");
  CHECK(u1.size() == 11);
  CHECK(u2.size() == 11);
  auto expected = dpLcsLength(u1, u2);
  for (auto algo : algos)
    checkAlgo(algo, u1, u2, expected);
}

TEST_CASE("Myers aborts when the edit distance exceeds its budget") {
  // Disjoint alphabets drive the composition pre-filter's lower bound past
  // the memory-derived D budget, so Find() must throw instead of running the
  // O(D^2) sweep on a dissimilar input.
  u32string s1(1000000, U'A');
  u32string s2(1000000, U'B');
  CHECK_THROWS_AS(
    LCSMyers::Find(nullptr, nullptr, s1, s2),
    std::runtime_error);
}
