// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Verification harness.  Cross-checks LCS::FindMidpoint against:
//   * an O(m*n) dynamic-programming reference (ground truth), and
//   * a Hunt-Szymanski length computation mirroring the existing
//     C++/LCS implementation.
//
// The midpoint is validated independently: a match (i, j) lies on an LCS of
// length p iff LCS(s1[0..i-1], s2[0..j-1]) + 1 + LCS(s1[i+1..], s2[j+1..]) == p.
//
#include "LCS.h"

#include <algorithm>                    // lower_bound, max
#include <format>
#include <iostream>
#include <iterator>                     // ssize
#include <random>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

//
// Dynamic-programming LCS length (ground truth).
//
static uint32_t dpLength(const string& a, const string& b) {
  size_t m = a.size(), n = b.size();
  vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
  for (size_t i = 1; i <= m; i++)
    for (size_t j = 1; j <= n; j++)
      dp[i][j] = a[i - 1] == b[j - 1]
        ? dp[i - 1][j - 1] + 1
        : max(dp[i - 1][j], dp[i][j - 1]);
  return (uint32_t)dp[m][n];
}

//
// Hunt-Szymanski LCS length, mirroring the existing C++/LCS FindLCS.
//
static uint32_t huntSzymanskiLength(const string& a, const string& b) {
  unordered_map<char, vector<uint32_t>> positions;
  for (uint32_t j = 0; j < b.size(); j++)
    positions[b[j]].push_back(j);

  vector<uint32_t> prefixEnd;           // threshold array
  for (char c : a) {
    auto& matches = positions[c];
    for (auto it = matches.rbegin(); it != matches.rend(); ++it) {
      auto j = *it;
      auto limit = lower_bound(prefixEnd.begin(), prefixEnd.end(), j);
      if (limit == prefixEnd.end())
        prefixEnd.push_back(j);
      else if (j < *limit)
        *limit = j;
    }
  }
  return (uint32_t)prefixEnd.size();
}

//
// Verify the reported midpoint independently of the Rick implementation.
//
static bool checkMidpoint(
  const string& a, const string& b, const LCS::Result& result) {
  if (!result.hasMidpoint)
    return result.length == 0;

  auto i = result.midpoint.index1;
  auto j = result.midpoint.index2;
  if (i < 0 || j < 0 || i >= ssize(a) || j >= ssize(b))
    return false;
  if (a[(size_t)i] != b[(size_t)j])
    return false;

  auto before = dpLength(a.substr(0, (size_t)i), b.substr(0, (size_t)j));
  auto after = dpLength(a.substr((size_t)i + 1), b.substr((size_t)j + 1));
  return before + 1 + after == result.length;
}

static uint64_t failures = 0;

static void check(const string& a, const string& b) {
  auto result = LCS::FindMidpoint(a, b);
  auto d = dpLength(a, b);
  auto h = huntSzymanskiLength(a, b);
  bool ok = result.length == d && d == h && checkMidpoint(a, b, result);
  if (!ok) {
    failures++;
    if (failures <= 20)
      cout << format("FAIL ({}, {}) len={} dp={} hs={}\n",
        a, b, result.length, d, h);
  }
}

int main() {
  //
  // Fixed cases.
  //
  const vector<pair<string, string>> cases = {
    { "", "" },
    { "a", "" },
    { "", "a" },
    { "a", "a" },
    { "a", "b" },
    { "a", "aa" },
    { "aa", "a" },
    { "ab", "ba" },
    { "ab", "ab" },
    { "aba", "aba" },
    { "abcde", "ace" },
    { "AGGTAB", "GXTXAYB" },
    { "thisisatest", "testing123testing" },
    { "aaaaaaaaaa", "aaaaaaaaaa" },
  };
  for (auto& c : cases)
    check(c.first, c.second);

  //
  // Exhaustive: every pair of strings of length <= 6 over {a, b}.
  //
  vector<string> alphabet = { "a", "b" };
  vector<string> level = { "" };
  vector<string> all;
  for (int len = 0; len <= 6; len++) {
    vector<string> nextLevel;
    for (auto& s : level) {
      all.push_back(s);
      for (auto& ch : alphabet)
        nextLevel.push_back(s + ch);
    }
    level = move(nextLevel);
  }
  for (auto& x : all)
    for (auto& y : all)
      check(x, y);

  //
  // Random small-alphabet strings.
  //
  mt19937 rng(12345);
  uniform_int_distribution<int> lenDist(0, 12);
  uniform_int_distribution<int> charDist(0, 2);
  for (int it = 0; it < 200000; it++) {
    string a, b;
    for (int i = 0, la = lenDist(rng); i < la; i++)
      a.push_back((char)('a' + charDist(rng)));
    for (int i = 0, lb = lenDist(rng); i < lb; i++)
      b.push_back((char)('a' + charDist(rng)));
    check(a, b);
  }

  //
  // Random larger-alphabet strings.
  //
  uniform_int_distribution<int> lenDist2(0, 30);
  uniform_int_distribution<int> charDist2(0, 5);
  for (int it = 0; it < 20000; it++) {
    string a, b;
    for (int i = 0, la = lenDist2(rng); i < la; i++)
      a.push_back((char)('a' + charDist2(rng)));
    for (int i = 0, lb = lenDist2(rng); i < lb; i++)
      b.push_back((char)('a' + charDist2(rng)));
    check(a, b);
  }

  if (failures == 0)
    cout << "ALL TESTS PASSED\n";
  else
    cout << format("FAILURES: {}\n", failures);
  return failures == 0 ? 0 : 1;
}
