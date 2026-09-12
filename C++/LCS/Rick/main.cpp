// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Command-line driver for the LCS primitives.
//
// Usage: rick s1 s2
//
#include "LCS.h"

#include <format>
#include <iostream>
#include <string>

using namespace std;

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Usage: rick s1 s2\n";
    return EXIT_FAILURE;
  }

  string s1 = argv[1];
  string s2 = argv[2];
  auto result = LCS::FindMidpoint(s1, s2);

  cout << format("LCS length = {}\n", result.length);
  cout << format("LCS = '{}'\n", LCS::Correspondence(s1, s2));
  if (result.hasMidpoint)
    cout << format("Midpoint = ({}, {}) -> '{}'\n",
      result.midpoint.index1, result.midpoint.index2,
      s1[(size_t)result.midpoint.index1]);

  return EXIT_SUCCESS;
}
