// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Command-line driver for the length and midpoint primitives.
//
// Usage: rick s1 s2
//
#include "LCS.h"

#include <iostream>
#include <string>

using namespace std;

int main(int argc, char* argv[]) {
  if (argc < 3) {
    cout << "Usage: rick s1 s2" << endl;
    return EXIT_FAILURE;
  }

  string s1 = argv[1];
  string s2 = argv[2];
  auto result = LCS::FindMidpoint(s1, s2);

  cout << "LCS length = " << result.length << endl;
  if (result.hasMidpoint)
    cout << "Midpoint = (" << result.midpoint.index1 << ", "
      << result.midpoint.index2 << ") -> '" << s1[result.midpoint.index1] << "'"
      << endl;

  return EXIT_SUCCESS;
}
