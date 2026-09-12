// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Based on:
//   Claus Rick, "Simple and fast linear space computation of longest common
//   subsequences", Information Processing Letters 75 (2000), pp. 275-281.
//
// Purpose:
//
// This class implements the contour machinery from the Rick (2000) paper.
// Forward contours (FC) and backward contours (BC) are computed in an
// alternating fashion until the two most recently computed contours cross.
// At that point the LCS length is p = f + b - 1 and a "midpoint" match is
// identified, where f and b are the ranks of the two crossing contours.
//
// Correspondence() reconstructs the full LCS by recursing at that midpoint via
// Hirschberg().  Only the two most recent contours of each direction are
// retained, so the working set is linear in the input lengths.
//
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

using namespace std;

class LCS {
public:
  struct Match {
    int64_t index1 = 0;
    int64_t index2 = 0;
  };

  struct Result {
    uint32_t length = 0;
    bool hasMidpoint = false;
    Match midpoint;
  };

  static uint32_t Length(string_view s1, string_view s2);
  static Result FindMidpoint(string_view s1, string_view s2);
  static string Correspondence(string_view s1, string_view s2);

private:
  // A contour is an antichain of dominant matches sorted by ascending index1
  // (which forces descending index2).
  typedef vector<Match> Contour;
  typedef unordered_map<char, vector<int64_t>> CHAR_TO_INDEXES;

  static void Hirschberg(string_view s1, string_view s2, string& lcs);

  static Contour FirstForward(
    string_view s1, const CHAR_TO_INDEXES& indexesOf2MatchedByChar);
  static Contour NextForward(
    const Contour& contour, string_view s1,
    const CHAR_TO_INDEXES& indexesOf2MatchedByChar);
  static Contour FirstBackward(
    string_view s1, const CHAR_TO_INDEXES& indexesOf2MatchedByChar);
  static Contour NextBackward(
    const Contour& contour, string_view s1,
    const CHAR_TO_INDEXES& indexesOf2MatchedByChar);
  static bool Crossed(const Contour& forward, const Contour& backward);
  static Match Midpoint(const Contour& forward, const Contour& backward);
};
