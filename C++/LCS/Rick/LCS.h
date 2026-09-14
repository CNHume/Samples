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

  // Number of contour candidates generated since the last ResetCandidates().
  static uint64_t Candidates;

  // Number of row positions scanned since the last ResetCandidates().
  static uint64_t Rows;

  static void ResetCandidates();

private:
  // A contour is an antichain of dominant matches sorted by ascending index1,
  // which forces descending index2 (if both coordinates increased, one match
  // would lie top-left of the other and dominate it).
  //
  // Match (i1, j1) dominates (i2, j2) when i1 <= i2 and j1 <= j2 and they
  // differ.  A dominant match of a given rank is one not dominated by any
  // other match of that rank: forward contours keep the minimal (top-left)
  // dominant matches, backward contours the maximal (bottom-right) ones.
  typedef vector<Match> Contour;

  // O(1) next/previous occurrence tables, built once from the longer string.
  // charToId maps a character to a compact id (-1 if it never occurs);
  // next[id][j] is the smallest position >= j holding that character (n if
  // none); prev[id][j] is the largest position <= j holding it (-1 if none).
  // This is the "LeftPos/TopPos" preprocessing of the threshold-array scan.
  struct Occurrences {
    vector<int64_t> charToId;      // 256 entries, indexed by unsigned char
    vector<vector<int64_t>> next;  // [id][j], j in [0, n]
    vector<vector<int64_t>> prev;  // [id][j], j in [0, n)
    int64_t n = 0;                 // length of the string the table indexes
  };

  static Occurrences BuildOccurrences(string_view s2);

  static void Hirschberg(string_view s1, string_view s2, string& lcs);

  // Lemma 5 contour cutting: retain only the sources that can still extend an
  // LCS (the ones "uncovered" by the opposite contour).
  static Contour CutForward(const Contour& forward, const Contour& backward);
  static Contour CutBackward(const Contour& backward, const Contour& forward);

  static Contour FirstForward(string_view s1, const Occurrences& occ);
  static Contour NextForward(const Contour& contour, string_view s1,
    const Occurrences& occ, int64_t rowLimit);
  static Contour FirstBackward(string_view s1, const Occurrences& occ);
  static Contour NextBackward(const Contour& contour, string_view s1,
    const Occurrences& occ, int64_t rowFloor);
  static bool Crossed(const Contour& forward, const Contour& backward);
  static Match Midpoint(const Contour& forward, const Contour& backward);
};
