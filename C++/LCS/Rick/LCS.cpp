// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Based on:
//   Claus Rick, "Simple and fast linear space computation of longest common
//   subsequences", Information Processing Letters 75 (2000), pp. 275-281.
//
// See LCS.h for an overview.
//
#include "LCS.h"

#include <algorithm>                    // lower_bound, upper_bound, reverse
#include <climits>                      // INT64_MAX
#include <iterator>                     // ssize
#include <utility>                      // swap

//
// A match (i, j) has forward rank k when the longest common subsequence of
// the prefixes s1[0..i] and s2[0..j] ending at (i, j) has length k.  The k-th
// forward contour FC[k] is the antichain of "dominant" (minimal under the
// product order) rank-k matches.  Backward contours BC[k] are the symmetric
// notion applied to the reversed strings; a match with backward rank k ends a
// common suffix of length k.
//
// A contour is stored as an antichain sorted by ascending index1, which forces
// descending index2: if both coordinates increased the two matches would be
// comparable, contradicting the antichain property.
//

uint32_t LCS::Length(const string& s1, const string& s2) {
  return FindMidpoint(s1, s2).length;
}

LCS::Result LCS::FindMidpoint(const string& s1, const string& s2) {
  // Rick's time bound assumes m <= n.  LCS is symmetric, so swap the inputs to
  // make s1 the shorter string and un-swap the midpoint on return.
  auto swapped = s1.size() > s2.size();
  const string& a = swapped ? s2 : s1;
  const string& b = swapped ? s1 : s2;

  // indexesOf2MatchedByChar[ch] holds the ascending positions of ch in b.
  CHAR_TO_INDEXES indexesOf2MatchedByChar;
  for (auto j = 0; j < ssize(b); j++)
    indexesOf2MatchedByChar[b[j]].push_back(j);

  Result result;
  if (a.empty() || b.empty())
    return result;

  // Contours are 1-indexed; FC[0] and BC[0] remain empty as sentinels.
  vector<Contour> FC(1), BC(1);
  int64_t f = 0;                        // # forward contours computed
  int64_t bCount = 0;                   // # backward contours computed

  for (;;) {
    // Alternate: FC[1], BC[1], FC[2], BC[2], ... so |f - bCount| <= 1.
    if (f <= bCount) {
      f++;
      FC.push_back(f == 1
        ? FirstForward(a, indexesOf2MatchedByChar)
        : NextForward(FC[f - 1], a, indexesOf2MatchedByChar));
      if (FC[f].empty())
        return result;                  // No matches: LCS length is zero.
    }
    else {
      bCount++;
      BC.push_back(bCount == 1
        ? FirstBackward(a, indexesOf2MatchedByChar)
        : NextBackward(BC[bCount - 1], a, indexesOf2MatchedByChar));
    }

    if (f >= 1 && bCount >= 1 && Crossed(FC[f], BC[bCount])) {
      auto midpoint = Midpoint(FC[f], BC[bCount]);
      if (swapped)
        swap(midpoint.index1, midpoint.index2);
      result.length = (uint32_t)(f + bCount - 1);
      result.hasMidpoint = true;
      result.midpoint = midpoint;
      return result;
    }
  }
}

//
// FC[1] consists of the minimal matches: those (i, j) for which no other match
// lies at the top-left.  Sweeping rows top to bottom, the first occurrence of
// s1[i] in s2 is the only per-row candidate, and the running minimum of the
// retained second coordinates filters matches dominated by an earlier row.
//
LCS::Contour LCS::FirstForward(
  const string& s1, const CHAR_TO_INDEXES& indexesOf2MatchedByChar) {
  Contour contour;
  int64_t minJ = INT64_MAX;
  for (auto i = 0; i < ssize(s1); i++) {
    auto it = indexesOf2MatchedByChar.find(s1[i]);
    if (it == indexesOf2MatchedByChar.end() || it->second.empty())
      continue;
    auto j = it->second.front();
    if (j < minJ) {
      contour.push_back({ .index1 = i, .index2 = j });
      minJ = j;
    }
  }
  return contour;
}

//
// FC[k + 1] is the set of minimal matches reachable from FC[k]: matches
// strictly bottom-right of some match in FC[k].  Because FC[k] is sorted by
// ascending index1, the reachable region below row i is bounded by the second
// coordinate of the last contour match above row i; the next occurrence of
// s1[i] past that bound is the only per-row candidate, and the running
// minimum filter again removes dominated matches.
//
LCS::Contour LCS::NextForward(
  const Contour& contour, const string& s1,
  const CHAR_TO_INDEXES& indexesOf2MatchedByChar) {
  Contour next;
  int64_t minJ = INT64_MAX;
  size_t l = 0;
  auto start = contour.empty() ? 0 : contour[0].index1 + 1;
  for (auto i = start; i < ssize(s1); i++) {
    while (l < contour.size() && contour[l].index1 < i)
      l++;
    if (l == 0)
      continue;                         // No contour match above row i.
    auto bound = contour[l - 1].index2;
    auto it = indexesOf2MatchedByChar.find(s1[i]);
    if (it == indexesOf2MatchedByChar.end() || it->second.empty())
      continue;
    const auto& positions = it->second;
    auto p = upper_bound(positions.begin(), positions.end(), bound);
    if (p == positions.end())
      continue;
    auto j = *p;
    if (j < minJ) {
      next.push_back({ .index1 = i, .index2 = j });
      minJ = j;
    }
  }
  return next;
}

//
// BC[1] consists of the maximal matches: those (i, j) for which no other match
// lies at the bottom-right.  This is the forward computation applied from the
// back of the strings; sweeping rows bottom to top, the last occurrence of
// s1[i] in s2 is the only per-row candidate.
//
LCS::Contour LCS::FirstBackward(
  const string& s1, const CHAR_TO_INDEXES& indexesOf2MatchedByChar) {
  Contour contour;
  int64_t maxJ = -1;
  for (auto i = ssize(s1) - 1; i >= 0; i--) {
    auto it = indexesOf2MatchedByChar.find(s1[i]);
    if (it == indexesOf2MatchedByChar.end() || it->second.empty())
      continue;
    auto j = it->second.back();
    if (j > maxJ) {
      contour.push_back({ .index1 = i, .index2 = j });
      maxJ = j;
    }
  }
  reverse(contour.begin(), contour.end());  // Restore ascending index1.
  return contour;
}

//
// BC[k + 1] is the set of maximal matches reachable backward from BC[k]:
// matches strictly top-left of some match in BC[k].  This mirrors NextForward
// with the sweep direction reversed and "next occurrence" replaced by
// "previous occurrence".
//
LCS::Contour LCS::NextBackward(
  const Contour& contour, const string& s1,
  const CHAR_TO_INDEXES& indexesOf2MatchedByChar) {
  Contour next;
  int64_t maxJ = -1;
  size_t l = contour.size();            // # contour matches below row i
  for (auto i = ssize(s1) - 1; i >= 0; i--) {
    while (l > 0 && contour[l - 1].index1 > i)
      l--;
    if (l == contour.size())
      continue;                         // No contour match below row i.
    auto bound = contour[l].index2;     // Max index2 among matches below.
    auto it = indexesOf2MatchedByChar.find(s1[i]);
    if (it == indexesOf2MatchedByChar.end() || it->second.empty())
      continue;
    const auto& positions = it->second;
    auto p = lower_bound(positions.begin(), positions.end(), bound);
    if (p == positions.begin())
      continue;                         // No occurrence before bound.
    auto j = *--p;
    if (j > maxJ) {
      next.push_back({ .index1 = i, .index2 = j });
      maxJ = j;
    }
  }
  reverse(next.begin(), next.end());    // Restore ascending index1.
  return next;
}

//
// Two contours have crossed when no backward match is strictly bottom-right of
// any forward match (Rick, Lemma 3).  Before the crossing, BC is entirely
// bottom-right of FC.
//
bool LCS::Crossed(const Contour& forward, const Contour& backward) {
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 > x.index1 && y.index2 > x.index2)
        return false;
  return true;
}

//
// After the contours have crossed, a midpoint is any forward match x for which
// some backward match y lies non-strictly bottom-right of x.  Such an x has
// backward rank b (its forward rank is already f), so by Rick's Lemma 1 it
// lies on some LCS.  Lemma 3(2) guarantees one exists.
//
LCS::Match LCS::Midpoint(const Contour& forward, const Contour& backward) {
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 >= x.index1 && y.index2 >= x.index2)
        return x;
  return { .index1 = -1, .index2 = -1 };
}
