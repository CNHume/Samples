// Longest Common Subsequence (LCS) -- Rick (2000) linear-space algorithm.
//
// Based on:
//   Claus Rick, "Simple and fast linear space computation of longest common
//   subsequences", Information Processing Letters 75 (2000), pp. 275-281.
//
// See LCS.h for an overview.
//
#include "LCS.h"

#include <algorithm>                    // min, reverse
#include <climits>                      // INT64_MAX
#include <iterator>                     // ssize
#include <utility>                      // move, swap

//
// A match (i, j) has forward rank k when the longest common subsequence of
// the prefixes s1[0..i] and s2[0..j] ending at (i, j) has length k.  The k-th
// forward contour FC[k] is the antichain of "dominant" (minimal under the
// product order) rank-k matches.  Backward contours BC[k] are the symmetric
// notion applied to the reversed strings; a match with backward rank k ends a
// common suffix of length k.
//
// "Dominant" means not dominated: match (i1, j1) dominates (i2, j2) when
// i1 <= i2 and j1 <= j2 and they differ.  A forward contour retains the
// minimal dominant matches, a backward contour the maximal ones.  Two
// dominant matches are therefore incomparable -- neither lies top-left nor
// bottom-right of the other -- which is the antichain property.
//
// A contour is stored as an antichain sorted by ascending index1, which forces
// descending index2: if both coordinates increased the two matches would be
// comparable, contradicting the antichain property.  The running-minimum
// (forward) / running-maximum (backward) test in First*/Next* computes exactly
// these dominant matches.
//

uint64_t LCS::Candidates = 0;
uint64_t LCS::Rows = 0;

void LCS::ResetCandidates() {
  Candidates = 0;
  Rows = 0;
}

//
// Builds O(1) next/previous occurrence tables for s2 in O(s * n) time and
// space, where s is the number of distinct characters.  This is the
// "LeftPos" preprocessing that lets the contour scan jump straight to the
// next (or previous) occurrence of a character instead of binary-searching a
// per-character position list.
//
LCS::Occurrences LCS::BuildOccurrences(string_view b) {
  Occurrences occ;
  occ.n = ssize(b);
  occ.charToId.assign(256, -1);

  vector<char> symbols;
  for (char c : b) {
    auto uc = (unsigned char)c;
    if (occ.charToId[uc] < 0) {
      occ.charToId[uc] = (int64_t)symbols.size();
      symbols.push_back(c);
    }
  }

  auto s = ssize(symbols);
  occ.next.assign(s, vector<int64_t>(occ.n + 1, occ.n));
  occ.prev.assign(s, vector<int64_t>(occ.n, -1));

  // next[id][j] = first occurrence of symbol id at or after position j.
  for (auto j = occ.n - 1; j >= 0; j--) {
    for (auto id = 0; id < s; id++)
      occ.next[id][j] = occ.next[id][j + 1];
    occ.next[occ.charToId[(unsigned char)b[j]]][j] = j;
  }

  // prev[id][j] = last occurrence of symbol id at or before position j.
  for (auto j = 0; j < occ.n; j++) {
    for (auto id = 0; id < s; id++)
      occ.prev[id][j] = j > 0 ? occ.prev[id][j - 1] : -1;
    occ.prev[occ.charToId[(unsigned char)b[j]]][j] = j;
  }

  return occ;
}

uint32_t LCS::Length(string_view s1, string_view s2) {
  return FindMidpoint(s1, s2).length;
}

LCS::Result LCS::FindMidpoint(string_view s1, string_view s2) {
  // Rick's time bound assumes m <= n.  LCS is symmetric, so swap the inputs to
  // make s1 the shorter string and un-swap the midpoint on return.
  auto swapped = s1.size() > s2.size();
  string_view a = swapped ? s2 : s1;
  string_view b = swapped ? s1 : s2;

  // O(1) next/previous occurrence tables for b.
  auto occ = BuildOccurrences(b);

  Result result;
  if (a.empty() || b.empty())
    return result;

  //
  // Only the two most recent contours of each direction are retained, so the
  // working set stays O(|a| + |b|) (linear) instead of growing with p.
  //
  Contour fc, fcPrev, bc, bcPrev;
  int64_t fCount = 0;                   // # forward contours computed
  int64_t bCount = 0;                   // # backward contours computed

  for (;;) {
    // Alternate: FC[1], BC[1], FC[2], BC[2], ... so |fCount - bCount| <= 1.
    if (fCount <= bCount) {
      fCount++;
      fcPrev = move(fc);
      if (fCount == 1) {
        fc = FirstForward(a, occ);
      }
      else {
        // Lemma 5: only extend from forward matches still "uncovered" by the
        // backward contours (those with a BC match strictly below-right).
        auto sources = CutForward(fcPrev, bc);
        // Skip rows that cannot hold a match of backward rank >= bCount:
        // such a match needs bCount - 1 further rows below it.
        auto rowLimit = ssize(a) - bCount + 1;
        fc = NextForward(sources.empty() ? fcPrev : sources,
          a, occ, rowLimit);
      }
      if (fc.empty())
        return result;                  // No matches: LCS length is zero.
    }
    else {
      bCount++;
      bcPrev = move(bc);
      if (bCount == 1) {
        bc = FirstBackward(a, occ);
      }
      else {
        // Symmetric cut for the backward direction.
        auto sources = CutBackward(bcPrev, fc);
        // Skip rows that cannot hold a match of forward rank >= fCount (= bCount):
        // such a match needs bCount - 1 rows above it.
        auto rowFloor = bCount - 1;
        bc = NextBackward(sources.empty() ? bcPrev : sources,
          a, occ, rowFloor);
      }
    }

    if (fCount >= 1 && bCount >= 1 && Crossed(fc, bc)) {
      auto midpoint = Midpoint(fc, bc);
      if (swapped)
        swap(midpoint.index1, midpoint.index2);
      result.length = (uint32_t)(fCount + bCount - 1);
      result.hasMidpoint = true;
      result.midpoint = midpoint;
      return result;
    }
  }
}

string LCS::Correspondence(string_view s1, string_view s2) {
  string lcs;
  Hirschberg(s1, s2, lcs);
  return lcs;
}

//
// Hirschberg (1975) recursion: split at the midpoint match and recurse on the
// prefix and suffix it separates, exactly as in Hirschberg's divide-and-conquer
// for the LCS problem.  The midpoint lies near the middle of some LCS (its
// forward and backward ranks differ by at most one), so the recursion depth is
// O(log p) and the working memory remains linear.
//
void LCS::Hirschberg(string_view s1, string_view s2, string& lcs) {
  if (s1.empty() || s2.empty())
    return;
  auto result = FindMidpoint(s1, s2);
  if (!result.hasMidpoint)
    return;
  auto i = result.midpoint.index1;
  auto j = result.midpoint.index2;
  Hirschberg(s1.substr(0, (size_t)i), s2.substr(0, (size_t)j), lcs);
  lcs.push_back(s1[(size_t)i]);
  Hirschberg(s1.substr((size_t)i + 1), s2.substr((size_t)j + 1), lcs);
}

//
// FC[1] holds the dominant rank-1 matches: a match (index1, index2) is
// dominant when no other match lies at its top-left (no match has both a
// smaller-or-equal index1 and a smaller-or-equal index2).  Sweeping rows top
// to bottom, only the leftmost occurrence of s1[index1] in s2 can be dominant
// in its row, and the running minimum minIndexOf2 rejects any row whose
// leftmost occurrence is dominated by a match in an earlier row.
//
LCS::Contour LCS::FirstForward(
  string_view s1, const Occurrences& occ) {
  Contour contour;
  int64_t minIndexOf2 = INT64_MAX;      // running minimum: the dominance test
  for (auto index1 = 0; index1 < ssize(s1); index1++) {
    Rows++;
    auto id = occ.charToId[(unsigned char)s1[index1]];
    if (id < 0)
      continue;                         // Character never occurs in s2.
    auto index2 = occ.next[id][0];      // leftmost occurrence in this row
    if (index2 >= occ.n)
      continue;
    if (index2 < minIndexOf2) {
      Candidates++;
      contour.push_back({ .index1 = index1, .index2 = index2 });
      minIndexOf2 = index2;
    }
  }
  return contour;
}

//
// FC[k + 1] holds the dominant matches reachable from FC[k]: matches strictly
// bottom-right of some match in FC[k] that are not dominated by another such
// match.  Because FC[k] is sorted by ascending index1, the reachable region
// below row index1 is bounded by the index2 of the last contour match above
// row index1; the next occurrence of s1[index1] past that bound is the only
// per-row candidate, and the running-minimum test again rejects matches
// dominated by an earlier (top-left) reachable match.
//
LCS::Contour LCS::NextForward(
  const Contour& contour, string_view s1,
  const Occurrences& occ, int64_t rowLimit) {
  Contour next;
  int64_t minIndexOf2 = INT64_MAX;      // running minimum: the dominance test
  size_t matchesAbove = 0;              // # contour matches above the row
  auto start = contour.empty() ? 0 : contour[0].index1 + 1;
  auto end = min(ssize(s1), rowLimit);
  for (auto index1 = start; index1 < end; index1++) {
    Rows++;
    while (matchesAbove < contour.size() && contour[matchesAbove].index1 < index1)
      matchesAbove++;
    if (matchesAbove == 0)
      continue;                         // No contour match above this row.
    auto boundIndex2 = contour[matchesAbove - 1].index2;
    auto id = occ.charToId[(unsigned char)s1[index1]];
    if (id < 0)
      continue;                         // Character never occurs in s2.
    auto index2 = occ.next[id][boundIndex2 + 1];  // first occurrence > bound
    if (index2 >= occ.n)
      continue;
    if (index2 < minIndexOf2) {
      Candidates++;
      next.push_back({ .index1 = index1, .index2 = index2 });
      minIndexOf2 = index2;
    }
  }
  return next;
}

//
// BC[1] holds the dominant (maximal) rank-1 matches: those with no other match
// at their bottom-right.  This is the forward computation applied from the
// back of the strings; sweeping rows bottom to top, only the rightmost
// occurrence of s1[index1] in s2 can be dominant in its row, and the running
// maximum maxIndexOf2 rejects any row dominated by a match in a later row.
//
LCS::Contour LCS::FirstBackward(
  string_view s1, const Occurrences& occ) {
  Contour contour;
  int64_t maxIndexOf2 = -1;             // running maximum: the dominance test
  for (auto index1 = ssize(s1) - 1; index1 >= 0; index1--) {
    Rows++;
    auto id = occ.charToId[(unsigned char)s1[index1]];
    if (id < 0)
      continue;                         // Character never occurs in s2.
    auto index2 = occ.prev[id][occ.n - 1];  // rightmost occurrence in this row
    if (index2 < 0)
      continue;
    if (index2 > maxIndexOf2) {
      Candidates++;
      contour.push_back({ .index1 = index1, .index2 = index2 });
      maxIndexOf2 = index2;
    }
  }
  reverse(contour.begin(), contour.end());  // Restore ascending index1.
  return contour;
}

//
// BC[k + 1] holds the dominant matches reachable backward from BC[k]: matches
// strictly top-left of some match in BC[k] that are not dominated by another
// such match.  This mirrors NextForward with the sweep direction reversed,
// "next occurrence" replaced by "previous occurrence", and the running-maximum
// test selecting the maximal (dominant) matches.
//
LCS::Contour LCS::NextBackward(
  const Contour& contour, string_view s1,
  const Occurrences& occ, int64_t rowFloor) {
  Contour next;
  int64_t maxIndexOf2 = -1;             // running maximum: the dominance test
  size_t matchesBelow = contour.size(); // # contour matches below the row
  for (auto index1 = ssize(s1) - 1; index1 >= rowFloor; index1--) {
    Rows++;
    while (matchesBelow > 0 && contour[matchesBelow - 1].index1 > index1)
      matchesBelow--;
    if (matchesBelow == contour.size())
      continue;                         // No contour match below this row.
    auto boundIndex2 = contour[matchesBelow].index2;  // Max index2 below.
    if (boundIndex2 <= 0)
      continue;                         // No occurrence can precede 0.
    auto id = occ.charToId[(unsigned char)s1[index1]];
    if (id < 0)
      continue;                         // Character never occurs in s2.
    auto index2 = occ.prev[id][boundIndex2 - 1];  // last occurrence < bound
    if (index2 < 0)
      continue;
    if (index2 > maxIndexOf2) {
      Candidates++;
      next.push_back({ .index1 = index1, .index2 = index2 });
      maxIndexOf2 = index2;
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

//
// Lemma 5 contour cutting.  A forward-contour match x whose backward rank does
// not exceed b (i.e., there is no BC match strictly below-right of x) can only
// be extended into matches of backward rank below b, whose rank sum cannot
// reach the LCS.  Such sources are pruned; the retained (uncovered) sources
// are exactly those with a backward-contour match strictly below-right.
//
LCS::Contour LCS::CutForward(const Contour& forward, const Contour& backward) {
  Contour kept;
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 > x.index1 && y.index2 > x.index2) {
        kept.push_back(x);
        break;
      }
  return kept;
}

//
// Symmetric cut for backward contours: retain backward matches y that have a
// forward-contour match strictly above-left, i.e., whose forward rank exceeds
// the current forward contour count.
//
LCS::Contour LCS::CutBackward(const Contour& backward, const Contour& forward) {
  Contour kept;
  for (const auto& y : backward)
    for (const auto& x : forward)
      if (x.index1 < y.index1 && x.index2 < y.index2) {
        kept.push_back(y);
        break;
      }
  return kept;
}
