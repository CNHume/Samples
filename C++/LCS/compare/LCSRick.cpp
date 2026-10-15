// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-09 CNHume  Created LCSRick class
//
// Rick (2000) linear-space LCS, adapted to reuse the Hunt-Szymanski
// Match() output.
//
// See LCSRick.h for an overview.
//
#include "LCSRick.h"

#include <algorithm>                    // for min, reverse
#include <climits>                      // for INT64_MAX
#include <memory>                       // for make_shared
#include <unordered_map>                // for pointer -> symbol id
#include <utility>                      // for move

//
// A match (i, j) has forward rank k when the longest common subsequence of
// the prefixes s1[0..i] and s2[0..j] ending at (i, j) has length k.  The k-th
// forward contour FC[k] is the antichain of "dominant" (minimal under the
// product order) rank-k matches.  Backward contours BC[k] are the symmetric
// notion applied to the reversed strings.  Two contours are computed in an
// alternating fashion until they cross, which fixes the LCS length and a
// midpoint match.  Hirschberg recursion reconstructs the full LCS from the
// midpoints.
//
// Each contour scan needs, per row index1, the first (or last) occurrence of
// s1[index1] relative to a bound.  These queries are answered in O(1) by the
// next/previous occurrence tables built from the shared match lists.
//

uint32_t LCSRick::Find(shared_ptr<Pair>* pairs,
  const MATCHES& matches, uint32_t size1, uint32_t size2) {
  // Build O(1) occurrence tables once and amortize them over the whole
  // Hirschberg recursion (the O(ns) preprocessing of the Rick bound).
  auto occ = BuildOccurrences(matches, size1, size2);

  vector<Match> out;
  Hirschberg(occ, 0, size1, 0, size2, out);

  if (pairs != nullptr) {
    shared_ptr<Pair> head = nullptr;
    shared_ptr<Pair> tail = nullptr;
    for (const auto& match : out) {
      auto pair = make_shared<Pair>(
        (uint32_t)match.index1, (uint32_t)match.index2);
      if (tail == nullptr)
        head = pair;
      else
        tail->next = pair;
      tail = pair;
    }
    *pairs = head;
  }

  return (uint32_t)out.size();
}

//
// Builds O(1) next/previous occurrence tables from the shared match lists in
// O(s*n + r) time and space, where s is the number of distinct symbols
// (distinct match-list pointers) and r the total number of matches.  This is
// the "LeftPos/TopPos" preprocessing that lets each contour scan jump straight
// to the next (or previous) occurrence instead of binary-searching a list.
//
LCSRick::Occurrences LCSRick::BuildOccurrences(
  const MATCHES& matches, size_t size1, size_t size2) {
  Occurrences occ;
  occ.n = (int64_t)size2;

  // Two rows sharing a symbol share the same match-list pointer, so pointer
  // identity compresses the symbols without needing the symbol values.
  unordered_map<const INDEXES*, int64_t> pointerToId;
  vector<const INDEXES*> lists;         // lists[id] = sorted positions of symbol id
  occ.rowId.assign(size1, -1);
  for (size_t i = 0; i < size1; i++) {
    const auto* p = matches[i];
    auto it = pointerToId.find(p);
    int64_t id;
    if (it == pointerToId.end()) {
      id = (int64_t)lists.size();
      pointerToId.emplace(p, id);
      lists.push_back(p);
    }
    else {
      id = it->second;
    }
    occ.rowId[i] = id;
  }

  auto s = lists.size();
  occ.next.assign(s, vector<int64_t>((size_t)size2 + 1, occ.n));
  occ.prev.assign(s, vector<int64_t>((size_t)size2, -1));

  for (size_t id = 0; id < s; id++) {
    const auto& list = *lists[id];      // sorted positions, ascending
    auto& nxt = occ.next[id];
    auto& prv = occ.prev[id];

    // next[id][j] = first list element >= j.
    size_t li = 0;
    for (int64_t j = 0; j <= occ.n; j++) {
      while (li < list.size() && (int64_t)list[li] < j)
        li++;
      nxt[(size_t)j] = li < list.size() ? (int64_t)list[li] : occ.n;
    }

    // prev[id][j] = last list element <= j.
    li = 0;
    int64_t last = -1;
    for (int64_t j = 0; j < occ.n; j++) {
      while (li < list.size() && (int64_t)list[li] <= j) {
        last = (int64_t)list[li];
        li++;
      }
      prv[(size_t)j] = last;
    }
  }

  return occ;
}

void LCSRick::Hirschberg(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1, vector<Match>& out) {
  if (i0 >= i1 || j0 >= j1)
    return;
  auto result = FindMidpoint(occ, i0, i1, j0, j1);
  if (!result.hasMidpoint)
    return;
  auto i = (size_t)result.midpoint.index1;
  auto j = (size_t)result.midpoint.index2;
  Hirschberg(occ, i0, i, j0, j, out);
  out.push_back(result.midpoint);
  Hirschberg(occ, i + 1, i1, j + 1, j1, out);
}

LCSRick::Result LCSRick::FindMidpoint(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  Result result;
  if (i0 >= i1 || j0 >= j1)
    return result;

  // Only the two most recent contours of each direction are retained, so the
  // working set stays linear in the sub-problem size.
  Contour fc, fcPrev, bc, bcPrev;
  int64_t fCount = 0;                   // # forward contours computed
  int64_t bCount = 0;                   // # backward contours computed

  while (true) {
    // Alternate: FC[1], BC[1], FC[2], BC[2], ... so |fCount - bCount| <= 1.
    if (fCount <= bCount) {
      fCount++;
      fcPrev = move(fc);
      if (fCount == 1) {
        fc = FirstForward(occ, i0, i1, j0, j1);
      }
      else {
        // Lemma 5: only extend from forward matches still "uncovered" by the
        // backward contours (those with a BC match strictly below-right).
        auto sources = CutForward(fcPrev, bc);
        // Skip rows that cannot hold a match of backward rank >= bCount:
        // such a match needs bCount - 1 further rows below it.
        auto rowLimit = (int64_t)i1 - bCount + 1;
        fc = NextForward(sources.empty() ? fcPrev : sources,
          occ, i0, i1, j0, j1, rowLimit);
      }
      if (fc.empty())
        return result;                  // No matches: sub-LCS length is zero.
    }
    else {
      bCount++;
      bcPrev = move(bc);
      if (bCount == 1) {
        bc = FirstBackward(occ, i0, i1, j0, j1);
      }
      else {
        // Symmetric cut for the backward direction.
        auto sources = CutBackward(bcPrev, fc);
        // Skip rows that cannot hold a match of forward rank >= fCount:
        // such a match needs bCount - 1 rows above it.
        auto rowFloor = (int64_t)i0 + bCount - 1;
        bc = NextBackward(sources.empty() ? bcPrev : sources,
          occ, i0, i1, j0, j1, rowFloor);
      }
    }

    if (fCount >= 1 && bCount >= 1 && Crossed(fc, bc)) {
      result.hasMidpoint = true;
      result.midpoint = Midpoint(fc, bc);
      return result;
    }
  }
}

//
// Lemma 5 contour cutting.
//
LCSRick::Contour LCSRick::CutForward(const Contour& forward, const Contour& backward) {
  Contour kept;
  // Both contours are sorted by ascending index1 (hence descending index2).
  // For each forward match x, the first backward match with index1 > x.index1
  // also has the largest index2 of any backward match below-right of x, so it
  // alone decides whether x is covered.  A single merged scan runs in linear
  // time O(|forward| + |backward|) instead of the quadratic product.
  size_t j = 0;                         // first backward match with index1 > x.index1
  for (const auto& x : forward) {
    while (j < backward.size() && backward[j].index1 <= x.index1)
      j++;
    if (j < backward.size() && backward[j].index2 > x.index2)
      kept.push_back(x);
  }
  return kept;
}

LCSRick::Contour LCSRick::CutBackward(const Contour& backward, const Contour& forward) {
  Contour kept;
  // Symmetric to CutForward: for each backward match y, the last forward match
  // with index1 < y.index1 has the smallest index2 of any forward match
  // above-left of y, so it alone decides whether y is covered.
  size_t k = 0;                         // first forward match with index1 >= y.index1
  for (const auto& y : backward) {
    while (k < forward.size() && forward[k].index1 < y.index1)
      k++;
    if (k > 0 && forward[k - 1].index2 < y.index2)
      kept.push_back(y);
  }
  return kept;
}

//
// FC[1]: dominant rank-1 matches of the sub-problem [i0, i1) x [j0, j1).
//
LCSRick::Contour LCSRick::FirstForward(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour contour;
  int64_t minIndex2 = INT64_MAX;        // running minimum: the dominance test
  for (auto i = i0; i < i1; i++) {
    auto id = occ.rowId[i];
    // Leftmost occurrence in [j0, j1).
    auto index2 = occ.next[(size_t)id][j0];
    if (index2 >= (int64_t)j1)
      continue;
    if (index2 < minIndex2) {
      contour.push_back({ .index1 = (int64_t)i, .index2 = index2 });
      minIndex2 = index2;
    }
  }
  return contour;
}

//
// FC[k + 1]: dominant matches strictly bottom-right of some FC[k] match.
//
LCSRick::Contour LCSRick::NextForward(const Contour& contour,
  const Occurrences& occ, size_t i0, size_t i1, [[maybe_unused]] size_t j0,
  size_t j1, int64_t rowLimit) {
  Contour next;
  int64_t minIndex2 = INT64_MAX;        // running minimum: the dominance test
  size_t matchesAbove = 0;              // # contour matches above the row
  auto start = contour.empty() ? (int64_t)i0 : contour[0].index1 + 1;
  auto end = min((int64_t)i1, rowLimit);
  for (auto i = start; i < end; i++) {
    while (matchesAbove < contour.size() && contour[matchesAbove].index1 < i)
      matchesAbove++;
    if (matchesAbove == 0)
      continue;                         // No contour match above this row.
    auto bound = contour[matchesAbove - 1].index2;
    auto id = occ.rowId[(size_t)i];
    // First occurrence strictly greater than bound, still within [j0, j1).
    auto index2 = occ.next[(size_t)id][(size_t)(bound + 1)];
    if (index2 >= (int64_t)j1)
      continue;
    if (index2 < minIndex2) {
      next.push_back({ .index1 = i, .index2 = index2 });
      minIndex2 = index2;
    }
  }
  return next;
}

//
// BC[1]: dominant (maximal) rank-1 matches, the forward computation applied
// from the back of the strings.
//
LCSRick::Contour LCSRick::FirstBackward(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour contour;
  int64_t maxIndex2 = -1;               // running maximum: the dominance test
  for (auto i = (int64_t)i1 - 1; i >= (int64_t)i0; i--) {
    auto id = occ.rowId[(size_t)i];
    // Rightmost occurrence in [j0, j1): last position strictly less than j1.
    auto index2 = occ.prev[(size_t)id][(size_t)(j1 - 1)];
    if (index2 < (int64_t)j0)
      continue;
    if (index2 > maxIndex2) {
      contour.push_back({ .index1 = i, .index2 = index2 });
      maxIndex2 = index2;
    }
  }
  reverse(contour.begin(), contour.end());  // Restore ascending index1.
  return contour;
}

//
// BC[k + 1]: dominant matches strictly top-left of some BC[k] match.
//
LCSRick::Contour LCSRick::NextBackward(const Contour& contour,
  const Occurrences& occ, [[maybe_unused]] size_t i0, size_t i1,
  size_t j0, [[maybe_unused]] size_t j1, int64_t rowFloor) {
  Contour next;
  int64_t maxIndex2 = -1;               // running maximum: the dominance test
  size_t matchesBelow = contour.size(); // # contour matches below the row
  for (auto i = (int64_t)i1 - 1; i >= rowFloor; i--) {
    while (matchesBelow > 0 && contour[matchesBelow - 1].index1 > i)
      matchesBelow--;
    if (matchesBelow == contour.size())
      continue;                         // No contour match below this row.
    auto bound = contour[matchesBelow].index2;
    if (bound <= (int64_t)j0)
      continue;                         // No occurrence can precede j0.
    auto id = occ.rowId[(size_t)i];
    // Last occurrence strictly less than bound, still within [j0, j1).
    auto index2 = occ.prev[(size_t)id][(size_t)(bound - 1)];
    if (index2 < (int64_t)j0)
      continue;
    if (index2 > maxIndex2) {
      next.push_back({ .index1 = i, .index2 = index2 });
      maxIndex2 = index2;
    }
  }
  reverse(next.begin(), next.end());    // Restore ascending index1.
  return next;
}

//
// Two contours have crossed when no backward match is strictly bottom-right
// of any forward match (Rick, Lemma 3).
//
bool LCSRick::Crossed(const Contour& forward, const Contour& backward) {
  // The crossing test is the same merged scan as CutForward, but it can stop
  // at the first forward match that still has a backward match below-right.
  size_t j = 0;
  for (const auto& x : forward) {
    while (j < backward.size() && backward[j].index1 <= x.index1)
      j++;
    if (j < backward.size() && backward[j].index2 > x.index2)
      return false;                     // Still crossing.
  }
  return true;
}

//
// After the contours have crossed, a midpoint is any forward match x for
// which some backward match y lies non-strictly bottom-right of x.
//
LCSRick::Match LCSRick::Midpoint(const Contour& forward, const Contour& backward) {
  // Merged scan: for each forward match x, the first backward match with
  // index1 >= x.index1 has the largest index2 of any backward match
  // non-strictly below-right of x; it decides whether x is a midpoint.
  size_t j = 0;
  for (const auto& x : forward) {
    while (j < backward.size() && backward[j].index1 < x.index1)
      j++;
    if (j < backward.size() && backward[j].index2 >= x.index2)
      return x;
  }
  return { .index1 = -1, .index2 = -1 };
}
