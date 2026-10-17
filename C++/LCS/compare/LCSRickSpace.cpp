// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2026-10-09 CNHume  Created LCSRickSpace class
//
// Rick (2000) linear-space LCS, adapted to reuse the Hunt-Szymanski
// Match() output.
//
// See LCSRickSpace.h for an overview.
//
#include "LCSRickSpace.h"

#include <algorithm>                    // for min, reverse
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

uint64_t LCSRickSpace::Candidates = 0;
uint64_t LCSRickSpace::Rows = 0;

void LCSRickSpace::ResetCounters() {
  Candidates = 0;
  Rows = 0;
}

uint32_t LCSRickSpace::Find(shared_ptr<Pair>* pairs,
  const MATCHES& matches, uint32_t size1, uint32_t size2) {
  return Solve(pairs, BuildOccurrences(matches, size1, size2), size1, size2);
}

uint32_t LCSRickSpace::Find(shared_ptr<Pair>* pairs,
  const u32string& s1, const u32string& s2) {
  // Build O(ns) occurrence tables directly from the code points and amortize
  // them over the whole Hirschberg recursion.  No match lists are ever
  // materialized, so this path stays feasible for very long strings.
  return Solve(pairs, BuildOccurrences(s1, s2), s1.size(), s2.size());
}

//
// Runs the Hirschberg recursion over an occurrence table and, when pairs is
// requested, reconstructs the linked list of matched positions.
//
uint32_t LCSRickSpace::Solve(shared_ptr<Pair>* pairs, const Occurrences& occ,
  size_t size1, size_t size2) {
  vector<Match> out;
  Hirschberg(occ, 0, size1, 0, size2, out);

  if (pairs != nullptr) {
    shared_ptr<Pair> head = nullptr;
    shared_ptr<Pair> tail = nullptr;
    for (const auto& match : out) {
      auto pair = make_shared<Pair>(match.index1, match.index2);
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
LCSRickSpace::Occurrences LCSRickSpace::BuildOccurrences(
  const MATCHES& matches, size_t size1, size_t size2) {
  Occurrences occ;
  occ.m = (int64_t)size1;
  occ.n = (int64_t)size2;

  // Two rows sharing a symbol share the same match-list pointer, so pointer
  // identity compresses the symbols without needing the symbol values.
  unordered_map<const INDEXES*, uint32_t> pointerToId;
  vector<const INDEXES*> lists;         // lists[id] = sorted positions of symbol id
  occ.rowId.assign(size1, UINT32_MAX);
  for (size_t i = 0; i < size1; i++) {
    const auto* p = matches[i];
    auto it = pointerToId.find(p);
    uint32_t id;
    if (it == pointerToId.end()) {
      id = (uint32_t)lists.size();
      pointerToId.emplace(p, id);
      lists.push_back(p);
    }
    else {
      id = it->second;
    }
    occ.rowId[i] = id;
  }

  auto s = lists.size();
  occ.next.assign(s, vector<uint32_t>((size_t)size2 + 1, (uint32_t)occ.n));
  occ.prev.assign(s, vector<uint32_t>((size_t)size2, UINT32_MAX));

  // colId[j] = symbol id of s2[j], inverting the match lists.
  occ.colId.assign(size2, UINT32_MAX);

  for (size_t id = 0; id < s; id++) {
    const auto& list = *lists[id];      // sorted positions, ascending
    auto& nxt = occ.next[id];
    auto& prv = occ.prev[id];

    // next[id][j] = first list element >= j.
    size_t li = 0;
    for (int64_t j = 0; j <= occ.n; j++) {
      while (li < list.size() && (int64_t)list[li] < j)
        li++;
      nxt[(size_t)j] = li < list.size() ? list[li] : (uint32_t)occ.n;
    }

    // prev[id][j] = last list element <= j.
    li = 0;
    uint32_t last = UINT32_MAX;
    for (int64_t j = 0; j < occ.n; j++) {
      while (li < list.size() && (int64_t)list[li] <= j) {
        last = list[li];
        li++;
      }
      prv[(size_t)j] = last;
    }

    // Invert: each position in the list holds symbol id.
    for (auto j : list)
      occ.colId[(size_t)j] = (uint32_t)id;
  }

  // next1[id][i] = first occurrence of symbol id at/after i in s1 (m if none).
  occ.next1.assign(s, vector<uint32_t>((size_t)occ.m + 1, (uint32_t)occ.m));
  for (int64_t i = occ.m - 1; i >= 0; i--) {
    for (size_t id = 0; id < s; id++)
      occ.next1[id][(size_t)i] = occ.next1[id][(size_t)i + 1];
    auto rid = occ.rowId[(size_t)i];
    if (rid != UINT32_MAX)
      occ.next1[(size_t)rid][(size_t)i] = (uint32_t)i;
  }

  // prev1[id][i] = last occurrence of symbol id at/before i in s1 (UINT32_MAX
  // if none).
  occ.prev1.assign(s, vector<uint32_t>((size_t)occ.m, UINT32_MAX));
  for (int64_t i = 0; i < occ.m; i++) {
    for (size_t id = 0; id < s; id++)
      occ.prev1[id][(size_t)i] = i > 0 ? occ.prev1[id][(size_t)i - 1] : UINT32_MAX;
    auto rid = occ.rowId[(size_t)i];
    if (rid != UINT32_MAX)
      occ.prev1[(size_t)rid][(size_t)i] = (uint32_t)i;
  }

  return occ;
}

//
// Character-path overload: builds the same tables directly from code-point
// strings in O(s*n) time and space, where s is the number of distinct code
// points in s2.  No match lists are materialized, so the preprocessing stays
// linear even when the number of matches r would be quadratic.
//
LCSRickSpace::Occurrences LCSRickSpace::BuildOccurrences(
  const u32string& s1, const u32string& s2) {
  Occurrences occ;
  occ.m = (int64_t)s1.size();
  occ.n = (int64_t)s2.size();

  // Compress the code points of s2 (the tabled string) to dense ids.
  unordered_map<char32_t, uint32_t> charToId;
  vector<uint32_t> s2Id(s2.size(), UINT32_MAX);
  for (size_t j = 0; j < s2.size(); j++) {
    auto c = s2[j];
    auto it = charToId.find(c);
    uint32_t id;
    if (it == charToId.end()) {
      id = (uint32_t)charToId.size();
      charToId.emplace(c, id);
    }
    else {
      id = it->second;
    }
    s2Id[j] = id;
  }

  auto s = charToId.size();
  occ.colId = move(s2Id);

  // rowId[i] = symbol id of s1[i], or UINT32_MAX when it never occurs in s2.
  occ.rowId.assign(s1.size(), UINT32_MAX);
  for (size_t i = 0; i < s1.size(); i++) {
    auto it = charToId.find(s1[i]);
    if (it != charToId.end())
      occ.rowId[i] = it->second;
  }

  // next[id][j] = first occurrence of symbol id at/after j (n if none).
  occ.next.assign(s, vector<uint32_t>((size_t)occ.n + 1, (uint32_t)occ.n));
  for (int64_t j = occ.n - 1; j >= 0; j--) {
    for (size_t id = 0; id < s; id++)
      occ.next[id][(size_t)j] = occ.next[id][(size_t)j + 1];
    occ.next[(size_t)occ.colId[(size_t)j]][(size_t)j] = (uint32_t)j;
  }

  // prev[id][j] = last occurrence of symbol id at/before j (UINT32_MAX if none).
  occ.prev.assign(s, vector<uint32_t>((size_t)occ.n, UINT32_MAX));
  for (int64_t j = 0; j < occ.n; j++) {
    for (size_t id = 0; id < s; id++)
      occ.prev[id][(size_t)j] = j > 0 ? occ.prev[id][(size_t)j - 1] : UINT32_MAX;
    occ.prev[(size_t)occ.colId[(size_t)j]][(size_t)j] = (uint32_t)j;
  }

  // next1[id][i] = first occurrence of symbol id at/after i in s1 (m if none).
  occ.next1.assign(s, vector<uint32_t>((size_t)occ.m + 1, (uint32_t)occ.m));
  for (int64_t i = occ.m - 1; i >= 0; i--) {
    for (size_t id = 0; id < s; id++)
      occ.next1[id][(size_t)i] = occ.next1[id][(size_t)i + 1];
    auto rid = occ.rowId[(size_t)i];
    if (rid != UINT32_MAX)
      occ.next1[(size_t)rid][(size_t)i] = (uint32_t)i;
  }

  // prev1[id][i] = last occurrence of symbol id at/before i in s1 (UINT32_MAX
  // if none).
  occ.prev1.assign(s, vector<uint32_t>((size_t)occ.m, UINT32_MAX));
  for (int64_t i = 0; i < occ.m; i++) {
    for (size_t id = 0; id < s; id++)
      occ.prev1[id][(size_t)i] = i > 0 ? occ.prev1[id][(size_t)i - 1] : UINT32_MAX;
    auto rid = occ.rowId[(size_t)i];
    if (rid != UINT32_MAX)
      occ.prev1[(size_t)rid][(size_t)i] = (uint32_t)i;
  }

  return occ;
}

void LCSRickSpace::Hirschberg(const Occurrences& occ,
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

LCSRickSpace::Result LCSRickSpace::FindMidpoint(const Occurrences& occ,
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
        fc = NextForward(sources.empty() ? fcPrev : sources,
          occ, i0, i1, j0, j1);
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
        bc = NextBackward(sources.empty() ? bcPrev : sources,
          occ, i0, i1, j0, j1);
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
LCSRickSpace::Contour LCSRickSpace::CutForward(const Contour& forward, const Contour& backward) {
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

LCSRickSpace::Contour LCSRickSpace::CutBackward(const Contour& backward, const Contour& forward) {
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
// This is the dual diagonal sweep with no predecessor contour.
//
LCSRickSpace::Contour LCSRickSpace::FirstForward(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  return NextForward(Contour{}, occ, i0, i1, j0, j1);
}

//
// FC[k + 1]: dominant matches strictly bottom-right of some FC[k] match,
// computed by Rick's dual diagonal sweep: a row sweep for the horizontal
// stripe (j - j0 >= i - i0) and a column sweep for the vertical stripe
// (i - i0 > j - j0), interleaved over the diagonal and stopped by the lmin
// completeness test once a dominant match has both components within the
// current diagonal.  This bounds each contour's work at O(min{m, n-p}).
//
LCSRickSpace::Contour LCSRickSpace::NextForward(const Contour& contour,
  const Occurrences& occ, size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour next;
  auto R = i1 - i0;                     // rows
  auto C = j1 - j0;                     // columns
  auto L = min(R, C);

  int64_t rowThresh = (int64_t)j1;      // leftmost column of a dominant match
  int64_t colThresh = (int64_t)i1;      // topmost row of a dominant match
  size_t matchesAbove = 0;              // # sources with index1 < current row
  size_t leftSources = contour.size();  // first source with index2 < current col

  // The sweeps stay idle until a source lies above the row (row sweep) or
  // left of the column (column sweep); begin at the first diagonal that can
  // yield a match instead of walking the whole idle prefix.
  auto lRow = contour.empty() ? (size_t)0 : (size_t)contour[0].index1 - i0 + 1;
  auto lCol = contour.empty() ? (size_t)0 : (size_t)contour.back().index2 - j0 + 1;
  auto lStart = min(min(lRow, lCol), L);
  auto iStart = i0 + lStart;
  auto jStart = j0 + lStart;
  while (matchesAbove < contour.size() && contour[matchesAbove].index1 < (uint32_t)iStart)
    matchesAbove++;
  while (leftSources > 0 && contour[leftSources - 1].index2 < (uint32_t)jStart)
    leftSources--;

  for (size_t l = lStart; l < L; l++) {
    Rows++;                             // one row + one column sweep step
    auto i = i0 + l;                    // row
    auto j = j0 + l;                    // column

    // Row sweep: horizontal matches (i, j') with j' >= j.  A source above row
    // i is needed unless the contour is empty (the rank-1 case).
    while (matchesAbove < contour.size() && contour[matchesAbove].index1 < (uint32_t)i)
      matchesAbove++;
    auto rowId = occ.rowId[(size_t)i];
    if (rowId != UINT32_MAX && (contour.empty() || matchesAbove > 0)) {
      auto lo = j;                      // first occurrence >= j (horizontal)
      if (matchesAbove > 0)
        lo = max(lo, (size_t)contour[matchesAbove - 1].index2 + 1);
      auto index2 = occ.next[(size_t)rowId][lo];
      // Skip if a vertical (column-sweep) match at row <= i dominates it.
      if (index2 < rowThresh && (int64_t)i < colThresh) {
        Candidates++;
        next.push_back({ .index1 = (uint32_t)i, .index2 = index2 });
        rowThresh = index2;
      }
    }

    // Column sweep: vertical matches (i', j) with i' > i.  A source left of
    // column j is needed unless the contour is empty (the rank-1 case).
    while (leftSources > 0 && contour[leftSources - 1].index2 < (uint32_t)j)
      leftSources--;
    auto colId = occ.colId[(size_t)j];
    if (colId != UINT32_MAX && (contour.empty() || leftSources < contour.size())) {
      auto lo = i + 1;                  // first occurrence > i (vertical)
      if (leftSources < contour.size())
        lo = max(lo, (size_t)contour[leftSources].index1 + 1);
      auto index1 = occ.next1[(size_t)colId][lo];
      // Skip if a horizontal (row-sweep) match at column <= j dominates it.
      if (index1 < colThresh && (int64_t)j < rowThresh) {
        Candidates++;
        next.push_back({ .index1 = index1, .index2 = (uint32_t)j });
        colThresh = index1;
      }
    }

    // Completeness: a dominant match with both components within the current
    // diagonal means no later diagonal can hold another dominant match.
    if (rowThresh <= (int64_t)j || colThresh <= (int64_t)i)
      break;
  }

  sort(next.begin(), next.end(),
    [](const Match& a, const Match& b) { return a.index1 < b.index1; });
  return next;
}

//
// BC[1]: dominant (maximal) rank-1 matches, the backward dual sweep applied
// with no predecessor contour.
//
LCSRickSpace::Contour LCSRickSpace::FirstBackward(const Occurrences& occ,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  return NextBackward(Contour{}, occ, i0, i1, j0, j1);
}

//
// BC[k + 1]: dominant matches strictly top-left of some BC[k] match.  This is
// the backward dual sweep, the mirror image of NextForward: a row sweep for
// the backward horizontal stripe (j - j0 <= i - i0) and a column sweep for
// the backward vertical stripe, interleaved over the anti-diagonal and
// stopped by the symmetric completeness test.
//
LCSRickSpace::Contour LCSRickSpace::NextBackward(const Contour& contour,
  const Occurrences& occ, size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour next;
  auto R = i1 - i0;                     // rows
  auto C = j1 - j0;                     // columns
  auto L = min(R, C);

  int64_t rowThresh = (int64_t)j0 - 1;  // rightmost column of a dominant match
  int64_t colThresh = (int64_t)i0 - 1;  // bottommost row of a dominant match
  size_t matchesBelow = contour.size(); // first source with index1 > current row
  size_t rightSources = 0;              // # sources with index2 > current col

  // Mirror of the forward idle-prefix skip: begin at the first diagonal that
  // can yield a match in either sweep.
  auto lRow = contour.empty() ? (size_t)0 : i1 - (size_t)contour.back().index1;
  auto lCol = contour.empty() ? (size_t)0 : j1 - (size_t)contour[0].index2;
  auto lStart = min(min(lRow, lCol), L);
  auto iStart = i1 - 1 - lStart;
  auto jStart = j1 - 1 - lStart;
  while (matchesBelow > 0 && contour[matchesBelow - 1].index1 > (uint32_t)iStart)
    matchesBelow--;
  while (rightSources < contour.size() && contour[rightSources].index2 > (uint32_t)jStart)
    rightSources++;

  for (size_t l = lStart; l < L; l++) {
    Rows++;                             // one row + one column sweep step
    auto i = i1 - 1 - l;                // row
    auto j = j1 - 1 - l;                // column

    // Row sweep: backward horizontal matches (i, j') with j' <= j.  A source
    // below row i is needed unless the contour is empty (the rank-1 case).
    while (matchesBelow > 0 && contour[matchesBelow - 1].index1 > (uint32_t)i)
      matchesBelow--;
    auto rowId = occ.rowId[(size_t)i];
    if (rowId != UINT32_MAX && (contour.empty() || matchesBelow < contour.size())) {
      bool active = true;
      auto hi = j;                      // k=1: rightmost occurrence <= j
      if (matchesBelow < contour.size()) {
        auto bound = contour[matchesBelow].index2;
        active = bound > j0;            // an occurrence must precede j0
        if (active)
          hi = min((size_t)(bound - 1), j);
      }
      if (active) {
        auto index2 = occ.prev[(size_t)rowId][hi];
        // Skip if a vertical (column-sweep) match at row >= i dominates it.
        if (index2 != UINT32_MAX && index2 > rowThresh && (int64_t)i > colThresh) {
          Candidates++;
          next.push_back({ .index1 = (uint32_t)i, .index2 = index2 });
          rowThresh = index2;
        }
      }
    }

    // Column sweep: backward vertical matches (i', j) with i' < i.  A source
    // right of column j is needed unless the contour is empty (rank-1 case).
    while (rightSources < contour.size() && contour[rightSources].index2 > (uint32_t)j)
      rightSources++;
    auto colId = occ.colId[(size_t)j];
    if (colId != UINT32_MAX && (contour.empty() || rightSources > 0) && i > i0) {
      bool active = true;
      auto hi = (int64_t)i - 1;         // k=1: bottommost occurrence < i
      if (rightSources > 0) {
        auto bound = contour[rightSources - 1].index1;
        active = bound > i0;
        if (active)
          hi = min((int64_t)bound - 1, (int64_t)i - 1);
      }
      if (active && hi >= (int64_t)i0) {
        auto index1 = occ.prev1[(size_t)colId][(size_t)hi];
        // Skip if a horizontal (row-sweep) match at column >= j dominates it.
        if (index1 != UINT32_MAX && index1 > colThresh && (int64_t)j > rowThresh) {
          Candidates++;
          next.push_back({ .index1 = index1, .index2 = (uint32_t)j });
          colThresh = index1;
        }
      }
    }

    // Completeness (mirror image).
    if (rowThresh >= (int64_t)j || colThresh >= (int64_t)i)
      break;
  }

  sort(next.begin(), next.end(),
    [](const Match& a, const Match& b) { return a.index1 < b.index1; });
  return next;
}

//
// Two contours have crossed when no backward match is strictly bottom-right
// of any forward match (Rick, Lemma 3).
//
bool LCSRickSpace::Crossed(const Contour& forward, const Contour& backward) {
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
LCSRickSpace::Match LCSRickSpace::Midpoint(const Contour& forward, const Contour& backward) {
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
  return { .index1 = UINT32_MAX, .index2 = UINT32_MAX };
}
