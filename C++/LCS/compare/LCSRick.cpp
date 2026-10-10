// Rick (2000) linear-space LCS, adapted to reuse the Hunt-Szymanski
// Match() output.
//
// See LCSRick.h for an overview.
//
#include "LCSRick.h"

#include <algorithm>                    // for lower_bound, upper_bound, min, reverse
#include <climits>                      // for INT64_MAX
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
// s1[index1] relative to a bound.  The sorted match list matches[index1]
// answers these queries via lower_bound()/upper_bound(), at O(log r) cost.
//

uint32_t LCSRick::Find(shared_ptr<Pair>* pairs,
  const MATCHES& matches, uint32_t size1, uint32_t size2) {
  vector<Match> out;
  Hirschberg(matches, 0, size1, 0, size2, out);

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

void LCSRick::Hirschberg(const MATCHES& matches,
  size_t i0, size_t i1, size_t j0, size_t j1, vector<Match>& out) {
  if (i0 >= i1 || j0 >= j1)
    return;
  auto result = FindMidpoint(matches, i0, i1, j0, j1);
  if (!result.hasMidpoint)
    return;
  auto i = (size_t)result.midpoint.index1;
  auto j = (size_t)result.midpoint.index2;
  Hirschberg(matches, i0, i, j0, j, out);
  out.push_back(result.midpoint);
  Hirschberg(matches, i + 1, i1, j + 1, j1, out);
}

LCSRick::Result LCSRick::FindMidpoint(const MATCHES& matches,
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
        fc = FirstForward(matches, i0, i1, j0, j1);
      }
      else {
        // Lemma 5: only extend from forward matches still "uncovered" by the
        // backward contours (those with a BC match strictly below-right).
        auto sources = CutForward(fcPrev, bc);
        // Skip rows that cannot hold a match of backward rank >= bCount:
        // such a match needs bCount - 1 further rows below it.
        auto rowLimit = (int64_t)i1 - bCount + 1;
        fc = NextForward(sources.empty() ? fcPrev : sources,
          matches, i0, i1, j0, j1, rowLimit);
      }
      if (fc.empty())
        return result;                  // No matches: sub-LCS length is zero.
    }
    else {
      bCount++;
      bcPrev = move(bc);
      if (bCount == 1) {
        bc = FirstBackward(matches, i0, i1, j0, j1);
      }
      else {
        // Symmetric cut for the backward direction.
        auto sources = CutBackward(bcPrev, fc);
        // Skip rows that cannot hold a match of forward rank >= fCount:
        // such a match needs bCount - 1 rows above it.
        auto rowFloor = (int64_t)i0 + bCount - 1;
        bc = NextBackward(sources.empty() ? bcPrev : sources,
          matches, i0, i1, j0, j1, rowFloor);
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
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 > x.index1 && y.index2 > x.index2) {
        kept.push_back(x);
        break;
      }
  return kept;
}

LCSRick::Contour LCSRick::CutBackward(const Contour& backward, const Contour& forward) {
  Contour kept;
  for (const auto& y : backward)
    for (const auto& x : forward)
      if (x.index1 < y.index1 && x.index2 < y.index2) {
        kept.push_back(y);
        break;
      }
  return kept;
}

//
// FC[1]: dominant rank-1 matches of the sub-problem [i0, i1) x [j0, j1).
//
LCSRick::Contour LCSRick::FirstForward(const MATCHES& matches,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour contour;
  int64_t minIndex2 = INT64_MAX;        // running minimum: the dominance test
  for (auto i = i0; i < i1; i++) {
    const auto& list = *matches[i];
    // Leftmost occurrence in [j0, j1).
    auto it = lower_bound(list.begin(), list.end(), (uint32_t)j0);
    if (it == list.end() || *it >= j1)
      continue;
    auto index2 = (int64_t)*it;
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
  const MATCHES& matches, size_t i0, size_t i1, [[maybe_unused]] size_t j0,
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
    const auto& list = *matches[i];
    // First occurrence strictly greater than bound, still within [j0, j1).
    auto it = upper_bound(list.begin(), list.end(), (uint32_t)bound);
    if (it == list.end() || *it >= j1)
      continue;
    auto index2 = (int64_t)*it;
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
LCSRick::Contour LCSRick::FirstBackward(const MATCHES& matches,
  size_t i0, size_t i1, size_t j0, size_t j1) {
  Contour contour;
  int64_t maxIndex2 = -1;               // running maximum: the dominance test
  for (auto i = (int64_t)i1 - 1; i >= (int64_t)i0; i--) {
    const auto& list = *matches[(size_t)i];
    // Rightmost occurrence in [j0, j1): last position strictly less than j1.
    auto it = lower_bound(list.begin(), list.end(), (uint32_t)j1);
    if (it == list.begin())
      continue;
    --it;
    if (*it < j0)
      continue;
    auto index2 = (int64_t)*it;
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
  const MATCHES& matches, [[maybe_unused]] size_t i0, size_t i1,
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
    const auto& list = *matches[(size_t)i];
    // Last occurrence strictly less than bound, still within [j0, j1).
    auto it = lower_bound(list.begin(), list.end(), (uint32_t)bound);
    if (it == list.begin())
      continue;
    --it;
    auto index2 = (int64_t)*it;
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
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 > x.index1 && y.index2 > x.index2)
        return false;
  return true;
}

//
// After the contours have crossed, a midpoint is any forward match x for
// which some backward match y lies non-strictly bottom-right of x.
//
LCSRick::Match LCSRick::Midpoint(const Contour& forward, const Contour& backward) {
  for (const auto& x : forward)
    for (const auto& y : backward)
      if (y.index1 >= x.index1 && y.index2 >= x.index2)
        return x;
  return { .index1 = -1, .index2 = -1 };
}
