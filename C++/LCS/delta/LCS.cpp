// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after prefixEnd.push_back()
// 2017-06-29 CNHume  Added Command class
// 2015-01-19 CNHume  Refactored Show() and Delta::Complement()
// 2015-01-17 CNHume  Added reclamation via shared_ptr<>
// 2015-01-16 CNHume  Avoid construction of superfluous Pairs
// 2015-01-15 CNHume  Settled on LCS as the name of the class
// 2015-01-06 CNHume  Added prefix and suffix support to Complement()
// 2015-01-02 CNHume  Implemented the Normal() method
// 2014-12-19 CNHume  Created file
//
// Purpose:
//
// This program implements a solution to the Longest Common Subsequence
// (LCS) Problem based on the Hunt and Szymanski algorithm.  Please see
// the overview provided in "Doc/LCS Overview.md"
//
#include "LCS.h"
#include <algorithm>                    // for lower_bound()
#include <iterator>                     // for next() and prev()

uint32_t LCS::FindLCS(
  MATCHES& indexesOf2MatchedByIndex1, shared_ptr<Pair>* pairs) {
  auto traceLCS = pairs != nullptr;
  PAIRS chains;
  INDEXES prefixEnd;

  //
  //[Assert]After each index1 iteration prefixEnd[index3] is the least index2
  // such that the LCS of s1[0:index1] and s2[0:index2] has length index3 + 1
  //
  uint32_t index1 = 0;
  for (const auto& it1 : indexesOf2MatchedByIndex1) {
    auto dq2 = *it1;
#ifdef SHOW_MATCHES
    cout << "L1[" << index1 << "] = " << join(dq2, " ") << endl;
#endif
#ifdef SHOW_PREFIXENDS
    auto updated = false;
#endif
    auto limit = prefixEnd.end();
    for (auto it2 = dq2.rbegin(); it2 != dq2.rend(); it2++) {
      // Each index1, index2 pair corresponds to a match
      auto index2 = *it2;

      //
      // Note: The reverse iterator it2 visits index2 values in descending order,
      // allowing in-place update of prefixEnd[].  std::lower_bound() is used to
      // perform a binary search.
      //
      limit = lower_bound(prefixEnd.begin(), limit, index2);
#ifdef FILTER_PAIRS
      //
      // Look ahead to the next index2 value to optimize Pairs used by the Hunt
      // and Szymanski algorithm.  If the next index2 is also an improvement on
      // the value currently held in prefixEnd[index3], a new Pair will only be
      // superseded on the next index2 iteration.
      //
      // Verify that a next index2 value exists; and that this value is greater
      // than the final index2 value of the LCS prefix at prev(limit):
      //
      auto preferNextIndex2 = next(it2) != dq2.rend() &&
        (limit == prefixEnd.begin() || *prev(limit) < *next(it2));

      //
      // Depending on match redundancy, this optimization may reduce the number
      // of Pair allocations by factors ranging from 2 up to 10 or more.
      //
      if (preferNextIndex2) continue;
#endif
      auto index3 = distance(prefixEnd.begin(), limit);
#ifdef SHOW_PREFIXENDS
      auto len = index3 + 1;
#endif
      if (limit == prefixEnd.end()) {
        // Insert Case
#ifdef SHOW_PREFIXENDS
        updated = true;
        cout << "inserting " << index2 << " at " << index1
          << " for length = " << len << endl;
#endif
        prefixEnd.push_back(index2);
        // Refresh limit iterator:
        limit = prev(prefixEnd.end());
        if (traceLCS) {
          chains.push_back(pushPair(chains, index3, index1, index2));
        }
      }
      else if (index2 < *limit) {
        // Update Case
#ifdef SHOW_PREFIXENDS
        updated = true;
        cout << "replacing " << *limit << " with " << index2 << " at " << index1
          << " for length = " << len << endl;
#endif
        // Update limit value:
        * limit = index2;
        if (traceLCS) {
          chains[index3] = pushPair(chains, index3, index1, index2);
        }
      }
    }                                   // next index2
#ifdef SHOW_PREFIXENDS
    if (updated) {
      uint32_t index = 0;
      for (const auto& it3 : prefixEnd)
        cout << "end[" << index++ << "] = " << it3 << endl;
    }
#endif
    index1++;
  }                                     // next index1

  if (traceLCS) {
    // Return the LCS as a linked list of matched index pairs:
    auto last = chains.size() > 0 ? chains.back() : nullptr;
    // Reverse longest chain
    *pairs = Pair::Reverse(last);
#ifdef SHOW_PAIRS
    Pair::List(*pairs);
#endif
  }

  auto length = prefixEnd.size();
#ifdef SHOW_COUNTS
  cout << "# Pairs = " << Pair::Pairs
    << "; LCS length = " << length << endl;
#endif
  return length;
}

shared_ptr<Pair> LCS::pushPair(
  PAIRS& chains, const ptrdiff_t& index3, uint32_t& index1, uint32_t& index2) {
  auto prefix = index3 > 0 ? chains[index3 - 1] : nullptr;
  return make_shared<Pair>(index1, index2, prefix);
}
