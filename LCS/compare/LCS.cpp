// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after threshold.push_back()
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

uint32_t LCS::FindLCS(MATCHES& indexesOf2MatchedByIndex1, shared_ptr<Pair>* pairs) {
  auto traceLCS = pairs != nullptr;
  PAIRS chains;
  THRESHOLD threshold;

  //
  //[Assert]After each index1 iteration threshold[index3] is the least index2
  // such that the LCS of s1[0:index1] and s2[0:index2] has length index3 + 1
  //
  uint32_t index1 = 0;
  for (const auto& it1 : indexesOf2MatchedByIndex1) {
    if (!it1->empty()) {
      auto dq2 = *it1;
#ifdef SHOW_MATCHES
      cout << "L1[" << index1 << "] = " << join(dq2, " ") << endl;
#endif
#ifdef SHOW_THRESHOLDS
      auto updated = false;
#endif
      auto limit = threshold.end();
      for (auto it2 = dq2.rbegin(); it2 != dq2.rend(); it2++) {
        // Each of the index1, index2 pairs considered here correspond to a match
        auto index2 = *it2;

        //
        // Note: The reverse iterator it2 visits index2 values in descending order,
        // allowing thresholds to be updated in-place.  std::lower_bound() is used
        // to perform a binary search.
        //
        limit = lower_bound(threshold.begin(), limit, index2);
        auto index3 = distance(threshold.begin(), limit);
#ifdef SHOW_THRESHOLDS
        auto len = index3 + 1;
#endif
#ifdef FILTER_PAIRS
        //
        // Look ahead to the next index2 value to optimize space used in the Hunt
        // and Szymanski algorithm.  If the next index2 is also an improvement on
        // the value currently held in threshold[index3], a new Pair will only be
        // superseded on the next index2 iteration.
        //
        // Depending on match redundancy, the number of Pair constructions may be
        // divided by factors ranging from 2 up to 10 or more.
        //
        auto preferNextIndex2 = next(it2) != dq2.rend() &&
          (limit == threshold.begin() || *prev(limit) < *next(it2));
        if (preferNextIndex2) continue;
#endif
        if (limit == threshold.end()) {
          // Insert Case
#ifdef SHOW_THRESHOLDS
          updated = true;
          cout << "inserting " << index2 << " at " << index1
            << " for length = " << len << endl;
#endif
          threshold.push_back(index2);
          // Refresh limit iterator:
          limit = prev(threshold.end());
          if (traceLCS) {
            auto prefix = index3 > 0 ? chains[index3 - 1] : nullptr;
            auto last = make_shared<Pair>(index1, index2, prefix);
            chains.push_back(last);
          }
        }
        else if (index2 < *limit) {
          // Update Case
#ifdef SHOW_THRESHOLDS
          updated = true;
          cout << "replacing " << *limit << " with " << index2 << " at " << index1
            << " for length = " << len << endl;
#endif
          // Update limit value:
          * limit = index2;
          if (traceLCS) {
            auto prefix = index3 > 0 ? chains[index3 - 1] : nullptr;
            auto last = make_shared<Pair>(index1, index2, prefix);
            chains[index3] = last;
          }
        }
      }                                 // next index2
#ifdef SHOW_THRESHOLDS
      if (updated) {
        uint32_t index = 0;
        for (const auto& it3 : threshold)
          cout << "th[" << index++ << "] = " << it3 << endl;
      }
#endif
    }

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

  auto length = threshold.size();
#ifdef SHOW_COUNTS
  cout << "# Pairs = " << Pair::Pairs
    << "; LCS length = " << length << endl;
#endif
  return length;
}
