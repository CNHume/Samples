// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after threshold.push_back()
// 2017-06-30 CNHume  Added Command class
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

uint32_t LCS::FindLCS(MATCHES& indexes2MatchedByIndex1, shared_ptr<Pair>* pairs) {
  auto traceLCS = pairs != nullptr;
  PAIRS chains;
  THRESHOLD threshold;

  //
  //[Assert]After each index1 iteration threshold[index3] is the least index2
  // such that the LCS of r1[0:index1] and r2[0:index2] has length index3 + 1
  //
  uint32_t index1 = 0;
  for (const auto& it1 : indexes2MatchedByIndex1) {
    if (!it1->empty()) {
      auto dq2 = *it1;
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

        //
        // Look ahead to the next index2 value to optimize space used in the Hunt
        // and Szymanski algorithm.  If the next index2 is also an improvement on
        // the value currently held in threshold[index3], a new Pair will only be
        // superseded on the next index2 iteration.
        //
        // Depending on match redundancy, the number of Pair constructions may be
        // divided by factors ranging from 2 up to 10 or more.
        //
        auto skipIndex2 = next(it2) != dq2.rend() &&
          (limit == threshold.begin() || *prev(limit) < *next(it2));
        if (skipIndex2) continue;

        if (limit == threshold.end()) {
          // Insert Case
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
          // Update limit value:
          *limit = index2;
          if (traceLCS) {
            auto prefix = index3 > 0 ? chains[index3 - 1] : nullptr;
            auto last = make_shared<Pair>(index1, index2, prefix);
            chains[index3] = last;
          }
        }
      }                                 // next index2
    }

    index1++;
  }                                     // next index1

  if (traceLCS) {
    // Return the LCS as a linked list of matched index pairs:
    auto last = chains.size() > 0 ? chains.back() : nullptr;
    // Reverse longest chain
    *pairs = Pair::Reverse(last);
  }

  auto length = threshold.size();
#ifdef SHOW_COUNTS
  cout << "# Pairs = " << Pair::Pairs
    << "; LCS length = " << length << endl;
#endif
  return length;
}
