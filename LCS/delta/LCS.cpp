// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after threshold.push_back()
// 2017-06-30 CNHume  Added Command class
// 2014-12-19 CNHume  Created file
//
// The Longest Common Subsequence (LCS) Problem
//
// Define a subsequence to be any string obtained by deleting zero or more symbols from an input string.
//
// The Longest Common Subsequence or LCS is a subsequence of maximum length common to two or more strings.
//
// Let A = A[0]… A[m - 1] and B = B[0]… B[n - 1], m <= n be strings drawn from an alphabet Σ of size s,
// containing every distinct symbol in A + B.
//
// An ordered pair (i, j) will be called a match if A[i] == B[j], where 0 <= i < m and 0 <= j < n.
//
// Define the strict Cartesian product-order (<) over matches, such that (i1, j1) < (i2, j2) iff i1 < j1 and i2 < j2.
// Defining (>) similarly, we can write m2 < m1 as m1 > m2.
//
// If i1 <= j1 and i2 >= j2 (or i1 >= i2 and i2 <= j2) then neither m1 < m2 nor m1 > m2 are possible; and m1, m2 are incomparable.
// Defining (#) to denote this case, we write m1 # m2.  Note: This includes the possibility that m1 == m2, i.e., that i1 == i2 and j1 == j2.
//
// We write m1 <> m2 to mean that either m1 < m2 or m1 > m2 holds.  Thus, the (<>) operator is the inverse of (#).
// In this case, m1 != m2, i.e., each of tuples differ in some component.
//
// Given a product-order over the set of matches M, a chain C is any subset of M where m1 <> m2 for every pair of distinct elements m1 and m2 of C.
//
// Finding an LCS can then be restated as the problem of finding a chain of maximum cardinality p over the set of matches M.
//
// The set of matches M can be visualized as an m*n bit matrix, where each bit M[i, j] is True iff there is a match at the corresponding positions
// of strings A and B.
//
// Then any chain C can be visualized as a strictly increasing curve through those match bits which are set to True.
// 
// Background:
//
// Where the number of symbols appearing in matches is small relative to the length of the input strings, reuse of the symbols increases;
// and the number of matches will tend towards quadratic, O(m*n) growth. This occurs, for example, in the Bioinformatics application of nucleotide and protein sequencing.
//
// The "divide and conquer" approach by Hirschberg limits the space required to O(m + n).  However, this approach requires O(m*n) time even in the best case.
//
// This quadratic time dependency may become prohibitive, given very long input strings.  Thus, heuristics are often favored over optimal Dynamic Programming solutions.
//
// In the application of comparing file revisions, records from the input files form a large symbol space; and the number of symbols approaches the length of the LCS.
// In this case, the number of matches reduces to linear, O(m + n) growth.
//
// In this case, a binary search optimization due to Hunt and Szymanski can be applied to the basic Dynamic Programming approach which results in expected performance of O(n log m).
// In the worst case, performance can degrade to O(m*n log m) time if the number of matches, and the space required to represent them, should grow to O(m*n).
//
// Note:
//
// Claus Rick has described a linear-space algorithm with a time bound of O(n*s + p*min(m, n - p)).
// 
// Legend:
//
// A, B are the input strings labelled such that m <= n
// where m, n are their respective lengths
// p is the length (to be found) of the LCS
// M is the set of match pairs (i, j) such that x[i] == y[j]
// r is the magnitude of M
// s is the magnitude of the alphabet Sigma of distinct symbols in x + y
//
// References:
//
//"A linear space algorithm for computing maximal common subsequences"
// by Daniel S. Hirschberg, published June 1975
// Communications of the ACM [Volume 18, Number 6, pp. 341-343]
//
//"An Algorithm for Differential File Comparison"
// by James W. Hunt and M. Douglas McIlroy, June 1976
// Computing Science Technical Report, Bell Laboratories 41
//
//"A Fast Algorithm for Computing Longest Common Subsequences"
// by James W. Hunt and Thomas G. Szymanski, published May 1977
// Communications of the ACM [Volume 20, Number 5, pp. 350-353]
//
// See http://www.cs.bgu.ac.il/~dpaa111/wiki.files/HuntSzymanski.pdf
//
//"A new flexible algorithm for the longest common subsequence problem"
// by Claus Rick, published 1995, Proceedings, 6th Annual Symposium on
// Combinatorial Pattern Matching [Lecture Notes in Computer Science,
// Springer Verlag, Volume 937, pp. 340-351]
//
//"Simple and fast linear space computation of longest common subsequences"
// by Claus Rick, received 17 March 2000, Information Processing Letters,
// Elsevier Science [Volume 75, pp. 275–281]
//
//"A New Practical Linear Space Algorithm for the Longest Common
// Subsequence Problem" by Heiko Goeman and Michael Clausen,
// published 2002, Kybernetika [Volume 38, Issue 1, pp. 45-66]
//
#include "LCS.h"
#include <algorithm>                    // for lower_bound()
#include <iterator>                     // for next() and prev()

// The following implements the Hunt and Szymanski algorithm:
uint32_t LCS::Pairs(MATCHES& matches, shared_ptr<Pair>* pairs) {
  auto trace = pairs != nullptr;
  PAIRS traces;
  THRESHOLD threshold;

  //
  //[Assert]After each index1 iteration threshold[index3] is the least index2
  // such that the LCS of r1[0:index1] and r2[0:index2] has length index3 + 1
  //
  uint32_t index1 = 0;
  for (const auto& it1 : matches) {
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
        auto skip = next(it2) != dq2.rend() &&
          (limit == threshold.begin() || *prev(limit) < *next(it2));
        if (skip) continue;

        if (limit == threshold.end()) {
          // insert case
          threshold.push_back(index2);
          // Refresh limit iterator:
          limit = prev(threshold.end());
          if (trace) {
            auto prefix = index3 > 0 ? traces[index3 - 1] : nullptr;
            auto last = make_shared<Pair>(index1, index2, prefix);
            traces.push_back(last);
          }
        }
        else if (index2 < *limit) {
          // replacement case
          *limit = index2;
          if (trace) {
            auto prefix = index3 > 0 ? traces[index3 - 1] : nullptr;
            auto last = make_shared<Pair>(index1, index2, prefix);
            traces[index3] = last;
          }
        }
      }                                 // next index2
    }

    index1++;
  }                                     // next index1

  if (trace) {
    // Return the LCS as a linked list of matched index pairs:
    auto last = traces.size() > 0 ? traces.back() : nullptr;
    // Reverse longest back-trace
    *pairs = Pair::Reverse(last);
  }

  auto length = threshold.size();
#ifdef SHOW_COUNTS
  cout << "# Pairs = " << Pair::Pairs
    << "; LCS length = " << length << endl;
#endif
  return length;
}
