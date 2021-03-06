// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after threshold.push_back()
// 2017-06-30 CNHume  Reduced Select() length precision
// 2015-01-25 CNHume  Allowed Pairs() to return the LCS Length
// 2015-01-14 CNHume  Created file to demonstrate a fast algorithm by Hunt and
//                    Szymanski for computing Longest Common Subsequences (LCS)
//
#include <stdint.h>
#include <string>
#include <memory>                       // for shared_ptr<>
#include <iostream>
#include <deque>
#include <map>
#include <algorithm>                    // for lower_bound()
#include <iterator>                     // for next() and prev()

using namespace std;

class LCS {
protected:
  // This linked list class is used to trace the LCS candidates
  class Pair {
  public:
    uint32_t index1;
    uint32_t index2;
    shared_ptr<Pair> next;

    Pair(uint32_t index1, uint32_t index2, shared_ptr<Pair> next = nullptr)
      : index1(index1), index2(index2), next(next) {
    }

    static shared_ptr<Pair> Reverse(const shared_ptr<Pair> pairs) {
      shared_ptr<Pair> head = nullptr;
      for (auto next = pairs; next != nullptr; next = next->next)
        head = make_shared<Pair>(next->index1, next->index2, head);
      return head;
    }
  };

  //
  // The Longest Common Subsequence (LCS) Problem:
  //
  // Defining a subsequence to be a string obtained by deleting
  // zero or more symbols from an input string, the LCS Problem is
  // to find a subsequence of maximum length that is common to two
  // input strings.
  //
  // Let x = x[0]... x[m-1] and y = y[0]... y[n-1], m <= n be the
  // two strings both drawn from an alphabet of size s.
  //
  // An ordered pair (i, j) will be called a match if x[i] = y[j],
  // where 0 <= i < m and 0 <= j < n.
  //
  // Define the partial order << over ordered pairs, such that
  // (i1, j1) << (i2, j2) iff i1 < j1 and i2 < j2.
  //
  // Given a partial order << over a set M, a chain C is any subset
  // of M where either p1 << p2 or p2 << p1, for distinct p1 and p2
  // in C.
  //
  // Finding the LCS can then be viewed as the problem of finding
  // a chain of maximum cardinality within the set of matches M.
  //
  // The set of matches between input strings x and y can be
  // visualized as an m*n bit matrix, where a bit is set iff
  // there is a corresponding match.  A chain can be visualized
  // as a graph through match bits that increases monotonically.
  //
  // Background:
  //
  // Where the number of symbols appearing in matches is small
  // relative to the length of the input strings, reuse of the
  // symbols necessarily increases; and the number of matches
  // will tend towards quadratic, O(m*n) growth.
  //
  // This occurs, for example, in Bioinformatics applications
  // of nucleotide and protein sequencing.
  //
  // Here the "divide and conquer" approach of Hirschberg
  // limits the space required to O(m+n).  However, this
  // approach requires O(m*n) time even in the best case.
  //
  // This quadratic time dependency may become prohibitive, given
  // very long input strings.  Thus, heuristics are often favored
  // over optimal Dynamic Programming solutions.
  //
  // In the application of comparing file revisions, records from
  // the input files form a large symbol space; and the number of
  // symbols used for matches may approach the length of the LCS.
  //
  // Assuming a uniform distribution of symbols, the number
  // of matches may tend only towards linear, O(m+n) growth.
  //
  // A binary search optimization due to Hunt and Szymanski
  // can be applied in this case, which results in expected
  // performance of O(n log m), given m <= n.  In the worst
  // case, performance degrades to O(m*n log m) time if the
  // number of matches, and the space required to represent
  // them, should grow to O(m*n).
  //
  // More recent improvements by Rick and by Goeman and Clausen
  // reduce the time bound to O(n*s + min(p*m, p*(n-p))), where
  // the alphabet is of size s and the LCS is of length p.
  //
  // Legend:
  //
  // x, y are the input strings labelled such that m <= n where m, n are their respective lengths
  // p is the length (to be found) of the LCS
  // M is the set of match pairs (i, j) such that x[i] = y[j]
  // r is the magnitude of M
  // s is the magnitude of the alphabet Sigma of distinct symbols in x, y
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
  //"A New Practical Linear Space Algorithm for the Longest Common
  // Subsequence Problem" by Heiko Goeman and Michael Clausen,
  // published 2002, Kybernetika [volume 38, Issue 1, pp. 45-66]
  //
  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> THRESHOLD;
  typedef deque<uint32_t> INDEXES;
  typedef map<char, INDEXES> CHAR2INDEXES;
  typedef deque<INDEXES*> MATCHES;

  // return the LCS as a linked list of matched index pairs
  uint32_t Pairs(MATCHES& matches, shared_ptr<Pair> *pairs) {
    auto trace = pairs != nullptr;
    PAIRS traces;
    THRESHOLD threshold;

    //
    //[Assert]After each index1 iteration threshold[index3] is the least index2
    // such that the LCS of s1[0:index1] and s2[0:index2] has length index3 + 1
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
      auto last = traces.size() > 0 ? traces.back() : nullptr;
      // Reverse longest back-trace
      *pairs = Pair::Reverse(last);
    }

    auto length = threshold.size();
    return length;
  }

  //
  // Match() avoids incurring m*n comparisons by using the associative
  // memory implemented by CHAR2INDEXES to achieve O(m+n) performance,
  // where m and n are the input lengths.
  //
  // The lookup time can be assumed constant in the case of characters.
  // The symbol space is larger in the case of records; but the lookup
  // time will be O(log(m+n)), at most.
  //
  void Match(CHAR2INDEXES& indexes, MATCHES& matches,
    const string& s1, const string& s2) {
    uint32_t index = 0;
    for (const auto& it : s2)
      indexes[it].push_back(index++);

    for (const auto& it : s1) {
      auto& dq2 = indexes[it];
      matches.push_back(&dq2);
    }
  }

  string Select(shared_ptr<Pair> pairs, uint32_t length,
    bool right, const string& s1, const string& s2) {
    string buffer;
    buffer.reserve(length);
    for (auto next = pairs; next != nullptr; next = next->next) {
      auto c = right ? s2[next->index2] : s1[next->index1];
      buffer.push_back(c);
    }
    return buffer;
  }

public:
  string Correspondence(const string& s1, const string& s2) {
    CHAR2INDEXES indexes;
    MATCHES matches;                    // holds references into indexes
    Match(indexes, matches, s1, s2);
    shared_ptr<Pair> pairs;             // obtain the LCS as index pairs
    auto length = Pairs(matches, &pairs);
    return Select(pairs, length, false, s1, s2);
  }
};

//
// command line processor
//
void parse(int argc, char* argv[], string& s1, string& s2) {
  auto usage = false;
  auto n = 1;                           // skip verb

  if (n < argc)
    s1 = argv[n++];
  else                                  // s1 is required
    usage = true;

  if (n < argc)
    s2 = argv[n++];
  else                                  // s2 is required
    usage = true;

  if (n < argc)                         // superfluous argument specified
    usage = true;

  if (usage)                            // throw usage line if parse failed
    throw runtime_error("Usage: LCS s1 s2");
}

//
// command line application
//
int main(int argc, char* argv[]) {
  const int32_t displayDefault = 0;
  auto errorLevel = EXIT_FAILURE;

  try {
    //
    // command line parameters to be parsed:
    //
    string s1, s2;
    parse(argc, argv, s1, s2);

    LCS lcs;
    auto s = lcs.Correspondence(s1, s2);
    cout << s << endl;

    errorLevel = EXIT_SUCCESS;
  }
  catch (exception& ex) {
    cout << ex.what() << endl;
  }
  catch (...) {
    cout << "exception" << endl;
    throw;
  }
#ifdef _DEBUG
  cout << "Press Enter";
  char c;
  cin.getline(&c, 1);
#endif
  return errorLevel;
}
