// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-07-05 CNHume  Applied prev() and next() in skip criteria.
// 2017-07-03 CNHume  Refresh limit iterator after threshold.push_back()
// 2017-06-30 CNHume  Reduced Select() length precision
// 2015-01-25 CNHume  Allowed FindLCS() to return the LCS Length
// 2015-01-14 CNHume  Created file to demonstrate a fast algorithm by Hunt and
//                    Szymanski for computing Longest Common Subsequences (LCS)
//
// Purpose:
//
// This program implements a solution to the Longest Common Subsequence
// (LCS) Problem based on the Hunt and Szymanski algorithm.  Please see
// the overview provided in "Doc/LCS Overview.md"
//
#include <stdint.h>
#include <string>
#include <memory>                       // for shared_ptr<>
#include <iostream>
#include <deque>
#include <unordered_map>                //[C++11]
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

  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> THRESHOLD;
  typedef deque<uint32_t> INDEXES;
  typedef unordered_map<char, INDEXES> CHAR_TO_INDEXES_MAP;
  typedef deque<INDEXES*> MATCHES;

  uint32_t FindLCS(MATCHES& indexesOf2MatchedByIndex1, shared_ptr<Pair>* pairs) {
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
          auto preferNextIndex2 = next(it2) != dq2.rend() &&
            (limit == threshold.begin() || *prev(limit) < *next(it2));
          if (preferNextIndex2) continue;

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
    return length;
  }

  //
  // Match() avoids m*n comparisons by using CHAR_TO_INDEXES_MAP to
  // achieve O(m+n) performance, where m and n are the input lengths.
  //
  // The lookup time can be assumed constant in the case of characters.
  // The symbol space is larger in the case of records; but the lookup
  // time will be O(log(m+n)), at most.
  //
  void Match(CHAR_TO_INDEXES_MAP& indexesOf2MatchedByChar, MATCHES& indexesOf2MatchedByIndex1,
    const string& s1, const string& s2) {
    uint32_t index = 0;
    for (const auto& it : s2)
      indexesOf2MatchedByChar[it].push_back(index++);

    for (const auto& it : s1) {
      auto& dq2 = indexesOf2MatchedByChar[it];
      indexesOf2MatchedByIndex1.push_back(&dq2);
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
    CHAR_TO_INDEXES_MAP indexesOf2MatchedByChar;
    MATCHES indexesOf2MatchedByIndex1;  // holds references into indexesOf2MatchedByChar
    Match(indexesOf2MatchedByChar, indexesOf2MatchedByIndex1, s1, s2);
    shared_ptr<Pair> pairs;             // obtain the LCS as index pairs
    auto length = FindLCS(indexesOf2MatchedByIndex1, &pairs);
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