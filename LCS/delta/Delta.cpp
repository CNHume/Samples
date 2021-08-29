// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-06-25 CNHume  Refactored Lengths()
// 2014-12-31 CNHume  Created file
//
#include "Delta.h"
#include <algorithm>                    // for min()

//
// Delta Class Implementation
//
Delta::~Delta() {
}

Delta::Delta() {
}

Delta::Delta(uint32_t begin1, uint32_t begin2, uint32_t end1, uint32_t end2, shared_ptr<Delta> next)
  : Pair(begin1, begin2, next), end1(end1), end2(end2) {
}

void Delta::Lengths(const shared_ptr<Delta> deltas, uint32_t& length1, uint32_t& length2) {
  length1 = 0;
  length2 = 0;
  for (auto next = deltas; next != nullptr; next = dynamic_pointer_cast<Delta>(next->next)) {
    length1 += next->end1 - next->begin1;
    length2 += next->end2 - next->begin2;
  }
}

//
// Coalesce() gathers runs of successive Pairs to create Deltas
//
shared_ptr<Delta> Delta::Coalesce(shared_ptr<Pair> pairs) {
  shared_ptr<Delta> head = nullptr;
  shared_ptr<Delta> last = nullptr;
  uint32_t first1 = 0;
  uint32_t first2 = 0;
  for (auto next = pairs; next != nullptr; next = next->next) {
    auto begin1 = next->begin1;
    auto begin2 = next->begin2;
    auto end1 = begin1 + 1;
    auto end2 = begin2 + 1;
    if (last == nullptr || first1 < begin1 || first2 < begin2) {
      auto deltas = make_shared<Delta>(begin1, begin2, end1, end2);
      last = last == nullptr ?
        head = deltas : static_pointer_cast<Delta>(last->next = deltas);
    }
    else {
      last->end1 = end1;
      last->end2 = end2;
    }

    first1 = end1;
    first2 = end2;
  }

  return head;
}

//
// Coalesce() overload to gather successive Deltas,
// when no more than join matches intervene
//
shared_ptr<Delta> Delta::Coalesce(const shared_ptr<Delta> deltas, uint32_t join) {
  shared_ptr<Delta> head = nullptr;
  shared_ptr<Delta> last = nullptr;
  uint32_t first1 = 0;
  uint32_t first2 = 0;
  for (auto next = deltas; next != nullptr; next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin1 = next->begin1;
    auto begin2 = next->begin2;
    auto end1 = next->end1;
    auto end2 = next->end2;
    if (last == nullptr || first1 + (uint64_t)join < begin1 || first2 + (uint64_t)join < begin2) {
      auto deltas = make_shared<Delta>(begin1, begin2, end1, end2);
      last = last == nullptr ?
        head = deltas : static_pointer_cast<Delta>(last->next = deltas);
    }
    else {
      last->end1 = end1;
      last->end2 = end2;
    }

    first1 = end1;
    first2 = end2;
  }

  return head;
}

//
// Complement() generates deltas where there were breaks between input
// Deltas and omits the original Deltas, leaving breaks in their place
//
shared_ptr<Delta> Delta::Complement(shared_ptr<Delta> deltas,
  size_t size1, size_t size2) {
  uint32_t prior1 = 0;                  // end of prior Delta
  uint32_t prior2 = 0;

  shared_ptr<Delta> head = nullptr;
  shared_ptr<Delta> rest = nullptr;

  for (auto next = deltas; next != nullptr; next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin1 = next->begin1;         // beginning of current Delta
    auto begin2 = next->begin2;
    auto end1 = next->end1;             // end of current Delta
    auto end2 = next->end2;

    if (prior1 < begin1 || prior2 < begin2) {
      auto last = make_shared<Delta>(prior1, prior2, begin1, begin2);
      rest = rest == nullptr ? head = last : static_pointer_cast<Delta>(rest->next = last);
    }

    prior1 = end1;
    prior2 = end2;
  }

  // empty final delta
  if (prior1 < size1 || prior2 < size2) {
    auto last = make_shared<Delta>(prior1, prior2, size1, size2);
    rest = rest == nullptr ? head = last : static_pointer_cast<Delta>(rest->next = last);
  }

  //prior1 = size1;
  //prior2 = size2;
  return head;
}

//
// Context() prefixes and suffixes matching records onto each Delta
//
void Delta::Context(shared_ptr<Delta> deltas,
  size_t size1, size_t size2, uint32_t prefix, uint32_t suffix) {
  uint32_t prior1 = 0;                  // end of prior Delta
  uint32_t prior2 = 0;

  shared_ptr<Delta> head = nullptr;
  shared_ptr<Delta> last = nullptr;

  for (auto next = deltas; next != nullptr; next = dynamic_pointer_cast<Delta>(next->next)) {
    auto begin1 = next->begin1;         // beginning of current Delta
    auto begin2 = next->begin2;
    auto last1 = prior1 + (uint64_t)suffix < begin1 ? prior1 + suffix : begin1;
    auto last2 = prior2 + (uint64_t)suffix < begin2 ? prior2 + suffix : begin2;

    if (last != nullptr) {
      last->end1 = last1;
      last->end2 = last2;
    }

    next->begin1 = last1 + (uint64_t)prefix < begin1 ? begin1 - prefix : last1;
    next->begin2 = last2 + (uint64_t)prefix < begin2 ? begin2 - prefix : last2;

    prior1 = next->end1;                // end of current Delta sans suffix
    prior2 = next->end2;
    last = next;
  }

  // empty final delta
  if (last != nullptr) {
    auto last1 = prior1 + (uint64_t)suffix < size1 ? prior1 + suffix : size1;
    auto last2 = prior2 + (uint64_t)suffix < size2 ? prior2 + suffix : size2;
    last->end1 = last1;
    last->end2 = last2;
  }

  //prior1 = size1;
  //prior2 = size2;
}
