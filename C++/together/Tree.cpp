// (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
//
// 2017-05-14 CNHume  Added static root
// 2017-05-13 CNHume  Added branch() and ensure()
// 2015-01-04 CNHume  Qualified const methods
// 2014-12-18 CNHume  Added array usage metrics
// 2014-12-08 CNHume  Added Copy and Move Semantics
// 2014-12-07 CNHume  Sparse 3x3x3 array reduced node size by one half
// 2014-11-26 CNHume  Moved implementation of the Tree class into Tree.cpp
//
#define TIME_GROW
#define TIME_FIND

#include "Tree.h"
#include <chrono>

using namespace chrono;

bool Tree::foLess::operator()(const string& s1, const string& s2) const {
  // Order by descending length, then by normal collation
  return s1.length() > s2.length() || (s1.length() == s2.length() && s1 < s2);
}

//
// Static initialization:
//
Tree& Tree::root = *new Tree();
int64_t Tree::Arr2s = 0;
int64_t Tree::Arr1s = 0;
int64_t Tree::Nodes = 0;

void Tree::deleteBranches() {
  for (size_t m2 = 0; m2 < radix; m2++) {
    if (_branch[m2] != nullptr) {
      for (size_t m1 = 0; m1 < radix; m1++) {
        if (_branch[m2][m1] != nullptr) {
          for (size_t m0 = 0; m0 < radix; m0++)
            delete _branch[m2][m1][m0];
          delete[] _branch[m2][m1];
          Arr1s--;
        }
      }
      delete[] _branch[m2];
      Arr2s--;
      _branch[m2] = nullptr;            // for copy and move assignments
    }
  }
}

Tree::~Tree() {
  deleteBranches();
  Nodes--;
}

void Tree::initBranches() {
  for (size_t m2 = 0; m2 < radix; m2++)
    _branch[m2] = nullptr;
}

Tree::Tree() {
  initBranches();
  Nodes++;
}

#ifdef COPY_SEMANTICS
void Tree::copyBranches(const Tree& other) {
  for (auto m2 = 0; m2 < radix; m2++)
    if (other._branch[m2] == nullptr)
      _branch[m2] = nullptr;
    else {
      _branch[m2] = new Tree**[radix];
      Arr2s++;
      for (auto m1 = 0; m1 < radix; m1++)
        if (other._branch[m2][m1] == nullptr)
          _branch[m2][m1] = nullptr;
        else {
          _branch[m2][m1] = new Tree*[radix];
          Arr1s++;
          for (auto m0 = 0; m0 < radix; m0++) {
            _branch[m2][m1][m0] = other._branch[m2][m1][m0] == nullptr ?
              nullptr : new Tree(*other._branch[m2][m1][m0]); // Recursive Copy
          }
        }
    }
}

Tree::Tree(const Tree& other) {
  _word = other._word;
  copyBranches(other);
  Nodes++;
}

Tree& Tree::operator=(const Tree& other) {
  if (this != &other) {
    _word = other._word;
    deleteBranches();
    copyBranches(other);
  }
  return *this;
}
#endif                                  // COPY_SEMANTICS

#ifdef MOVE_SEMANTICS
void Tree::moveBranches(Tree& other) {
  for (auto m2 = 0; m2 < radix; m2++) {
    _branch[m2] = other._branch[m2];
    other._branch[m2] = nullptr;
  }
}

Tree::Tree(Tree&& other) {
  initBranches();
  *this = std::move(other);             // move ctor invokes move assignment
  Nodes++;
}

Tree& Tree::operator=(Tree&& other) {
  if (this != &other) {
    _word = other._word;
    deleteBranches();
    moveBranches(other);
  }
  return *this;
}
#endif                                  // MOVE_SEMANTICS

inline void Tree::digits(uint32_t u, uint8_t& m0, uint8_t& m1, uint8_t& m2) {
  m0 = u % radix;
  u /= radix;
  m1 = u % radix;
  u /= radix;
  m2 = u % radix;
}

void Tree::Add(const char* pSuffix, const string& word) {
  if (*pSuffix == 0)
    _word = word;
  else {
    auto& branch = ensure(*pSuffix++ - ca);
    branch.Add(pSuffix, word);
  }
}

Tree& Tree::ensure(uint32_t u) {
  uint8_t m0, m1, m2;
  digits(u, m0, m1, m2);

  if (_branch[m2] == nullptr) {
    _branch[m2] = new Tree**[radix];
    Arr2s++;
    for (auto m1 = 0; m1 < radix; m1++)
      _branch[m2][m1] = nullptr;
  }

  if (_branch[m2][m1] == nullptr) {
    _branch[m2][m1] = new Tree*[radix];
    Arr1s++;
    for (auto m0 = 0; m0 < radix; m0++)
      _branch[m2][m1][m0] = nullptr;
  }

  if (_branch[m2][m1][m0] == nullptr)
    _branch[m2][m1][m0] = new Tree;

  return *_branch[m2][m1][m0];
}

//
// The sparse 3x3x3 array reduced node size by one half.
// _branch can be a simple 26-element array.
//
Tree* Tree::branch(uint32_t u) const {
  uint8_t m0, m1, m2;
  digits(u, m0, m1, m2);

  if (_branch[m2] == nullptr || _branch[m2][m1] == nullptr)
    return nullptr;
  else
    return _branch[m2][m1][m0];
}

//
// Count() and List() rely on the Tree structure built by Add() to find
// the maximum number of words that can be used to form compound words.
// Each word is a solitary compound of itself.
//
void Tree::List(const char* pSuffix, STRINGLIST& prefixes) const {
  if (*pSuffix == 0) {                  // Suffix traversed: Did it match a word?
    if (_word.empty())
      prefixes.clear();
    else
      prefixes.push_back(_word);
  }
  else {
    // Restart climb from the root when a Prefix is found:
    STRINGLIST tail1;
    if (!_word.empty()) {
      tail1.push_back(_word);
      root.List(pSuffix, tail1);
    }

    // Also follow branch to the leaf:
    STRINGLIST tail2;
    auto pBranch = branch(*pSuffix++ - ca);
    if (pBranch) pBranch->List(pSuffix, tail2);

    // Return alternative using the most words:
    auto used1 = tail1.size();
    auto used2 = tail2.size();

    auto used = used1 < used2 ? used2 : used1;
    auto tail = used1 < used2 ? tail2 : tail1;

    if (used > 0)
      prefixes.splice(prefixes.end(), tail);
    else
      prefixes.clear();
  }
}

uint32_t Tree::Count(const char* pSuffix, uint32_t prefixCount) const {
  if (*pSuffix == 0)                    // Suffix traversed: Did it match a word?
    return _word.empty() ? 0 : prefixCount + 1;
  else {
    // Restart climb from the root when a Prefix is found:
    auto used1 = _word.empty() ? 0 : root.Count(pSuffix, prefixCount + 1);

    // Also follow branch to the leaf:
    auto pBranch = branch(*pSuffix++ - ca);
    uint32_t used2 = pBranch ? pBranch->Count(pSuffix, prefixCount) : 0;

    // Return alternative using the most words:
    return used1 < used2 ? used2 : used1;
  }
}

uint32_t Tree::FindList(const STRINGSET& words, uint32_t display) {
  STRINGLIST prefixes;
  auto compounds = 0U;
  for (const auto& it : words) {
    root.List(it.c_str(), prefixes);
    auto used = prefixes.size();

    if (used > 1) {                     // Ignore non-compounds
      if (compounds++ < display) {
        cout << it << " is a compound of " << used << " words: ";
        cout << join(prefixes) << endl;
      }
    }

    prefixes.clear();
  }
  return compounds;
}

uint32_t Tree::FindCount(const STRINGSET& words, uint32_t display) {
  auto compounds = 0U;
  for (const auto& it : words) {
    auto used = root.Count(it.c_str());

    if (used > 1) {                     // Ignore non-compounds
      if (compounds++ < display)
        cout << it << " is a compound of " << used << " words" << endl;
    }
  }
  return compounds;
}

//
// Grow the Tree that will allow compounds to be found efficiently
//
void Tree::Grow(const STRINGSET& words) {
  auto n0 = Tree::Nodes;
  auto a1 = Tree::Arr1s;
  auto a2 = Tree::Arr2s;
  auto chars = 0ULL;

  for (const auto& it : words) {
    root.Add(it.c_str(), it);
    chars += it.length();
  }

  cout << chars << " characters" << endl;
  cout << Tree::Nodes - n0 << " nodes of size " << sizeof(Tree) << " grew" << endl;
  cout << Tree::Arr1s - a1 << " arr1s grew" << endl;
  cout << Tree::Arr2s - a2 << " arr2s grew" << endl;
  auto total = Tree::Arr2s - a2 + Tree::Arr1s - a1;
  auto size = total * sizeof(_branch);  // Assume arrays are same size as the root
  cout << size << " array bytes" << endl;
  // Average bytes per node = 58 [vs. 128 using one dimension]
}

//
// Find compound words
//
uint32_t Tree::Compound(const STRINGSET& words, uint32_t display, bool test) {
#ifdef TIME_GROW
  auto grow_t0 = high_resolution_clock::now();
#endif
  Grow(words);
#ifdef TIME_GROW
  auto grow_t1 = high_resolution_clock::now();
  auto grow_msec = duration_cast<milliseconds>(grow_t1 - grow_t0).count();
  cout << grow_msec / 1E3 << " sec in Grow()" << endl;
#endif
#ifdef TIME_FIND
  auto find_t0 = high_resolution_clock::now();
#endif
  auto result = test ? FindList(words, display) : FindCount(words, display);
#ifdef TIME_FIND
  auto find_t1 = high_resolution_clock::now();
  auto find_msec = duration_cast<milliseconds>(find_t1 - find_t0).count();
  auto name = test ? "FindList()" : "FindCount()";
  cout << find_msec / 1E3 << " sec in " << name << endl;
#endif
  return result;
}
