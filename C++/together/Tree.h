// (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
//
// 2025-12-12 CNHume  Removed use of std::binary_function, deprecated since C++11.
// 2017-05-14 CNHume  Added static root
// 2017-05-13 CNHume  Added branch() and ensure()
// 2015-01-04 CNHume  Qualified const methods
// 2014-12-18 CNHume  Added array usage metrics
// 2014-12-08 CNHume  Added Copy and Move Semantics
// 2014-12-07 CNHume  Sparse 3x3x3 array reduced node size by one half
// 2014-11-26 CNHume  Moved declaration of the Tree class into Tree.h
//
#pragma once
#define COPY_SEMANTICS
#define MOVE_SEMANTICS

#include "join.h"

#include <stdint.h>
#include <string>
#include <list>
#include <set>
#include <iostream>                     // for cout

using namespace std;

class Tree {
private:
  static const size_t radix = 3;
  static const char ca = 'a', cz = 'z';
  static const size_t letters = cz - ca + 1;

protected:
  Tree*** _branch[radix];               // sparse 3x3x3 array reduces node size
  string _word;
  void initBranches();
  void deleteBranches();
#ifdef COPY_SEMANTICS
  void copyBranches(const Tree& other);
#endif
#ifdef MOVE_SEMANTICS
  void moveBranches(Tree& other);
#endif
  inline static void digits(uint32_t u, uint8_t& m0, uint8_t& m1, uint8_t& m2);

public:
  struct foLess {
    bool operator()(const string &s1, const string &s2) const;
  };

  typedef set<string, foLess> STRINGSET;// to prevent duplicates
  typedef list<string> STRINGLIST;      // to display compounds

  //
  // Static variables:
  //
  static Tree& root;
  static int64_t Nodes;
  static int64_t Arr1s;
  static int64_t Arr2s;

  virtual ~Tree();
  Tree();
#ifdef COPY_SEMANTICS
  Tree(const Tree& other) noexcept;
  Tree& operator=(const Tree& other) noexcept;
#endif
#ifdef MOVE_SEMANTICS
  Tree(Tree&& other) noexcept;
  Tree& operator=(Tree&& other) noexcept;
#endif
  void Add(const char* pSuffix, const string& word);
  Tree& ensure(uint32_t u);
  Tree* branch(uint32_t u) const;
  void List(const char* pSuffix, STRINGLIST& prefixes) const;
  uint32_t Count(const char* pSuffix, uint32_t prefixCount = 0) const;

  //
  // Static methods:
  //
  static void Grow(const STRINGSET& words);
  static uint32_t FindList(const STRINGSET& words, uint32_t display);
  static uint32_t FindCount(const STRINGSET& words, uint32_t display);
  static uint32_t Compound(const STRINGSET& words, uint32_t display = 0, bool test = false);
};
