// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2026-10-09 CNHume  Created LCSRickSpace class
//
// Rick (2000) linear-space LCS, adapted to reuse Hunt-Szymanski Match()
//
// See LCSRickSpace.h for an overview.
//
#include "LCSRickSpace.h"

#include <algorithm>                    // for min, reverse
#include <cstdint>                      // for int64_t, uint32_t, UINT32_MAX
#include <memory>                       // for make_shared
#include <unordered_map>                // for pointer -> symbol id
#include <utility>                      // for move
#include <vector>                       // for vector

//
// A match (row, column) has forward rank k when the longest common
// subsequence of the prefixes s1[0..row] and s2[0..column] ending at
// (row, column) has length k.  The k-th forward contour FC[k] is the
// antichain of "dominant" (minimal under the product order) rank-k matches.
// Backward contours BC[k] are the symmetric notion applied to the reversed
// strings.  Two contours are computed in an alternating fashion until they
// cross, which fixes the LCS length and a midpoint match.  Hirschberg
// recursion reconstructs the full LCS from the midpoints.
//
// Each contour scan needs, per row, the first (or last) occurrence of s1[row]
// relative to a bound.  These queries are answered in O(1) by the
// next/previous occurrence tables built from the shared match lists.
//
namespace {
  struct Match {
    uint32_t row = 0;
    uint32_t column = 0;
  };

  struct Result {
    bool hasMidpoint = false;
    uint32_t length = 0;               // LCS length of the sub-problem
    Match midpoint;
  };

  // A contour is an antichain of dominant matches sorted by ascending row,
  // which forces descending column.
  typedef vector<Match> Contour;

  //
  //[Assert]next[id][j] = first column >= j in s2 holding symbol id (columns if
  // none), prev[id][j] = last column <= j; next1/prev1 are the same queries
  // over s1 rows.  Each contour-scan step therefore advances straight to the
  // next/previous occurrence in O(1) instead of binary-searching a position
  // list — the "LeftPos/TopPos" trick behind the time bound.
  //
  // O(1) next/previous occurrence tables over both strings.  rowId[row] is
  // the symbol id of s1[row] and colId[column] that of s2[column] (UINT32_MAX
  // when the symbol never occurs); next/prev are over s2 (the longer string)
  // and next1/prev1 over s1, each giving the smallest position >= column /
  // largest position <= column holding the symbol (length / UINT32_MAX if
  // none).  This is the "LeftPos/TopPos" O(ns) preprocessing of the Rick
  // contour scan, extended to both strings so the column sweep of the dual
  // diagonal sweep can run.
  //
  struct Occurrences {
    vector<uint32_t> rowId;             // symbol id of each s1 position
    vector<uint32_t> colId;             // symbol id of each s2 position
    vector<vector<uint32_t>> next;      // [symbol][column] over s2, [0, columns]
    vector<vector<uint32_t>> prev;      // [symbol][column] over s2, [0, columns)
    vector<vector<uint32_t>> next1;     // [symbol][row] over s1, [0, rows]
    vector<vector<uint32_t>> prev1;     // [symbol][row] over s1, [0, rows]
    int64_t rows = 0;                   // size1
    int64_t columns = 0;                // size2
  };

  // Forward declarations (definitions follow).
  uint32_t Solve(shared_ptr<Pair>* pairs, const Occurrences& occ,
    size_t size1, size_t size2);
  Occurrences BuildOccurrences(const LCS::MATCHES& matches,
    size_t size1, size_t size2);
  Occurrences BuildOccurrences(const u32string& s1, const u32string& s2);
  void Hirschberg(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1, vector<Match>& out);
  Result FindMidpoint(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1);
  Contour CutForward(const Contour& forward, const Contour& backward);
  Contour CutBackward(const Contour& backward, const Contour& forward);
  Contour FirstForward(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1);
  Contour NextForward(const Contour& contour, const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1);
  Contour FirstBackward(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1);
  Contour NextBackward(const Contour& contour, const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1);
  bool Crossed(const Contour& forward, const Contour& backward);
  Match Midpoint(const Contour& forward, const Contour& backward);

  //
  // Runs the Hirschberg recursion over an occurrence table and, when pairs is
  // requested, reconstructs the linked list of matched positions.  When pairs is
  // null only the length is wanted, so one forward/backward contour alternation
  // (FindMidpoint) suffices and the recursion is skipped.
  //
  uint32_t Solve(shared_ptr<Pair>* pairs, const Occurrences& occ,
    size_t size1, size_t size2) {
    if (pairs == nullptr)
      return FindMidpoint(occ, 0, size1, 0, size2).length;

    vector<Match> out;
    Hirschberg(occ, 0, size1, 0, size2, out);

    if (pairs != nullptr) {
      shared_ptr<Pair> head = nullptr;
      shared_ptr<Pair> tail = nullptr;
      for (const auto& match : out) {
        //
        // Translate the matrix (row, column) match back into input-sequence
        // indexes: row is the position in s1 (Pair::begin1) and column the
        // position in s2 (Pair::begin2).
        //
        auto pair = make_shared<Pair>(match.row, match.column);
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

  //
  // Builds O(1) next/previous occurrence tables from the shared match lists in
  // O(s*n + r) time and space, where s is the number of distinct symbols
  // (match-list pointers) and r the total number of matches.
  //
  // This is the "LeftPos/TopPos" preprocessing that lets each contour scan jump
  // straight to the next (or previous) occurrence vs. binary-searching a list.
  //
  Occurrences BuildOccurrences(
    const LCS::MATCHES& matches, size_t size1, size_t size2) {
    Occurrences occ;
    occ.rows = (int64_t)size1;
    occ.columns = (int64_t)size2;

    //
    // Two rows sharing a symbol share the same match-list pointer, so pointer
    // identity compresses the symbols without needing the symbol values.
    //
    unordered_map<const LCS::INDEXES*, uint32_t> pointerToId;
    vector<const LCS::INDEXES*> lists;  // lists[id] = sorted positions of symbol id
    occ.rowId.assign(size1, UINT32_MAX);
    for (size_t rowIndex = 0; rowIndex < size1; rowIndex++) {
      const auto* matchList = matches[rowIndex];
      auto found = pointerToId.find(matchList);
      uint32_t symbolId;
      if (found == pointerToId.end()) {
        symbolId = (uint32_t)lists.size();
        pointerToId.emplace(matchList, symbolId);
        lists.push_back(matchList);
      }
      else {
        symbolId = found->second;
      }
      occ.rowId[rowIndex] = symbolId;
    }

    auto symbols = lists.size();
    occ.next.assign(symbols, vector<uint32_t>((size_t)size2 + 1, (uint32_t)occ.columns));
    occ.prev.assign(symbols, vector<uint32_t>((size_t)size2, UINT32_MAX));

    // colId[j] = symbol id of s2[j], inverting the match lists.
    occ.colId.assign(size2, UINT32_MAX);

    for (size_t symbolId = 0; symbolId < symbols; symbolId++) {
      const auto& list = *lists[symbolId];    // sorted positions, ascending
      auto& nextTable = occ.next[symbolId];
      auto& prevTable = occ.prev[symbolId];

      // next[id][j] = first list element >= j.
      size_t listIndex = 0;
      for (int64_t columnIndex = 0; columnIndex <= occ.columns; columnIndex++) {
        while (listIndex < list.size() && (int64_t)list[listIndex] < columnIndex)
          listIndex++;
        nextTable[(size_t)columnIndex] = listIndex < list.size() ? list[listIndex] : (uint32_t)occ.columns;
      }

      // prev[id][j] = last list element <= j.
      listIndex = 0;
      uint32_t last = UINT32_MAX;
      for (int64_t columnIndex = 0; columnIndex < occ.columns; columnIndex++) {
        while (listIndex < list.size() && (int64_t)list[listIndex] <= columnIndex) {
          last = list[listIndex];
          listIndex++;
        }
        prevTable[(size_t)columnIndex] = last;
      }

      // Invert: each position in the list holds symbol id.
      for (auto columnIndex : list)
        occ.colId[(size_t)columnIndex] = (uint32_t)symbolId;
    }

    // next1[id][i] = first occurrence of symbol id at/after i in s1 (m if none).
    occ.next1.assign(symbols, vector<uint32_t>((size_t)occ.rows + 1, (uint32_t)occ.rows));
    for (int64_t rowIndex = occ.rows - 1; rowIndex >= 0; rowIndex--) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.next1[symbolId][(size_t)rowIndex] = occ.next1[symbolId][(size_t)rowIndex + 1];
      auto rowSymbol = occ.rowId[(size_t)rowIndex];
      if (rowSymbol != UINT32_MAX)
        occ.next1[(size_t)rowSymbol][(size_t)rowIndex] = (uint32_t)rowIndex;
    }

    // prev1[id][i] = last occurrence of symbol id at/before i in s1 (UINT32_MAX if none).
    occ.prev1.assign(symbols, vector<uint32_t>((size_t)occ.rows, UINT32_MAX));
    for (int64_t rowIndex = 0; rowIndex < occ.rows; rowIndex++) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.prev1[symbolId][(size_t)rowIndex] = rowIndex > 0 ? occ.prev1[symbolId][(size_t)rowIndex - 1] : UINT32_MAX;
      auto rowSymbol = occ.rowId[(size_t)rowIndex];
      if (rowSymbol != UINT32_MAX)
        occ.prev1[(size_t)rowSymbol][(size_t)rowIndex] = (uint32_t)rowIndex;
    }

    return occ;
  }

  //
  // Character-path overload: builds the same tables directly from code-point
  // strings in O(s*n) time and space, where s is the number of distinct code
  // points in s2.  No match lists are materialized, so the preprocessing stays
  // linear even when the number of matches r would be quadratic.
  //
  Occurrences BuildOccurrences(
    const u32string& s1, const u32string& s2) {
    Occurrences occ;
    occ.rows = (int64_t)s1.size();
    occ.columns = (int64_t)s2.size();

    // Compress the code points of s2 (the tabled string) to dense ids.
    unordered_map<char32_t, uint32_t> charToId;
    vector<uint32_t> s2Id(s2.size(), UINT32_MAX);
    for (size_t columnIndex = 0; columnIndex < s2.size(); columnIndex++) {
      auto codePoint = s2[columnIndex];
      auto found = charToId.find(codePoint);
      uint32_t symbolId;
      if (found == charToId.end()) {
        symbolId = (uint32_t)charToId.size();
        charToId.emplace(codePoint, symbolId);
      }
      else {
        symbolId = found->second;
      }
      s2Id[columnIndex] = symbolId;
    }

    auto symbols = charToId.size();
    occ.colId = move(s2Id);

    // rowId[i] = symbol id of s1[i], or UINT32_MAX when it never occurs in s2.
    occ.rowId.assign(s1.size(), UINT32_MAX);
    for (size_t rowIndex = 0; rowIndex < s1.size(); rowIndex++) {
      auto found = charToId.find(s1[rowIndex]);
      if (found != charToId.end())
        occ.rowId[rowIndex] = found->second;
    }

    // next[id][j] = first occurrence of symbol id at/after j (n if none).
    occ.next.assign(symbols, vector<uint32_t>((size_t)occ.columns + 1, (uint32_t)occ.columns));
    for (int64_t columnIndex = occ.columns - 1; columnIndex >= 0; columnIndex--) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.next[symbolId][(size_t)columnIndex] = occ.next[symbolId][(size_t)columnIndex + 1];
      occ.next[(size_t)occ.colId[(size_t)columnIndex]][(size_t)columnIndex] = (uint32_t)columnIndex;
    }

    // prev[id][j] = last occurrence of symbol id at/before j (UINT32_MAX if none).
    occ.prev.assign(symbols, vector<uint32_t>((size_t)occ.columns, UINT32_MAX));
    for (int64_t columnIndex = 0; columnIndex < occ.columns; columnIndex++) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.prev[symbolId][(size_t)columnIndex] = columnIndex > 0 ? occ.prev[symbolId][(size_t)columnIndex - 1] : UINT32_MAX;
      occ.prev[(size_t)occ.colId[(size_t)columnIndex]][(size_t)columnIndex] = (uint32_t)columnIndex;
    }

    // next1[id][i] = first occurrence of symbol id at/after i in s1 (m if none).
    occ.next1.assign(symbols, vector<uint32_t>((size_t)occ.rows + 1, (uint32_t)occ.rows));
    for (int64_t rowIndex = occ.rows - 1; rowIndex >= 0; rowIndex--) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.next1[symbolId][(size_t)rowIndex] = occ.next1[symbolId][(size_t)rowIndex + 1];
      auto rowSymbol = occ.rowId[(size_t)rowIndex];
      if (rowSymbol != UINT32_MAX)
        occ.next1[(size_t)rowSymbol][(size_t)rowIndex] = (uint32_t)rowIndex;
    }

    // prev1[id][i] = last occurrence of symbol id at/before i in s1 (UINT32_MAX if none).
    occ.prev1.assign(symbols, vector<uint32_t>((size_t)occ.rows, UINT32_MAX));
    for (int64_t rowIndex = 0; rowIndex < occ.rows; rowIndex++) {
      for (size_t symbolId = 0; symbolId < symbols; symbolId++)
        occ.prev1[symbolId][(size_t)rowIndex] = rowIndex > 0 ? occ.prev1[symbolId][(size_t)rowIndex - 1] : UINT32_MAX;
      auto rowSymbol = occ.rowId[(size_t)rowIndex];
      if (rowSymbol != UINT32_MAX)
        occ.prev1[(size_t)rowSymbol][(size_t)rowIndex] = (uint32_t)rowIndex;
    }

    return occ;
  }

  void Hirschberg(
    const Occurrences& occ, size_t row0, size_t row1, size_t column0, size_t column1,
    vector<Match>& out) {
    if (row0 >= row1 || column0 >= column1)
      return;
    auto result = FindMidpoint(occ, row0, row1, column0, column1);
    if (!result.hasMidpoint)
      return;

    //
    // Rick (2000) early termination: when the shorter string is matched in its
    // entirety, every one of its positions lies on the LCS, so emit the greedy
    // leftmost matches directly instead of recursing.  This prunes the recursion
    // tree for near-identical inputs and drops the O(log m) factor.
    //
    auto rowCount = row1 - row0;
    auto columnCount = column1 - column0;
    if (result.length == rowCount && rowCount <= columnCount) {
      // All rows matched: greedy leftmost occurrence per row.
      size_t bound = column0;
      for (auto row = row0; row < row1; row++) {
        auto symbol = occ.rowId[row];
        auto column = occ.next[symbol][bound];
        out.push_back({ .row = (uint32_t)row, .column = column });
        bound = (size_t)column + 1;
      }
      return;
    }
    if (result.length == columnCount) {
      // All columns matched: greedy topmost occurrence per column.
      size_t bound = row0;
      for (auto column = column0; column < column1; column++) {
        auto symbol = occ.colId[column];
        auto row = occ.next1[symbol][bound];
        out.push_back({ .row = row, .column = (uint32_t)column });
        bound = (size_t)row + 1;
      }
      return;
    }

    auto rowIndex = (size_t)result.midpoint.row;
    auto columnIndex = (size_t)result.midpoint.column;
    Hirschberg(occ, row0, rowIndex, column0, columnIndex, out);
    out.push_back(result.midpoint);
    Hirschberg(occ, rowIndex + 1, row1, columnIndex + 1, column1, out);
  }

  Result FindMidpoint(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1) {
    Result result;
    if (row0 >= row1 || column0 >= column1)
      return result;

    //
    //[Assert]Across iterations fc == FC[fCount] and bc == BC[bCount];
    // and only fcPrev/bcPrev are retained, so the working set stays
    // O(|fc|+|bc|) linear in the sub-problem, rather than O(p·m).
    //
    // The loop alternates FC[1], BC[1], FC[2], BC[2], ...
    // where |fCount - bCount| <= 1 until the two newest contours cross,
    // which fixes length = fCount + bCount - 1 to find a midpoint for the
    // Hirschberg split.
    //
    Contour fc, fcPrev, bc, bcPrev;
    int64_t fCount = 0;                 // # forward contours computed
    int64_t bCount = 0;                 // # backward contours computed

    while (true) {
      // Alternate: FC[1], BC[1], FC[2], BC[2], ... so |fCount - bCount| <= 1.
      if (fCount <= bCount) {
        fCount++;
        fcPrev = move(fc);
        if (fCount == 1) {
          fc = FirstForward(occ, row0, row1, column0, column1);
        }
        else {
          // Lemma 5: only extend from forward matches still "uncovered" by the
          // backward contours (those with a BC match strictly below-right).
          auto sources = CutForward(fcPrev, bc);
          fc = NextForward(sources.empty() ? fcPrev : sources,
            occ, row0, row1, column0, column1);
        }
        if (fc.empty())
          return result;                // No matches: sub-LCS length is zero.
      }
      else {
        bCount++;
        bcPrev = move(bc);
        if (bCount == 1) {
          bc = FirstBackward(occ, row0, row1, column0, column1);
        }
        else {
          // Symmetric cut for the backward direction.
          auto sources = CutBackward(bcPrev, fc);
          bc = NextBackward(sources.empty() ? bcPrev : sources,
            occ, row0, row1, column0, column1);
        }
      }

      if (fCount >= 1 && bCount >= 1 && Crossed(fc, bc)) {
        result.hasMidpoint = true;
        result.length = (uint32_t)(fCount + bCount - 1);
        result.midpoint = Midpoint(fc, bc);
        return result;
      }
    }
  }

  //
  // Lemma 5 contour cutting
  //
  Contour CutForward(const Contour& forward, const Contour& backward) {
    Contour kept;
    //
    // Both contours are sorted by ascending row (hence descending column).
    // For each forward match, the first backward match with row > match.row also
    // has the largest column of any backward match below-right of it, so it
    // alone decides whether the match is covered.  A single merged scan runs in
    // linear time O(|forward| + |backward|) instead of the quadratic product.
    //
    size_t backwardIndex = 0;               // first backward match with row > match.row
    for (const auto& match : forward) {
      while (backwardIndex < backward.size() && backward[backwardIndex].row <= match.row)
        backwardIndex++;
      if (backwardIndex < backward.size() && backward[backwardIndex].column > match.column)
        kept.push_back(match);
    }
    return kept;
  }

  Contour CutBackward(const Contour& backward, const Contour& forward) {
    Contour kept;
    //
    // Symmetric to CutForward: for each backward match, the last forward match
    // with row < match.row has the smallest column of any forward match
    // above-left of it, so it alone decides whether the match is covered.
    //
    size_t forwardIndex = 0;              // first forward match with row >= match.row
    for (const auto& match : backward) {
      while (forwardIndex < forward.size() && forward[forwardIndex].row < match.row)
        forwardIndex++;
      if (forwardIndex > 0 && forward[forwardIndex - 1].column < match.column)
        kept.push_back(match);
    }
    return kept;
  }

  //
  // FC[1]: dominant rank-1 matches of the sub-problem [row0, row1) x [column0, column1).
  // This is the dual diagonal sweep with no predecessor contour.
  //
  Contour FirstForward(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1) {
    return NextForward(Contour{}, occ, row0, row1, column0, column1);
  }

  //
  // FC[k + 1]: dominant matches strictly bottom-right of some FC[k] match,
  // computed by Rick's dual diagonal sweep: a row sweep for the horizontal
  // stripe (column - column0 >= row - row0) and a column sweep for the vertical
  // stripe (row - row0 > column - column0), interleaved over the diagonal and
  // stopped by the lmin completeness test once a dominant match has both components
  // within the current diagonal.  This bounds each contour's work at O(min{m, n-p}).
  //
  Contour NextForward(
    const Contour& contour, const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1) {
    Contour next;
    LCSRickSpace::Contours++;
    auto rowCount = row1 - row0;
    auto columnCount = column1 - column0;
    auto diagonalCount = min(rowCount, columnCount);

    //
    //[Assert]Across the diagonal loop, rowThresh is the leftmost column of a dominant
    // match emitted so far and colThresh the topmost row (both non-increasing), so a
    // candidate (r, c) is emitted as dominant exactly when c < rowThresh and r < colThresh.
    // The two cursors track the predecessor contour relative to the current row/column:
    // matchesAbove counts sources strictly above the row, and leftSources indexes the
    // topmost source strictly left of the column, so an O(1) test rejects any candidate
    // that no source can extend.
    //
    int64_t rowThresh = (int64_t)column1;   // leftmost column of a dominant match
    int64_t colThresh = (int64_t)row1;      // topmost row of a dominant match
    size_t matchesAbove = 0;                // # sources above the current row
    size_t leftSources = contour.size();    // first source left of the current column

    //
    // The sweeps stay idle until a source lies above the row (row sweep) or left
    // of the column (column sweep); begin at the first diagonal that can yield a
    // match instead of walking the whole idle prefix.
    //
    auto startRowDiagonal = contour.empty() ? (size_t)0 : (size_t)contour[0].row - row0 + 1;
    auto startColumnDiagonal = contour.empty() ? (size_t)0 : (size_t)contour.back().column - column0 + 1;
    auto startDiagonal = min(min(startRowDiagonal, startColumnDiagonal), diagonalCount);
    auto startRow = row0 + startDiagonal;
    auto startColumn = column0 + startDiagonal;
    while (matchesAbove < contour.size() && contour[matchesAbove].row < (uint32_t)startRow)
      matchesAbove++;
    while (leftSources > 0 && contour[leftSources - 1].column < (uint32_t)startColumn)
      leftSources--;

    for (size_t diagonal = startDiagonal; diagonal < diagonalCount; diagonal++) {
      LCSRickSpace::Rows++;             // one row + one column sweep step
      auto rowIndex = row0 + diagonal;
      auto columnIndex = column0 + diagonal;

      //
      // Row sweep: horizontal matches in the diagonal row, at/right of the diagonal column.
      // A source above the row is needed unless the contour is empty (the rank-1 case).
      //
      while (matchesAbove < contour.size() && contour[matchesAbove].row < (uint32_t)rowIndex)
        matchesAbove++;
      auto rowId = occ.rowId[(size_t)rowIndex];
      if (rowId != UINT32_MAX && (contour.empty() || matchesAbove > 0)) {
        auto lower = columnIndex;       // first occurrence at/after the diagonal column (horizontal)
        if (matchesAbove > 0)
          lower = max(lower, (size_t)contour[matchesAbove - 1].column + 1);
        auto column = occ.next[(size_t)rowId][lower];
        // Skip if a vertical (column-sweep) match at row <= rowIndex dominates it.
        if (column < rowThresh && (int64_t)rowIndex < colThresh) {
          LCSRickSpace::Candidates++;
          next.push_back({ .row = (uint32_t)rowIndex, .column = column });
          rowThresh = column;
        }
      }

      //
      // Column sweep: vertical matches in the diagonal column, below the diagonal row.
      // A source left of the column is needed unless the contour is empty (the rank-1 case).
      //
      while (leftSources > 0 && contour[leftSources - 1].column < (uint32_t)columnIndex)
        leftSources--;
      auto colId = occ.colId[(size_t)columnIndex];
      if (colId != UINT32_MAX && (contour.empty() || leftSources < contour.size())) {
        auto lower = rowIndex + 1;      // first occurrence below the diagonal row (vertical)
        if (leftSources < contour.size())
          lower = max(lower, (size_t)contour[leftSources].row + 1);
        auto row = occ.next1[(size_t)colId][lower];
        // Skip if a horizontal (row-sweep) match at column <= columnIndex dominates it.
        if (row < colThresh && (int64_t)columnIndex < rowThresh) {
          LCSRickSpace::Candidates++;
          next.push_back({ .row = row, .column = (uint32_t)columnIndex });
          colThresh = row;
        }
      }

      //
      // Completeness: a dominant match with both components within the current
      // diagonal means no later diagonal can hold another dominant match.
      //
      if (rowThresh <= (int64_t)columnIndex || colThresh <= (int64_t)rowIndex)
        break;
    }

    sort(next.begin(), next.end(), [](const Match& left, const Match& right) {
      return left.row < right.row; });
    return next;
  }

  //
  // BC[1]: dominant (maximal) rank-1 matches,
  // the backward dual sweep applied with no predecessor contour.
  //
  Contour FirstBackward(const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1) {
    return NextBackward(Contour{}, occ, row0, row1, column0, column1);
  }

  //
  // BC[k + 1]: dominant matches strictly top-left of some BC[k] match.
  //
  // The backward dual sweep, the mirror image of NextForward: a row sweep for
  // the backward horizontal stripe (column - column0 <= row - row0)
  // and a column sweep for the backward vertical stripe, interleaved over the
  // anti-diagonal and stopped by the symmetric completeness test.
  //
  Contour NextBackward(
    const Contour& contour, const Occurrences& occ,
    size_t row0, size_t row1, size_t column0, size_t column1) {
    Contour next;
    LCSRickSpace::Contours++;
    auto rowCount = row1 - row0;
    auto columnCount = column1 - column0;
    auto diagonalCount = min(rowCount, columnCount);

    //
    //[Assert]Mirror image of NextForward: rowThresh is the rightmost column of a
    // dominant match emitted so far and colThresh the bottommost row (both non-decreasing),
    // so a backward candidate (r, c) is emitted as dominant exactly when c > rowThresh
    // and r > colThresh.  matchesBelow indexes the bottom-most source strictly below the row,
    // and rightSources counts sources strictly right of the column.
    //
    int64_t rowThresh = (int64_t)column0 - 1;   // rightmost column of a dominant match
    int64_t colThresh = (int64_t)row0 - 1;      // bottommost row of a dominant match
    size_t matchesBelow = contour.size();       // first source below the current row
    size_t rightSources = 0;                    // # sources right of the current column

    //
    // Mirror of the forward idle-prefix skip: begin at the first diagonal that
    // can yield a match in either sweep.
    //
    auto startRowDiagonal = contour.empty() ? (size_t)0 : row1 - (size_t)contour.back().row;
    auto startColumnDiagonal = contour.empty() ? (size_t)0 : column1 - (size_t)contour[0].column;
    auto startDiagonal = min(min(startRowDiagonal, startColumnDiagonal), diagonalCount);
    auto startRow = row1 - 1 - startDiagonal;
    auto startColumn = column1 - 1 - startDiagonal;
    while (matchesBelow > 0 && contour[matchesBelow - 1].row > (uint32_t)startRow)
      matchesBelow--;
    while (rightSources < contour.size() && contour[rightSources].column > (uint32_t)startColumn)
      rightSources++;

    for (size_t diagonal = startDiagonal; diagonal < diagonalCount; diagonal++) {
      LCSRickSpace::Rows++;             // one row + one column sweep step
      auto rowIndex = row1 - 1 - diagonal;
      auto columnIndex = column1 - 1 - diagonal;

      //
      // Row sweep: backward horizontal matches in the diagonal row, at/left of the diagonal column.
      // A source below the row is needed unless the contour is empty (the rank-1 case).
      //
      while (matchesBelow > 0 && contour[matchesBelow - 1].row > (uint32_t)rowIndex)
        matchesBelow--;
      auto rowId = occ.rowId[(size_t)rowIndex];
      if (rowId != UINT32_MAX && (contour.empty() || matchesBelow < contour.size())) {
        bool active = true;
        auto upper = columnIndex;       // rank 1: rightmost occurrence at/left of the diagonal column
        if (matchesBelow < contour.size()) {
          auto bound = contour[matchesBelow].column;
          active = bound > column0;     // an occurrence must precede column0
          if (active)
            upper = min((size_t)(bound - 1), columnIndex);
        }
        if (active) {
          auto column = occ.prev[(size_t)rowId][upper];
          // Skip if a vertical (column-sweep) match at row >= rowIndex dominates it.
          if (column != UINT32_MAX && column > rowThresh && (int64_t)rowIndex > colThresh) {
            LCSRickSpace::Candidates++;
            next.push_back({ .row = (uint32_t)rowIndex, .column = column });
            rowThresh = column;
          }
        }
      }

      //
      // Column sweep: backward vertical matches in the diagonal column, above the diagonal row.
      // A source right of the column is needed unless the contour is empty (rank-1 case).
      //
      while (rightSources < contour.size() && contour[rightSources].column > (uint32_t)columnIndex)
        rightSources++;
      auto colId = occ.colId[(size_t)columnIndex];
      if (colId != UINT32_MAX && (contour.empty() || rightSources > 0) && rowIndex > row0) {
        bool active = true;
        auto upper = (int64_t)rowIndex - 1;     // rank 1: bottommost occurrence above the diagonal row
        if (rightSources > 0) {
          auto bound = contour[rightSources - 1].row;
          active = bound > row0;
          if (active)
            upper = min((int64_t)bound - 1, (int64_t)rowIndex - 1);
        }
        if (active && upper >= (int64_t)row0) {
          auto row = occ.prev1[(size_t)colId][(size_t)upper];
          // Skip if a horizontal (row-sweep) match at column >= columnIndex dominates it.
          if (row != UINT32_MAX && row > colThresh && (int64_t)columnIndex > rowThresh) {
            LCSRickSpace::Candidates++;
            next.push_back({ .row = row, .column = (uint32_t)columnIndex });
            colThresh = row;
          }
        }
      }

      // Completeness (mirror image).
      if (rowThresh >= (int64_t)columnIndex || colThresh >= (int64_t)rowIndex)
        break;
    }

    sort(
      next.begin(), next.end(), [](const Match& left, const Match& right) {
        return left.row < right.row;
      });
    return next;
  }

  //
  // Two contours have crossed when no backward match is strictly bottom-right
  // of any forward match (Rick, Lemma 3).
  //
  bool Crossed(const Contour& forward, const Contour& backward) {
    //
    // The crossing test is the same merged scan as CutForward, but it can stop
    // at the first forward match that still has a backward match below-right.
    //
    size_t backwardIndex = 0;
    for (const auto& match : forward) {
      while (backwardIndex < backward.size() && backward[backwardIndex].row <= match.row)
        backwardIndex++;
      if (backwardIndex < backward.size() && backward[backwardIndex].column > match.column)
        return false;                   // Still crossing
    }
    return true;
  }

  //
  // After the contours have crossed, a midpoint is any forward match for which
  // some backward match lies non-strictly bottom-right of it.
  //
  Match Midpoint(const Contour& forward, const Contour& backward) {
    //
    // Merged scan: for each forward match, the first backward match with
    // row >= match.row has the largest column of any backward match non-strictly
    // below-right of it; it decides whether the match is a midpoint.
    //
    size_t backwardIndex = 0;
    for (const auto& match : forward) {
      while (backwardIndex < backward.size() && backward[backwardIndex].row < match.row)
        backwardIndex++;
      if (backwardIndex < backward.size() && backward[backwardIndex].column >= match.column)
        return match;
    }
    return { .row = UINT32_MAX, .column = UINT32_MAX };
  }
}

uint64_t LCSRickSpace::Candidates = 0;
uint64_t LCSRickSpace::Rows = 0;
uint64_t LCSRickSpace::Contours = 0;

void LCSRickSpace::ResetCounters() {
  Candidates = 0;
  Rows = 0;
  Contours = 0;
}

uint32_t LCSRickSpace::Find(
  shared_ptr<Pair>* pairs, const MATCHES& matches, uint32_t size1, uint32_t size2) {
  return Solve(pairs, BuildOccurrences(matches, size1, size2), size1, size2);
}

uint32_t LCSRickSpace::Find(shared_ptr<Pair>* pairs,
  const u32string& s1, const u32string& s2) {
  //
  // Build O(ns) occurrence tables directly from the code points and amortize them
  // over the whole Hirschberg recursion.  No match lists are ever materialized,
  // so this path stays feasible for very long strings.
  //
  return Solve(pairs, BuildOccurrences(s1, s2), s1.size(), s2.size());
}
