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
// 2026-10-26 CNHume  Created Cigar class
//
#include "formatters/Cigar.h"

#include <format>                       //[C++20]
#include <iostream>                     // for cout
#include <stdexcept>                    // for runtime_error

using namespace std;
using namespace lcs;

namespace lcs::formatters {

void Cigar::BeginResult(
  const string&, const string&, uint32_t size1, uint32_t size2) {
  size1_ = size1;
  size2_ = size2;
  prevEnd1_ = 0;
  reference_ = 0;
  query_ = 0;
  cigar_.clear();
}

void Cigar::Layout(
  uint32_t, const SideSpan& side1, const SideSpan& side2, EditKind) {
  // The match preceding this delta.
  auto match = side1.begin - prevEnd1_;
  if (match > 0) {
    cigar_ += format("{}M", match);
    reference_ += match;
    query_ += match;
  }
  // Deletions (reference-only), then insertions (query-only).
  auto del = side1.end - side1.begin;
  if (del > 0) {
    cigar_ += format("{}D", del);
    reference_ += del;
  }
  auto ins = side2.end - side2.begin;
  if (ins > 0) {
    cigar_ += format("{}I", ins);
    query_ += ins;
  }
  prevEnd1_ = side1.end;
}

void Cigar::EndResult() {
  // The match following the last delta.
  auto match = size1_ - prevEnd1_;
  if (match > 0) {
    cigar_ += format("{}M", match);
    reference_ += match;
    query_ += match;
  }
  //
  // The trace must account for exactly the reference and query lengths.
  //
  if (reference_ != size1_ || query_ != size2_)
    throw runtime_error("CIGAR trace does not match the input lengths");
  cout << cigar_ << '\n';
}

} // namespace lcs::formatters
