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
// 2026-10-20 CNHume  Created Statistics class
//
#pragma once

#include "Duration.h"                   // for Duration::ToString
#include "LCS.h"                        // for LCS::Algorithm

#include <cstdint>                      // for uint64_t
#include <iostream>                     // for cout

namespace lcs {

  //
  // Statistics carries the work metrics of one LCS run, assessed by the algorithm
  // that produced it.  Each algorithm records its own counters here, maintained
  // alongside each other; Operations() returns the salient work count,
  // AlgorithmName() the algorithm's name, and ShowDuration() reports the elapsed
  // time — all derived from the recorded values.
  //
  class Statistics {
  public:
    LCS::Algorithm algorithm = LCS::Algorithm::HuntSzymanski;

    std::uint64_t matches = 0;            // r: Hunt-Szymanski match-list magnitude
    std::uint64_t records = 0;            // d: RickTime dominant matches recorded
    std::uint64_t candidates = 0;         // RickSpace dominant matches emitted
    std::uint64_t rows = 0;               // RickSpace diagonal sweep steps
    std::uint64_t contours = 0;           // RickSpace contour sweeps
    std::uint64_t elapsedMicroseconds = 0;// Compare() elapsed time

    // The salient work count for the algorithm that produced these statistics.
    std::uint64_t Operations() const {
      switch (algorithm) {
      case LCS::Algorithm::RickTime:
        return records;
      case LCS::Algorithm::RickSpace:
        return candidates;
      default:                            // Hunt-Szymanski
        return matches;
      }
    }

    // A human-readable algorithm name.
    const char* AlgorithmName() const {
      switch (algorithm) {
      case LCS::Algorithm::HuntSzymanski:
        return "Hunt-Szymanski";
      case LCS::Algorithm::RickSpace:
        return "RickSpace";
      case LCS::Algorithm::RickTime:
        return "RickTime";
      default:
        return "Unknown";
      }
    }

    // Report the elapsed time as a duration line.
    void ShowDuration() const {
      std::cout << Duration::ToString(
        AlgorithmName(), (long)Operations(), (std::int64_t)elapsedMicroseconds);
    }
  };

} // namespace lcs
