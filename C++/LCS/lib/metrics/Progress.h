// Copyright (C) 2026, Christopher N. Hume.  All rights reserved.
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
// 2026-10-30 CNHume  Created Progress class
//
#pragma once

namespace lcs::metrics {

  //
  // Progress reports the fraction [0, 1] of an operation that has completed.
  // An algorithm calls Update() as it advances; a concrete renderer (such as
  // ProgressBar) supplies the display.
  //
  class Progress {
  public:
    virtual ~Progress() = default;
    virtual void Update(double fraction) = 0;
    //
    // Finish() is called once when progress reporting ends, before any results
    // are printed, so a renderer can erase the display it has drawn.
    //
    virtual void Finish() {}
  };

} // namespace lcs::metrics
