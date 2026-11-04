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
// 2026-10-30 CNHume  Created ProgressBar class
//
#pragma once

#include "metrics/Progress.h"
#include "metrics/Stopwatch.h"

#include <cstddef>                      // for size_t

namespace lcs::metrics {

  //
  // ProgressBar renders a simple ASCII progress bar to stderr, throttled to
  // progressUpdateMilliseconds between updates.
  //
  class ProgressBar : public Progress {
  public:
    ProgressBar();
    ~ProgressBar() override;

    void Update(double fraction) override;
    void Finish() override;

  private:
    void Draw(double fraction);

    Stopwatch stopwatch_;               // throttles redraws
    double latest_ = 0.0;
    size_t drawnLength_ = 0;            // visible length of the last bar line
  };

} // namespace lcs::metrics
