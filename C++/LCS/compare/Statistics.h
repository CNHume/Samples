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

#include <cstdint>                      // for uint64_t

//
// Statistics carries the work metric of one LCS run, assessed by the algorithm
// that produced it.  Each algorithm decides which of its internal counters is
// the salient "operations" count and records it here, so the caller can report
// a per-algorithm effort without knowing the algorithm's internals.
//
class Statistics {
public:
  uint64_t operations = 0;              // the algorithm's salient work count
};
