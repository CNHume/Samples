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
// 2026-11-01 CNHume  Created Formatter factory
//
#include "formatters/Formatter.h"

#include "formatters/Cigar.h"
#include "formatters/Edit.h"
#include "formatters/Parallel.h"
#include "formatters/Sequential.h"

#include <memory>                       // for make_unique
#include <stdexcept>                    // for runtime_error

namespace lcs::formatters {

  //
  // Build the formatter selected by the format specifier.
  //
  std::unique_ptr<Formatter> Formatter::Make(
    Format format, std::uint32_t width, std::uint32_t tabsize) {
    switch (format) {
    case Format::CIGAR:
      return std::make_unique<Cigar>();
    case Format::Edit:
      return std::make_unique<Edit>();
    case Format::Parallel:
      return std::make_unique<Parallel>(width, tabsize);
    case Format::Sequential:
      return std::make_unique<Sequential>();
    default:
      throw std::runtime_error("Unknown format");
    }
  }

} // namespace lcs::formatters
