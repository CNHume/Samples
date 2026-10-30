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
// 2026-10-24 CNHume  Delegate layout to Formatter subclasses
// 2022-07-04 CNHume  Created RecordDriver class
//
#pragma once

#include "modes/Record.h"
#include "formatters/Formatter.h"
#include "formatters/Edit.h"

#include <cstdint>                      // for uint32_t
#include <memory>                       // for shared_ptr
#include <string>                       // for string

namespace lcs::formatters {

  class RecordDriver : public modes::Record {
  public:
    static RECORDS Select(
      std::shared_ptr<Delta> deltas, const RECORDS& r1, const RECORDS& r2,
      bool isright = false);

    static std::uint32_t Show(
      Formatter& format, std::shared_ptr<Delta> deltas,
      const RECORDS& r1, const RECORDS& r2,
      const std::string& label1, const std::string& label2);

    static std::uint32_t ShowEdit(
      Edit& edit, const EDIT_INTERVALS& intervals,
      const RECORDS& r1, const RECORDS& r2,
      const std::string& label1, const std::string& label2);

  private:
    static SideSpan MakeSpan(
      const std::string& label, const RECORDS& records,
      std::uint32_t begin, std::uint32_t end);
  };

} // namespace lcs::formatters
