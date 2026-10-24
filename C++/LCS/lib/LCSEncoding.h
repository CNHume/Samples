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
// 2026-10-07 CNHume  Created LCSEncoding class
//
#pragma once
//#define KEEP_CRLF                     // Suppress CRLF normalization in LCSText::Read()
// If KEEP_CRLF is defined -t istext treats files as one long string,
// leaving Windows (CRLF) vs. Linux (LF) end-of-line distinct.
// -b ignorespace can be used to ignore them.

#include <string>
#include <tuple>
#include <vector>

namespace lcs {

class LCSEncoding {
public:
  enum Encoding {
    ANSI, UTF8_BOM, UTF16_BE, UTF16_LE, UTF32_BE, UTF32_LE, UTF7, UTF1, UTF_EBCDIC, SCSU, BOCU1, GB18030
  };

  static std::tuple<Encoding, int> GetEncoding(const std::string& buffer);
  static bool IsWide(Encoding encoding);
  static std::string ToUtf8(const std::u32string& codePoints);
  static std::tuple<Encoding, int> PeekEncoding(const std::string& filename);
  static std::vector<std::string> ReadWide(
    const std::string& filename, Encoding encoding, bool isword);
  static std::string ReadBytes(const std::string& filename);
  static std::u32string DecodeCodePoints(const std::string& bytes, Encoding encoding);
  static std::u32string DecodeUtf8(const std::string& bytes);
  static std::u32string NormalizeCrlf(const std::u32string& codePoints);

  const static std::vector<std::vector<unsigned char>> BOM;
  const static std::vector<Encoding> encodings;
};

} // namespace lcs
