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

#include <string>
#include <tuple>
#include <vector>

using namespace std;

class LCSEncoding {
public:
  enum Encoding {
    ANSI, UTF8_BOM, UTF16_BE, UTF16_LE, UTF32_BE, UTF32_LE, UTF7, UTF1, UTF_EBCDIC, SCSU, BOCU1, GB18030
  };

  static tuple<Encoding, int> GetEncoding(const string& buffer);
  static bool IsWide(Encoding encoding);
  static string ToUtf8(const u32string& codePoints);
  static tuple<Encoding, int> PeekEncoding(const string& filename);
  static vector<string> ReadWide(
    const string& filename, Encoding encoding, bool isword);
  static string ReadBytes(const string& filename);
  static u32string DecodeCodePoints(const string& bytes, Encoding encoding);

  const static vector<vector<unsigned char>> BOM;
  const static vector<Encoding> encodings;
};
