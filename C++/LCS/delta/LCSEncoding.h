// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-07 CNHume  Created LCSEncoding class
//
#pragma once

#include <string>
#include <vector>

using namespace std;

class LCSEncoding {
public:
  enum Encoding {
    ANSI, UTF8_BOM, UTF16_BE, UTF16_LE, UTF32_BE, UTF32_LE, UTF7, UTF1, UTF_EBCDIC, SCSU, BOCU1, GB18030
  };

  static tuple<Encoding, int> GetEncoding(const string& buffer);

  const static vector<vector<unsigned char>> BOM;
  const static vector<Encoding> encodings;
};
