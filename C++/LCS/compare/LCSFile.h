// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#pragma once

#include "Command.h"
#include "LCSFormat.h"
#include <fstream>
#include <sstream>
#include <iterator>

using namespace std;

class LCSFile : protected LCSFormat {
public:
  enum Encoding {
    ANSI, UTF8_BOM, UTF16_BE, UTF16_LE, UTF32_BE, UTF32_LE, UTF7, UTF1, UTF_EBCDIC, SCSU, BOCU1, GB18030
  };

  void Correspondence(const Command command);
  void Difference(const Command command);

  const static vector<vector<unsigned char>> BOM;
  const static vector<Encoding> encodings;

protected:
  static RECORDS Read(const string& filename, bool isword);
  static pair<Encoding, int> GetEncoding(const string& buffer);
};
