// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-07 CNHume  Created LCSEncoding class
//
#pragma once
//
// Define KEEP_CRLF to suppress CRLF normalization in ReadCodePoints().
// The -t (text) mode then treats each file as one large string, leaving
// Windows (CRLF) vs. Linux (LF) end-of-line characters distinct.  They are
// non-printing, so -b (ignorespace) can be used to ignore them instead.
//
//#define KEEP_CRLF

#include <fstream>
#include <sstream>
#include <iterator>
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
  static u32string ReadCodePoints(const string& filename);

  const static vector<vector<unsigned char>> BOM;
  const static vector<Encoding> encodings;

private:
  static u32string DecodeCodePoints(const string& bytes, Encoding encoding);
  static u32string DecodeUtf8(const string& bytes);
  static u32string NormalizeCrlf(const u32string& codePoints);
};
