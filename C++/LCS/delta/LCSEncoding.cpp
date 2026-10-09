// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-07 CNHume  Created LCSEncoding class
//
#include "LCSEncoding.h"

tuple<LCSEncoding::Encoding, int> LCSEncoding::GetEncoding(const string& buffer) {
  for (int index = 0; index < (int)BOM.size(); index++) {
    const auto& bom = BOM[index];
    if (bom.size() > buffer.size())
      continue;                         // Buffer too short for this BOM.

    auto matched = true;
    for (int length = 0; length < (int)bom.size(); length++) {
      if ((unsigned char)buffer[length] != bom[length]) {
        matched = false;
        break;
      }
    }
    if (matched)
      return { encodings[index], (int)bom.size() };
  }
  return { encodings.back(), 0 };       // ANSI: no BOM matched
}

bool LCSEncoding::IsWide(Encoding encoding) {
  return encoding == UTF16_LE || encoding == UTF16_BE;
}

//
// Byte Order Mark (BOM)
// See https://en.wikipedia.org/wiki/Byte_order_mark
//
const vector<vector<unsigned char>> LCSEncoding::BOM = {
  { 0x84, 0x31, 0x95, 0x33 },           // GB18030
  { 0xFB, 0xEE, 0x28 },                 // BOCU1
  { 0x0E, 0xFE, 0xFF },                 // SCSU
  { 0xDD, 0x73, 0x66, 0x73 },           // UTF_EBCDIC
  { 0xF7, 0x64, 0x4C },                 // UTF1
  { 0x2B, 0x2F, 0x76 },                 // UTF7 [Obsolete]
  { 0x00, 0x00, 0xFE, 0xFF },           // UTF32_LE
  { 0xFF, 0xFE, 0x00, 0x00 },           // UTF32_BE
  { 0xFF, 0xFE },                       // UTF16_LE (b[2] > 0 || b[3] > 0)
  { 0xFE, 0xFF },                       // UTF16_BE
  { 0xEF, 0xBB, 0xBF }                  // UTF8_BOM
};

const vector<LCSEncoding::Encoding> LCSEncoding::encodings = {
  GB18030,
  BOCU1,
  SCSU,
  UTF_EBCDIC,
  UTF1,
  UTF7,
  UTF32_LE,
  UTF32_BE,
  UTF16_LE,
  UTF16_BE,
  UTF8_BOM,
  ANSI
};
