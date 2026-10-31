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
// 2026-10-07 CNHume  Created Encoding class
//
#include "Encoding.h"
#include "Normal.h"

#include <cstdint>                      // for uint32_t
#include <format>                       // for format
#include <fstream>                      // for ifstream
#include <ranges>
#include <stdexcept>                    // for runtime_error

using namespace std;
using namespace lcs;

tuple<Encoding::Kind, int> Encoding::GetEncoding(const string& buffer) {
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

bool Encoding::IsWide(Kind encoding) {
  switch (encoding) {
  case UTF16_LE:
  case UTF16_BE:
  case UTF32_LE:
  case UTF32_BE:
    return true;
  default:
    return false;
  }
}

string Encoding::ToUtf8(const u32string& codePoints) {
  string utf8;
  for (auto cp : codePoints) {
    if (cp < 0x80)
      utf8.push_back((char)cp);
    else if (cp < 0x800) {
      utf8.push_back((char)(0xC0u | (cp >> 6)));
      utf8.push_back((char)(0x80u | (cp & 0x3Fu)));
    }
    else if (cp < 0x10000) {
      utf8.push_back((char)(0xE0u | (cp >> 12)));
      utf8.push_back((char)(0x80u | ((cp >> 6) & 0x3Fu)));
      utf8.push_back((char)(0x80u | (cp & 0x3Fu)));
    }
    else {
      utf8.push_back((char)(0xF0u | (cp >> 18)));
      utf8.push_back((char)(0x80u | ((cp >> 12) & 0x3Fu)));
      utf8.push_back((char)(0x80u | ((cp >> 6) & 0x3Fu)));
      utf8.push_back((char)(0x80u | (cp & 0x3Fu)));
    }
  }
  return utf8;
}

tuple<Encoding::Kind, int> Encoding::PeekEncoding(const string& filename) {
  ifstream input(filename, ios::binary);
  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }
  char head[4] = {};
  input.read(head, 4);
  string peek(head, (size_t)input.gcount());
  return GetEncoding(peek);
}

vector<string> Encoding::ReadWide(
  const string& filename, Kind encoding, bool isword) {
  string bytes = ReadBytes(filename);
  auto codePoints = DecodeCodePoints(bytes, encoding);
  // Split the entire input into views of lines.  std::views::split appends
  // an empty trailing view when the input ends with '\n'; that one is dropped
  // below, but genuine empty lines (leading or interior) are kept.
  auto endsWithNewline = !codePoints.empty() && codePoints.back() == U'\n';
  auto lines = codePoints | std::views::split(U'\n');
  std::vector<std::string> records;
  for (auto line_view : lines) {
    std::u32string line(line_view.begin(), line_view.end());

    // Remove trailing \r
    if (!line.empty() && line.back() == U'\r') {
      line.pop_back();
    }

    if (isword) {
      std::u32string word;
      for (auto cp : line) {
        if (Normal::IsSpace(cp)) {
          // Only push non-empty words
          if (!word.empty()) {
            records.push_back(ToUtf8(word));
            word.clear();
          }
        }
        else
          word.push_back(cp);
      }
      if (!word.empty()) {
        records.push_back(ToUtf8(word));
      }
    }
    else
      records.push_back(ToUtf8(line));
  }

  // Drop the single trailing empty record a trailing '\n' produces.
  if (!isword && endsWithNewline && !records.empty())
    records.pop_back();

  return records;
}

string Encoding::ReadBytes(const string& filename) {
  // Open at the end to learn the file size, then rewind and read in a single
  // call.  This avoids the per-byte growth of the istreambuf_iterator.
  ifstream input(filename, ios::binary | ios::ate);
  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }
  streamsize size = input.tellg();
  if (size < 0) {
    string msg(format("{} is not seekable", filename));
    throw runtime_error(msg);
  }
  input.seekg(0, ios::beg);

  string bytes;
  bytes.resize((size_t)size);
  if (size > 0)
    input.read(bytes.data(), size);
  return bytes;
}

u32string Encoding::DecodeCodePoints(
  const string& bytes, Kind encoding) {
  auto is32 = encoding == UTF32_LE || encoding == UTF32_BE;
  auto bigEndian = encoding == UTF16_BE || encoding == UTF32_BE;
  auto width = is32 ? 4 : 2;

  // Decode code units (skipping the BOM) into Unicode code points.  Code
  // points are used internally rather than wchar_t, whose width differs
  // between Windows (16-bit) and Linux (32-bit).
  vector<uint32_t> units;
  for (size_t i = width; i + width <= bytes.size(); i += width) {
    uint32_t unit = 0;
    if (bigEndian)
      for (auto k = 0; k < width; k++)
        unit = (unit << 8) | (uint32_t)(unsigned char)bytes[i + k];
    else
      for (auto k = 0; k < width; k++)
        unit |= (uint32_t)(unsigned char)bytes[i + k] << (8 * k);
    units.push_back(unit);
  }

  u32string codePoints;
  if (is32) {
    for (auto unit : units)
      if (unit <= 0x10FFFF && !(unit >= 0xD800 && unit <= 0xDFFF))
        codePoints.push_back((char32_t)unit);
  }
  else {
    for (size_t i = 0; i < units.size(); i++) {
      auto unit = units[i];
      if (unit >= 0xD800 && unit <= 0xDBFF && i + 1 < units.size()) {
        auto lo = units[i + 1];
        if (lo >= 0xDC00 && lo <= 0xDFFF) {
          codePoints.push_back((char32_t)(0x10000u +
            ((unit - 0xD800u) << 10) + (lo - 0xDC00u)));
          i++;
          continue;
        }
      }
      codePoints.push_back((char32_t)unit);
    }
  }
  return codePoints;
}

//
// A UTF-8 continuation byte has the form 10xxxxxx.
//
static bool isContinuation(unsigned char c) {
  return (c & 0xC0) == 0x80;
}

u32string Encoding::DecodeUtf8(const string& bytes) {
  u32string codePoints;
  for (size_t i = 0; i < bytes.size();) {
    auto b0 = (unsigned char)bytes[i];

    // The lead byte gives the sequence length and payload bits; a length
    // of one covers ASCII, stray continuation bytes, and invalid lead
    // bytes, each of which is its own code point.
    size_t length = 1;
    uint32_t codePoint = b0;
    if (b0 >= 0xE0 && b0 < 0xF0) { length = 3; codePoint = b0 & 0x0Fu; }
    else if (b0 >= 0xF0 && b0 < 0xF5) { length = 4; codePoint = b0 & 0x07u; }
    else if (b0 >= 0xC0 && b0 < 0xE0) { length = 2; codePoint = b0 & 0x1Fu; }

    if (length == 1) {
      codePoints.push_back((char32_t)codePoint);
      i++;
      continue;
    }

    // Complete the code point from the continuation bytes; fall back to
    // a one-byte code point when the sequence is incomplete or malformed.
    auto valid = i + length <= bytes.size();
    for (size_t k = 1; valid && k < length; k++) {
      auto c = (unsigned char)bytes[i + k];
      if (!isContinuation(c)) {
        valid = false;
        break;
      }
      codePoint = (codePoint << 6) | (c & 0x3Fu);
    }

    if (valid) {
      codePoints.push_back((char32_t)codePoint);
      i += length;
    }
    else {
      codePoints.push_back((char32_t)b0);
      i++;
    }
  }
  return codePoints;
}

u32string Encoding::NormalizeCrlf(const u32string& codePoints) {
  u32string normalized;
  normalized.reserve(codePoints.size());
  for (size_t i = 0; i < codePoints.size(); i++) {
    if (codePoints[i] == U'\r' &&
        i + 1 < codePoints.size() && codePoints[i + 1] == U'\n')
      continue;                         // Drop the CR of a CRLF pair.
    normalized.push_back(codePoints[i]);
  }
  return normalized;
}

//
// Byte Order Mark (BOM)
// See https://en.wikipedia.org/wiki/Byte_order_mark
//
const vector<vector<unsigned char>> Encoding::BOM = {
  { 0x84, 0x31, 0x95, 0x33 },           // GB18030
  { 0xFB, 0xEE, 0x28 },                 // BOCU1
  { 0x0E, 0xFE, 0xFF },                 // SCSU
  { 0xDD, 0x73, 0x66, 0x73 },           // UTF_EBCDIC
  { 0xF7, 0x64, 0x4C },                 // UTF1
  { 0x2B, 0x2F, 0x76 },                 // UTF7 [Obsolete]
  { 0xFF, 0xFE, 0x00, 0x00 },           // UTF32_LE
  { 0x00, 0x00, 0xFE, 0xFF },           // UTF32_BE
  { 0xFF, 0xFE },                       // UTF16_LE (b[2] > 0 || b[3] > 0)
  { 0xFE, 0xFF },                       // UTF16_BE
  { 0xEF, 0xBB, 0xBF }                  // UTF8_BOM
};

const vector<Encoding::Kind> Encoding::encodings = {
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
