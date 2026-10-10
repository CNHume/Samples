// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2026-10-07 CNHume  Created LCSEncoding class
//
#include "LCSEncoding.h"

#include <cstdint>                      // for uint32_t
#include <format>                       // for format
#include <stdexcept>                    // for runtime_error

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
  return encoding == UTF16_LE || encoding == UTF16_BE ||
         encoding == UTF32_LE || encoding == UTF32_BE;
}

string LCSEncoding::ToUtf8(const u32string& codePoints) {
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

tuple<LCSEncoding::Encoding, int> LCSEncoding::PeekEncoding(const string& filename) {
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

vector<string> LCSEncoding::ReadWide(
  const string& filename, Encoding encoding, bool isword) {
  string bytes = ReadBytes(filename);

  auto codePoints = DecodeCodePoints(bytes, encoding);

  // Split into records: lines on '\n' (stripping a trailing '\r'), then
  // optionally words on ASCII whitespace within each line.
  vector<string> records;
  u32string line;
  auto flushLine = [&]() {
    if (!line.empty() && line.back() == U'\r')
      line.pop_back();
    if (isword) {
      u32string word;
      auto flushWord = [&]() {
        if (!word.empty())
          records.push_back(ToUtf8(word));
        word.clear();
      };
      for (auto cp : line) {
        if (cp == U' ' || cp == U'\t' || cp == U'\v' || cp == U'\f' || cp == U'\r')
          flushWord();
        else
          word.push_back(cp);
      }
      flushWord();
    }
    else
      records.push_back(ToUtf8(line));
    line.clear();
  };

  for (auto cp : codePoints) {
    if (cp == U'\n')
      flushLine();
    else
      line.push_back(cp);
  }
  if (!line.empty())
    flushLine();

  return records;
}

string LCSEncoding::ReadBytes(const string& filename) {
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

u32string LCSEncoding::DecodeCodePoints(
  const string& bytes, Encoding encoding) {
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

u32string LCSEncoding::DecodeUtf8(const string& bytes) {
  u32string codePoints;
  auto cont = [&](size_t k) {
    return (unsigned char)bytes[k] >= 0x80 && (unsigned char)bytes[k] < 0xC0;
  };
  for (size_t i = 0; i < bytes.size();) {
    auto b0 = (unsigned char)bytes[i];
    if (b0 < 0x80) {
      codePoints.push_back((char32_t)b0);
      i++;
    }
    else if (b0 < 0xC0) {
      // Stray continuation byte: treat as Latin-1.
      codePoints.push_back((char32_t)b0);
      i++;
    }
    else if (b0 < 0xE0) {
      if (i + 1 < bytes.size() && cont(i + 1)) {
        codePoints.push_back((char32_t)(
          ((b0 & 0x1Fu) << 6) | ((unsigned char)bytes[i + 1] & 0x3Fu)));
        i += 2;
      }
      else { codePoints.push_back((char32_t)b0); i++; }
    }
    else if (b0 < 0xF0) {
      if (i + 2 < bytes.size() && cont(i + 1) && cont(i + 2)) {
        codePoints.push_back((char32_t)(
          ((b0 & 0x0Fu) << 12) |
          (((unsigned char)bytes[i + 1] & 0x3Fu) << 6) |
          ((unsigned char)bytes[i + 2] & 0x3Fu)));
        i += 3;
      }
      else { codePoints.push_back((char32_t)b0); i++; }
    }
    else if (b0 < 0xF5) {
      if (i + 3 < bytes.size() && cont(i + 1) && cont(i + 2) && cont(i + 3)) {
        codePoints.push_back((char32_t)(
          ((b0 & 0x07u) << 18) |
          (((unsigned char)bytes[i + 1] & 0x3Fu) << 12) |
          (((unsigned char)bytes[i + 2] & 0x3Fu) << 6) |
          ((unsigned char)bytes[i + 3] & 0x3Fu)));
        i += 4;
      }
      else { codePoints.push_back((char32_t)b0); i++; }
    }
    else {
      // Invalid lead byte: treat as Latin-1.
      codePoints.push_back((char32_t)b0);
      i++;
    }
  }
  return codePoints;
}

u32string LCSEncoding::NormalizeCrlf(const u32string& codePoints) {
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
const vector<vector<unsigned char>> LCSEncoding::BOM = {
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
