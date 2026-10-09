// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Moved Command overloads to LCSFile subclass
// 2017-07-04 CNHume  Created LCSRecord subclass
// 2015-01-19 CNHume  Created file
//
#include "LCSRecord.h"

//
// Encoding helpers
//
namespace {

  // Swap the byte order of a 16-bit code unit (Windows wchar_t).
  wchar_t ByteSwap(wchar_t wc) {
    return (wchar_t)(((wc & 0xFF00u) >> 8) | ((wc & 0x00FFu) << 8));
  }

  // Encode a wide string as UTF-8, combining surrogate pairs.
  string ToUtf8(const wstring& ws) {
    string utf8;
    for (size_t i = 0; i < ws.size(); i++) {
      auto cp = (uint32_t)ws[i];
      if (cp >= 0xD800 && cp <= 0xDBFF && i + 1 < ws.size()) {
        auto lo = (uint32_t)ws[i + 1];
        if (lo >= 0xDC00 && lo <= 0xDFFF) {
          cp = 0x10000u + ((cp - 0xD800u) << 10) + (lo - 0xDC00u);
          i++;
        }
      }
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

  // Peek the leading bytes to detect a Byte Order Mark (BOM).
  tuple<LCSEncoding::Encoding, int> PeekEncoding(const string& filename) {
    ifstream input(filename, ios::binary);
    if (input.fail()) {
      string msg(format("{} not found", filename));
      throw runtime_error(msg);
    }
    char head[4] = {};
    input.read(head, 4);
    string peek(head, input.gcount());
    return LCSEncoding::GetEncoding(peek);
  }

  // Read and decode a UTF-16 file.
  LCSRecord::RECORDS ReadWide(
    const string& filename, LCSEncoding::Encoding encoding, bool isword) {
    ifstream input(filename, ios::binary);
    if (input.fail()) {
      string msg(format("{} not found", filename));
      throw runtime_error(msg);
    }
    auto bigEndian = encoding == LCSEncoding::UTF16_BE;

    // Read the whole file as raw bytes, then decode UTF-16 code units.
    string bytes((istreambuf_iterator<char>(input)), istreambuf_iterator<char>());
    input.close();
    wstring all;
    all.reserve(bytes.size() / 2);
    for (size_t i = 0; i + 1 < bytes.size(); i += 2) {
      auto unit = (wchar_t)((unsigned char)bytes[i] | ((unsigned char)bytes[i + 1] << 8));
      all.push_back(bigEndian ? ByteSwap(unit) : unit);
    }
    if (!all.empty())
      all.erase(0, 1);                  // Strip the UTF-16 BOM (one code unit).

    LCSRecord::RECORDS records;
    size_t pos = 0;
    for (;;) {
      auto nl = all.find(L'\n', pos);
      auto end = nl == wstring::npos ? all.size() : nl;
      if (nl != wstring::npos || end > pos) {   // Skip trailing empty line at EOF.
        wstring record = all.substr(pos, end - pos);
        if (!record.empty() && record.back() == L'\r')
          record.pop_back();

        if (isword) {
          wistringstream wiss(record);
          wstring token;
          while (!wiss.eof()) {
            wiss >> token;
            records.push_back(ToUtf8(token));
          }
        }
        else
          records.push_back(ToUtf8(record));
      }
      if (nl == wstring::npos)
        break;
      pos = nl + 1;
    }
    return records;
  }

} // anonymous namespace

//
// Find Matches
//
// Match() avoids m*n comparisons by using STRING_TO_INDEXES_MAP to
// achieve O(m+n) performance, where m and n are the input lengths.
//
// The lookup time can be assumed constant in the case of characters.
// The symbol space is larger in the case of records; but the lookup
// time will be O(log(m+n)), at most.
//
uint32_t LCSRecord::Match(
  MATCHES& indexesOf2MatchedByIndex1,
  STRING_TO_INDEXES_MAP& indexesOf2MatchedByString,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  uint32_t count = 0;
  uint32_t index = 0;
  string buffer;
  for (const auto& it : r2) {
    LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
    indexesOf2MatchedByString[buffer].push_back(index++);
  }

  for (const auto& it : r1) {
    LCSNormal::Normal(it, buffer, ignorecase, ignorespace);
    auto& dq2 = indexesOf2MatchedByString[buffer];
    indexesOf2MatchedByIndex1.push_back(&dq2);
    count += dq2.size();
  }
#ifdef SHOW_MATCH_COUNT
  cout << format("count = {} of indexesOf2MatchedByIndex1\n", count);
#endif
  return count;
}

uint32_t LCSRecord::Correspondence(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  Delta::Context(*intervals, size1, size2, prefix, suffix);
  if (isjoin)
    *intervals = Delta::Coalesce(*intervals, join);
  return length;
}

uint32_t LCSRecord::Difference(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace, bool isjoin,
  uint32_t join, uint32_t prefix, uint32_t suffix) {
  auto length = Compare(intervals, r1, r2, ignorecase, ignorespace);
  auto size1 = r1.size();               // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(*intervals, size1, size2);
  Delta::Context(deltas, size1, size2, prefix, suffix);
  *intervals = isjoin ? Delta::Coalesce(deltas, join) : deltas;
  return length;
}

uint32_t LCSRecord::Compare(shared_ptr<Delta>* intervals,
  const RECORDS& r1, const RECORDS& r2,
  bool ignorecase, bool ignorespace) {
  STRING_TO_INDEXES_MAP indexesOf2MatchedByString;
  MATCHES indexesOf2MatchedByIndex1;    // indexesOf2MatchedByIndex1 holds references into indexesOf2MatchedByString
  [[maybe_unused]] auto count = Match(indexesOf2MatchedByIndex1, indexesOf2MatchedByString, r1, r2, ignorecase, ignorespace);
  shared_ptr<Pair> pairs;
  auto ppairs = intervals != nullptr ? &pairs : nullptr;
  auto length = FindLCS(ppairs, indexesOf2MatchedByIndex1);
  if (intervals != nullptr)
    *intervals = Delta::Coalesce(pairs);
  return length;
}

//
// RECORDS Reader
//
LCSRecord::RECORDS LCSRecord::Read(const string& filename, bool isword) {
  auto [encoding, bomLength] = PeekEncoding(filename);

  if (encoding == LCSEncoding::UTF32_LE || encoding == LCSEncoding::UTF32_BE)
    throw runtime_error(format("{}: UTF-32 input is not yet supported", filename));

  if (LCSEncoding::IsWide(encoding))
    return ReadWide(filename, encoding, isword);

  ifstream input(filename, ios::binary);

  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }

  RECORDS records;
  string buffer;
  auto count = 0;
  while (getline(input, buffer)) {
    if (!buffer.empty() && buffer.back() == '\r')
      buffer.pop_back();                // Strip CRLF terminator.
    auto record = buffer;
    if (count == 0 && bomLength > 0)
      record = &buffer[bomLength];

    if (isword) {
      istringstream iss(record);
      string token;
      while (!iss.eof()) {
        iss >> token;
        records.push_back(token);
      }
    }
    else
      records.push_back(record);
    count++;
  }

  input.close();
#ifdef SHOW_MATCH_COUNT
  cout << format(
    "{} records read from {}\n",
    records.size(), filename);
#endif
  return records;
}
