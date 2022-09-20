// Copyright (C) 2017-2022, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#include "LCSFile.h"

void LCSFile::Correspondence(const Command command) {
  auto r1 = Read(command.f1, command.isword);
  auto r2 = Read(command.f2, command.isword);
  auto size1 = r1.size();           // empty final delta
  auto size2 = r2.size();

  auto intervals = Compare(r1, r2, command.ignorecase, command.ignorespace);
#ifdef SHOW_DELTAS
  Delta::List(intervals);
#endif
  Delta::Context(intervals, size1, size2, command.prefix, command.suffix);
  Show(intervals, r1, r2, command.f1, command.f2);
}

void LCSFile::Difference(const Command command) {
  auto r1 = Read(command.f1, command.isword);
  auto r2 = Read(command.f2, command.isword);
  auto intervals = Compare(r1, r2, command.ignorecase, command.ignorespace);

  auto size1 = r1.size();           // empty final delta
  auto size2 = r2.size();
  auto deltas = Delta::Complement(intervals, size1, size2);
#ifdef SHOW_DELTAS
  Delta::List(deltas);
#endif
  Delta::Context(deltas, size1, size2, command.prefix, command.suffix);
  auto joins = command.isjoin ? Delta::Coalesce(deltas, command.join) : deltas;
  Show(joins, r1, r2, command.f1, command.f2);
}

//
// file reader
//
LCSRecord::RECORDS LCSFile::Read(const string& filename, bool isword) {
  ifstream input;
  input.open(filename, ios::in);

  if (input.fail()) {
    string msg(filename + " not found");
    throw runtime_error(msg);
  }

  RECORDS records;
  string buffer;
  auto count = 0;
  while (getline(input, buffer)) {
    auto record = buffer;
    if (count == 0) {
      auto [encoding, length] = GetEncoding(buffer);
      record = &buffer[length];
    }

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
#ifdef SHOW_COUNTS
  cout << records.size() << " records read from " << filename << endl;
#endif
  return records;
}

tuple<LCSFile::Encoding, int> LCSFile::GetEncoding(const string& buffer) {
  int index = 0;
  for (const auto& bom : BOM) {
    int length = 0;
    for (const auto& bom_uch : bom) {
      auto buffer_uch = (unsigned char)buffer[length++];
      if (buffer_uch != bom_uch)
        goto next;
    }    
    return { encodings[index], length };

  next:
    index++;
  }
  return { encodings[index], 0 };
}

//
// Byte Order Mark (BOM)
// See https://en.wikipedia.org/wiki/Byte_order_mark
//
const vector<vector<unsigned char>> LCSFile::BOM = {
  { 0x84, 0x31, 0x95, 0x33 },           // GB18030
  { 0xFB, 0xEE, 0x28 },                 // BOCU1
  { 0x0E, 0xFE, 0xFF },                 // SCSU
  { 0xDD, 0x73, 0x66, 0x73 },           // UTF_EBCDIC
  { 0xF7, 0x64, 0x4C},                  // UTF1
  { 0x2B, 0x2F, 0x76 },                 // UTF7 [Obsolete]
  { 0x00, 0x00, 0xFE, 0xFF },           // UTF32_LE
  { 0xFF, 0xFE, 0x00, 0x00 },           // UTF32_BE
  { 0xFF, 0xFE },                       // UTF16_LE (b[2] > 0 || b[3] > 0)
  { 0xFE, 0xFF },                       // UTF16_BE
  { 0xEF, 0xBB, 0xBF }                  // UTF8_BOM
};

const vector<LCSFile::Encoding> LCSFile::encodings = {
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
