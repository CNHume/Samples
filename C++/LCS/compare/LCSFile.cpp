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
      auto pair = GetEncoding(buffer);
      auto encoding = pair.first;
      record = &buffer[pair.second];
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

pair<LCSFile::Encoding, int> LCSFile::GetEncoding(const string& buffer) {
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

const vector<vector<unsigned char>> LCSFile::BOM = {
  { 0x2b, 0x2f, 0x76 },                 // UTF7
  { 0x00, 0x00, 0xfe, 0xff },           // UTF32_LE
  { 0xff, 0xfe, 0x00, 0x00 },           // UTF32_BE
  { 0xff, 0xfe },                       // UTF16_LE (b[2] > 0 || b[3] > 0)
  { 0xfe, 0xff },                       // UTF16_BE
  { 0xef, 0xbb, 0xbf }                  // UTF8_BOM
};

const vector<LCSFile::Encoding> LCSFile::encodings = {
  UTF7,
  UTF32_LE,
  UTF32_BE,
  UTF16_LE,
  UTF16_BE,
  UTF8_BOM,
  ANSI
};
