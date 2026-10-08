// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// 2018-05-11 CNHume  Added word switch
// 2017-07-09 CNHume  Created LCSFile subclass
//
#include "LCSFile.h"

uint32_t  LCSFile::Correspondence(
  shared_ptr<Delta>* intervals, const Command command) {
  auto r1 = Read(command.file1, command.isword);
  auto r2 = Read(command.file2, command.isword);
  auto length = LCSRecord::Correspondence(intervals, r1, r2,
    command.ignorecase, command.ignorespace, command.isjoin,
    command.join, command.prefix, command.suffix);
  Show(*intervals, r1, r2, command.file1, command.file2);
  return length;
}

uint32_t  LCSFile::Difference(
  shared_ptr<Delta>* intervals, const Command command) {
  auto r1 = Read(command.file1, command.isword);
  auto r2 = Read(command.file2, command.isword);
  auto length = LCSRecord::Difference(intervals, r1, r2,
    command.ignorecase, command.ignorespace, command.isjoin,
    command.join, command.prefix, command.suffix);
  Show(*intervals, r1, r2, command.file1, command.file2);
  return length;
}

//
// RECORDS Reader
//
LCSRecord::RECORDS LCSFile::Read(const string& filename, bool isword) {
  ifstream input;
  input.open(filename, ios::in);

  if (input.fail()) {
    string msg(format("{} not found", filename));
    throw runtime_error(msg);
  }

  RECORDS records;
  string buffer;
  auto count = 0;
  while (getline(input, buffer)) {
    auto record = buffer;
    if (count == 0) {
      auto [encoding, length] = LCSEncoding::GetEncoding(buffer);
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
#ifdef SHOW_MATCH_COUNT
  cout << format(
    "{} records read from {}\n",
    records.size(), filename);
#endif
  return records;
}
