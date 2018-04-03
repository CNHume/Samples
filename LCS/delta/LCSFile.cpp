// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-07-09 CNHume  Created LCSFile subclass
//
#include "LCSFile.h"

void LCSFile::Difference(const Command command) {
  auto r1 = Read(command.f1);
  auto r2 = Read(command.f2);
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
LCSFile::RECORDS LCSFile::Read(const string& filename) {
  ifstream input;
  input.open(filename, ios::in);

  if (input.fail()) {
    string msg(filename + " not found");
    throw runtime_error(msg);
  }

  RECORDS records;
  string buffer;
  while (getline(input, buffer))
    records.push_back(buffer);

  input.close();
#ifdef SHOW_COUNTS
  cout << records.size() << " records read from " << filename << endl;
#endif
  return records;
}
