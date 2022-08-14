// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-05-16 CNHume  Added Processor class
//
#define TIME_READ

#include "Processor.h"

//
// case normalization
//
void Processor::normalize(string& buffer) {
  for (auto& c : buffer) {
    if (isalpha(c))
      c = tolower(c);                   // Ensure input range
    else {
      string msg("Invalid character: ");
      throw domain_error(msg + c);
    }
  }
}

//
// file reader
//
Tree::STRINGSET Processor::read(const string& filename) {
  ifstream input;
  input.open(filename, ios::in);

  if (input.fail()) {
    string msg(filename + " not found");
    throw runtime_error(msg);
  }

  Tree::STRINGSET words;
  string buffer;
  while (input >> buffer) {
    normalize(buffer);
    words.insert(buffer);
  }

  input.close();
  return words;
}

void Processor::Execute(Command commmand) {
#ifdef TIME_READ
  auto read_t0 = high_resolution_clock::now();
#endif
  auto words = read(commmand.filename);
#ifdef TIME_READ
  auto read_t1 = high_resolution_clock::now();
  auto read_msec = duration_cast<milliseconds>(read_t1 - read_t0).count();
  cout << read_msec / 1E3 << " sec in read()" << endl;
#endif
  cout << words.size() << " words read" << endl;
  auto compounds = Tree::Compound(words, commmand.display, commmand.test);
  cout << "found a total of " << compounds << " compound words" << endl;
}
