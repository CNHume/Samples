// (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-05-16 CNHume  Added Command class
// 2014-12-06 CNHume  Moved join() template function into join.h
// 2014-12-06 CNHume  Moved Grow(), FindList(), FindCount() and Compound() methods into the Tree class
// 2014-11-26 CNHume  Moved implementation of the Tree class into Tree.cpp
// 2014-11-23 CNHume  Tree came together
//
// Purpose:
//
// Identifies the longest words that can be formed as compounds
// of words other than themselves
//
// Invoke with input filename as the sole command line argument:
//
// together words.txt
//
// Title inspired by The Beatles: Come Together
//
#include "Command.h"
#include "Processor.h"

#include <iostream>                     // for cout

//
// command line application
//
int main(int argc, char* argv[]) {
  const uint32_t displayDefault = 2;
  auto errorLevel = EXIT_FAILURE;

  try {
    Command command;
    command.Parse(argc, argv);

    Processor processor;
    processor.Execute(command);

    errorLevel = EXIT_SUCCESS;
  }
  catch (exception& ex) {
    cout << ex.what() << endl;
  }
  catch (...) {
    cout << "exception" << endl;
    throw;
  }
#ifdef _DEBUG
  cout << Tree::Arr2s << " arr2s remain" << endl;
  cout << Tree::Arr1s << " arr1s remain" << endl;
  cout << Tree::Nodes << " nodes remain" << endl;
  cout << "Press Enter";
  char c;
  cin.getline(&c, 1);
#endif
  return errorLevel;
}
