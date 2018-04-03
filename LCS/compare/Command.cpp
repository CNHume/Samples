// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
//
// 2017-06-29 CNHume  Created file
//
#include "Command.h"

string Command::arg_msg(const string& s, const string& name) {
  auto msg = string("Invalid ");
  msg += name;
  msg += ": ";
  msg += s;
  return msg;
}

uint32_t Command::parse_uint32(const char* s, const string& name) {
  char* pEnd;
  auto result = strtoul(s, &pEnd, 10);
  if (errno != 0 || *pEnd != '\0')
    throw invalid_argument(arg_msg(s, name));
  return result;
}

//
// command line parser
//
void Command::Parse(int argc, char* argv[]) {
  auto usage = false;
  auto n = 1;                           // skip verb
  for (; n < argc && !usage; n++) {     // parse any switches
    char* token = argv[n];
    auto len = strlen(token);
    if (len < 1 || token[0] != '-')     // end of switches
      break;
    else if (len < 2)
      usage = true;
    else {                              // parse single character switch
      switch (token[1]) {
      case 'b':                         // the ignorespace switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ignorespace = true;
        break;

      case 'i':                         // the ignorecase switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ignorecase = true;
        break;

      case 'j':                         // the join switch
        isjoin = true;
        if (len > 2)                    // whitespace optional
          join = parse_uint32(&token[2], "join");
        else if (n < argc)              // whitespace allowed
          join = parse_uint32(argv[++n], "join");
        else
          usage = true;
        break;

      case 'p':                         // the prefix switch
        if (len > 2)                    // whitespace optional
          prefix = parse_uint32(&token[2], "prefix");
        else if (n < argc)              // whitespace allowed
          prefix = parse_uint32(argv[++n], "prefix");
        else
          usage = true;
        break;

      case 's':                         // the suffix switch
        if (len > 2)                    // whitespace optional
          suffix = parse_uint32(&token[2], "suffix");
        else if (n < argc)              // whitespace allowed
          suffix = parse_uint32(argv[++n], "suffix");
        else
          usage = true;
        break;

      default:                          // switch unknown
        usage = true;
        break;
      }
    }
  }

  if (n < argc)                         // parse f1
    f1 = argv[n++];
  else                                  // f1 is required
    usage = true;

  if (n < argc)                         // parse f2
    f2 = argv[n++];
  else                                  // f2 is required
    usage = true;

  if (n < argc)                         // superfluous argument specified
    usage = true;

  if (usage)                            // throw usage line if parse failed
    throw runtime_error("Usage: compare [-b] [-i] [-j <join>] [-p <prefix>] [-s <suffix>] file1 file2");
}
