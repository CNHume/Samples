// Copyright (C) 2018, Christopher N. Hume.  All rights reserved.
//
// 2018-05-16 CNHume  Added Command class
//
#include "Command.h"

#include <stdexcept>

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
      case 'd':                         // the display switch
        if (len > 2)                    // whitespace optional
          display = parse_uint32(&token[2], "display");
        else if (n < argc)              // whitespace allowed
          display = parse_uint32(argv[++n], "display");
        else
          usage = true;
        break;

      case 't':                         // the test switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          test = true;
        break;

      default:                          // switch unknown
        usage = true;
        break;
      }
    }
  }

  if (n < argc)                         // parse the filename
    filename = argv[n++];
  else                                  // filename is required
    usage = true;

  if (n < argc)                         // superfluous argument specified
    usage = true;

  if (usage)                            // throw usage line if parse failed
    throw runtime_error("Usage: together [-t] [-d <display-limit>] filename");
}
