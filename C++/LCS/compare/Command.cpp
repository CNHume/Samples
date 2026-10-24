// Copyright (C) 2017-2026, Christopher N. Hume.  All rights reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <https://www.gnu.org/licenses/>.
//
// 2017-06-29 CNHume  Created file
//
// Usage:
//
// The usage synopsis is the `usageSynopsis` string below, printed by -h and
// thrown by Parse() when the command line fails to parse.
//
#include "Command.h"
#include "Version.h"

#include <cerrno>                       // for errno
#include <cstdlib>                      // for EXIT_SUCCESS, exit, strtoul
#include <cstring>                      // for strlen
#include <iostream>                     // for cout
#include <stdexcept>                    // for invalid_argument, runtime_error

using namespace std;
using namespace lcs;

namespace {
  constexpr const char* usageSynopsis =
    "Usage: compare [-a <d|s|t>] [-b] [-c] [-e] [-f] [-h] [-i] [-j <join>] [-l] "
    "[-p <prefix>] [-q] [-s <suffix>] [-t] [-V] [-v] [-w] file1 file2";
}

string Command::arg_msg(const string& s, const string& name) {
  auto msg = string("Invalid ");
  msg += name;
  msg += ": ";
  msg += s;
  return msg;
}

uint32_t Command::parse_uint32(const char* s, const string& name) {
  char* pEnd;
  //
  // Reset errno before strtoul(): strtoul() sets errno only on failure and
  // leaves it unchanged on success, so a stale non-zero errno left by an
  // earlier call (such as setlocale()) would otherwise make a valid numeric
  // argument look like an overflow error.
  //
  errno = 0;
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
      case 'a':                         // the algorithm switch: d=dynamic programming, s=space, t=time
        {
          // Accept both -ad and -a d (whitespace optional, as with -j, -p, -s).
          const char* sub = nullptr;
          if (len == 3)                 // -ad, -as, or -at
            sub = &token[2];
          else if (len == 2 && n + 1 < argc && strlen(argv[n + 1]) == 1)
            sub = argv[++n];            // -a d, -a s, or -a t
          else {
            usage = true;               // -a needs exactly one sub-letter
            break;
          }
          switch (sub[0]) {
          case 'd':
            algorithm = LCS::Algorithm::HuntSzymanski;
            break;
          case 's':
            algorithm = LCS::Algorithm::RickSpace;
            break;
          case 't':
            algorithm = LCS::Algorithm::RickTime;
            break;
          default:
            usage = true;               // unknown algorithm
          }
        }
        break;

      case 'b':                         // the ignorespace switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ignorespace = true;
        break;

      case 'c':                         // the correspondence switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isscorrespondence = true;
        break;

      case 'e':                         // the environment switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isenvironment = true;
        break;

      case 'f':                         // the fasta switch, which implies text
        if (len > 2)                    // superfluous value specified
          usage = true;
        else {
          isfasta = true;
          istext = true;                // -f implies -t
        }
        break;

      case 'h':                         // the help switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ishelp = true;
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
        else if (n + 1 < argc)              // whitespace allowed
          join = parse_uint32(argv[++n], "join");
        else
          usage = true;
        break;

      case 'l':                         // the length-only switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          islength = true;
        break;

      case 'p':                         // the prefix switch
        if (len > 2)                    // whitespace optional
          prefix = parse_uint32(&token[2], "prefix");
        else if (n + 1 < argc)              // whitespace allowed
          prefix = parse_uint32(argv[++n], "prefix");
        else
          usage = true;
        break;

      case 'q':                         // the quote switch, which implies text
        if (len > 2)                    // superfluous value specified
          usage = true;
        else {
          isquote = true;
          istext = true;                // -q implies -t
        }
        break;

      case 't':                         // the text switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          istext = true;
        break;

      case 's':                         // the suffix switch
        if (len > 2)                    // whitespace optional
          suffix = parse_uint32(&token[2], "suffix");
        else if (n + 1 < argc)              // whitespace allowed
          suffix = parse_uint32(argv[++n], "suffix");
        else
          usage = true;
        break;

      case 'V':                         // the version switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isversion = true;
        break;

      case 'v':                         // the verbose switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isverbose = true;
        break;

      case 'w':                         // the word switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isword = true;
        break;

      default:                          // switch unknown
        usage = true;
        break;
      }
    }
  }

  if (ishelp) {
    cout << usageSynopsis << '\n';
    exit(EXIT_SUCCESS);
  }

  if (isversion) {
    cout << Version::ToString();
    exit(EXIT_SUCCESS);
  }

  if (n < argc)                         // parse f1
    file1 = argv[n++];
  else                                  // f1 is required
    usage = true;

  if (n < argc)                         // parse f2
    file2 = argv[n++];
  else                                  // f2 is required
    usage = true;

  if (n < argc)                         // superfluous argument specified
    usage = true;

  if (usage)                            // throw usage line if parse failed
    throw runtime_error(usageSynopsis);
}
