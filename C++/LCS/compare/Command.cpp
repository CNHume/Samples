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
#include "Host.h"
#include "SuspectDash.h"
#include "Version.h"

#include <cerrno>                       // for errno
#include <cstdlib>                      // for EXIT_SUCCESS, exit, strtoul
#include <cstring>                      // for strlen
#include <format>                       // for format
#include <iostream>                     // for cout
#include <stdexcept>                    // for invalid_argument, runtime_error

using namespace std;
using namespace lcs;

namespace {
  constexpr const char* usageSynopsis =
    "Usage: compare [-a <a|d|m|s|t>] [-b] [-C] [-F] [-h] [-H] [-i] "
    "[-j <join>] [-l] [-p <prefix>] [-Q] [-s <suffix>] [-t <tabsize>] "
    "[-T] [-u] [-v] [-V] [-w] [-W <width>] [-y <c|p|s>] file1 file2";

  constexpr uint32_t edit_context_affix = 3;  // -u default context
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
  auto isunified = false;               // -u given
  auto isyformat = false;               // -y given
  auto prefixGiven = false;             // -p given
  auto suffixGiven = false;             // -s given
  auto n = 1;                           // skip verb
  for (; n < argc && !usage; n++) {     // parse any switches
    char* token = argv[n];
    auto len = strlen(token);
    if (len < 1 || token[0] != '-') {
      auto message = SuspectDashMessage(token);
      if (message.empty())              // not a switch
        break;
      throw runtime_error(message);     // switch typed with a suspect dash
    }
    else if (len < 2)
      usage = true;
    else if (len == 2 && token[1] == '-') {
      n++;                              // skip the -- operand delimiter
      break;                            // end of switches
    }
    else {                              // parse single character switch
      switch (token[1]) {
      case 'a':                         // the algorithm switch
        {
          //
          // Parse algorithm specifier: a=automatic, d=edit distance, m=matches,
          // s=space, t=time.  Allow leading spaces -a d (as in -j, -p, -s).
          //
          const char* sub = nullptr;
          if (len > 2)                 // Same token
            sub = &token[2];
          else if (n + 1 < argc && strlen(argv[n + 1]) == 1)
            sub = argv[++n];            // Next token
          else {
            usage = true;               // -a needs exactly one sub-letter
            break;
          }
          switch (sub[0]) {
          case 'a':
            algorithm = Algorithm::Automatic;
            break;
          case 'd':
            algorithm = Algorithm::EditDistance;
            break;
          case 'm':
            algorithm = Algorithm::Matches;
            break;
          case 's':
            algorithm = Algorithm::RickSpace;
            break;
          case 't':
            algorithm = Algorithm::RickTime;
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

      case 'C':                         // the correspondence switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          iscorrespondence = true;
        break;

      case 'F':                         // the fasta switch, which implies text
        if (len > 2)                    // superfluous value specified
          usage = true;
        else {
          isfasta = true;
          istext = true;                // -F implies -T
        }
        break;

      case 'h':                         // the help switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ishelp = true;
        break;

      case 'H':                         // the host switch (early exit)
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          ishost = true;
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
        else if (n + 1 < argc)          // whitespace allowed
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
        else if (n + 1 < argc)          // whitespace allowed
          prefix = parse_uint32(argv[++n], "prefix");
        else
          usage = true;
        prefixGiven = true;
        break;

      case 'Q':                         // the quote switch, which implies text
        if (len > 2)                    // superfluous value specified
          usage = true;
        else {
          isquote = true;
          istext = true;                // -Q implies -T
        }
        break;

      case 's':                         // the suffix switch
        if (len > 2)                    // whitespace optional
          suffix = parse_uint32(&token[2], "suffix");
        else if (n + 1 < argc)          // whitespace allowed
          suffix = parse_uint32(argv[++n], "suffix");
        else
          usage = true;
        suffixGiven = true;
        break;

      case 't':                         // the tab size switch (parallel layout)
        if (len > 2)                    // whitespace optional
          tabsize = parse_uint32(&token[2], "tabsize");
        else if (n + 1 < argc)          // whitespace allowed
          tabsize = parse_uint32(argv[++n], "tabsize");
        else
          usage = true;
        break;

      case 'T':                         // the text switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          istext = true;
        break;

      case 'u':                         // the unified diff switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else {
          format = Format::Edit;
          isunified = true;
        }
        break;

      case 'v':                         // the verbose switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isverbose = true;
        break;

      case 'V':                         // the version switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isversion = true;
        break;

      case 'w':                         // the word switch
        if (len > 2)                    // superfluous value specified
          usage = true;
        else
          isword = true;
        break;

      case 'W':                         // the width switch (parallel layout)
        if (len > 2)                    // whitespace optional
          width = parse_uint32(&token[2], "width");
        else if (n + 1 < argc)          // whitespace allowed
          width = parse_uint32(argv[++n], "width");
        else
          usage = true;
        if (width < MIN_WIDTH)          // reject below the minimum
          throw invalid_argument(
            std::format("Invalid width: {} (minimum is {})", width, MIN_WIDTH));
        break;

      case 'y':                         // the format switch: c=CIGAR, p=parallel, s=sequential
        {
          //
          // Parse format specifier: c=CIGAR, p=parallel, s=sequential.
          // Allow leading spaces -y c (as in -j, -p, -s).
          //
          const char* sub = nullptr;
          if (len > 2)                 // Same token
            sub = &token[2];
          else if (n + 1 < argc && strlen(argv[n + 1]) == 1)
            sub = argv[++n];            // Next token
          else {
            usage = true;               // -y needs exactly one sub-letter
            break;
          }
          switch (sub[0]) {
          case 'c':
            format = Format::CIGAR;
            break;
          case 'p':
            format = Format::Parallel;
            break;
          case 's':
            format = Format::Sequential;
            break;
          default:
            usage = true;               // unknown format
          }
          isyformat = true;
        }
        break;

      default:                          // switch unknown
        usage = true;
        break;
      }
    }
  }

  // Early-exit switches print their report and terminate without comparing.
  if (ishelp) {
    cout << usageSynopsis << '\n';
    exit(EXIT_SUCCESS);
  }

  if (isversion) {
    cout << Version::ToString();
    exit(EXIT_SUCCESS);
  }

  if (ishost) {
    cout << Host::ToString();
    exit(EXIT_SUCCESS);
  }

  if (iscorrespondence && format == Format::CIGAR)
    throw invalid_argument(
      "-C (correspondence) cannot be combined with -y c (CIGAR)");

  if (isunified && isyformat)
    throw invalid_argument("-u cannot be combined with -y");

  if (isunified) {
    if (iscorrespondence)
      throw invalid_argument("-C (correspondence) cannot be combined with -u");
    if (istext)
      throw invalid_argument(
        "-F, -Q, and -T (text mode) cannot be combined with -u");
    if (islength)
      throw invalid_argument("-l (length only) cannot be combined with -u");
    if (isword)
      throw invalid_argument("-w (word mode) with -u is not yet implemented");

    if (!prefixGiven)
      prefix = edit_context_affix;
    if (!suffixGiven)
      suffix = edit_context_affix;
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
