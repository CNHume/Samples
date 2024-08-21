//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
//[2011-10-10 CNHume]Created Console Application
//
// Conditionals:
//
#define NoteLaunchAndExit
#define PressEnter
//#define StackTrace

using Commands;

using static System.String;
using static Logging.Logger;

#region Constants
const String sSpace = " ";
const String sPrompt = "uci>";
#endregion

using UCI command = new();
try {
#if NoteLaunchAndExit
  LogInfo(LogLevel.note, $"Launched at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
#endif

  var sCommand = Join(sSpace, args).Trim();
  var bContinue = IsNullOrEmpty(sCommand) || command.Execute(sCommand);
  while (bContinue) {
    Log(sPrompt);
    var sLine = Console.ReadLine();
    if (sLine == null)
      bContinue = false;
    else {
      LogLine(sLine, false);
      bContinue = command.Execute(sLine.Trim());
    }
  }
#if NoteLaunchAndExit
  LogInfo(LogLevel.note, $"Exited at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
#endif
}
catch (ApplicationException ex) {
#if StackTrace
  LogInfo(LogLevel.error, ex.ToString());
#else
  LogInfo(LogLevel.error, ex.Message);
#endif
}
catch (Exception ex) {
  LogInfo(LogLevel.error, ex.ToString());
}
#if DEBUG && PressEnter
Console.Write("Press Enter");
Console.ReadLine();
#endif
