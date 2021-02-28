//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
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
//#define StackTrace
#define NoteLaunchAndExit

namespace Square {
  using Command;
  using static Logging.Logger;

  using System;
  using static System.String;

  static class Program {
    #region Constants
    private const String sSpace = " ";
    private const String sPrompt = "uci>";
    #endregion

    #region Methods
    private static void Main(String[] args) {
      using (var command = new UCI()) {
        try {
          if (UCI.IsDebug) {
            var dtLaunch = DateTime.Now;
#if NoteLaunchAndExit
            LogInfo(Level.note, "Launched at {0:HH:mm:ss.ff}", dtLaunch);
#endif
          }

          var sCommand = Join(sSpace, args).Trim();
          var bContinue = IsNullOrEmpty(sCommand) || command.Execute(sCommand);
          while (bContinue) {
            Log(sPrompt);
            var sLine = Console.ReadLine();
            LogWriteLine(sLine);
            bContinue = sLine is not null && command.Execute(sLine.Trim());
          }

          var dtExited = DateTime.Now;
#if NoteLaunchAndExit
          LogInfo(Level.note, "Exited at {0:HH:mm:ss.ff}", dtExited);
#endif
        }
        catch (ApplicationException ex) {
#if StackTrace
          LogInfo(Level.error, ex.ToString());
#else
          LogInfo(Level.error, ex.Message);
#endif
        }
        catch (Exception ex) {
          LogInfo(Level.error, ex.ToString());
        }
      }
#if DEBUG
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }
    #endregion
  }
}
