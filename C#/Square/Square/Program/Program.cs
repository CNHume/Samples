//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
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

using static System.String;

namespace Square {
  using Command;

  using static Logging.Logger;

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
#if NoteLaunchAndExit
            LogInfo(Level.note, $"Launched at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
#endif
          }

          var sCommand = Join(sSpace, args).Trim();
          var bContinue = IsNullOrEmpty(sCommand) || command.Execute(sCommand);
          while (bContinue) {
            Log(sPrompt);
            var sLine = Console.ReadLine();
            if (sLine == null)
              bContinue = false;
            else {
              LogWriteLine(sLine);
              bContinue = command.Execute(sLine.Trim());
            }
          }
#if NoteLaunchAndExit
          LogInfo(Level.note, $"Exited at {DateTime.Now:yyyy-MM-dd HH:mm:ss.ff}");
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
#if DEBUG && PressEnter
      Console.Write("Press Enter");
      Console.ReadLine();
#endif
    }
    #endregion
  }
}
