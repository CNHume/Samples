//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
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
//[2010-07-17 CNHume]Created Class
//
// Conditionals:
//
#define UseTask

/*
 *    Reasons for a Result:
 *
 *    Decision           Draw
 *    --------           ----
 *    M Checkmate        S Stalemate
 *    R Resignation      D Draw Agreed by Players
 *    3-fold Repetition  N No Capture or Pawn Move in 50 moves [100 plies]
 *    T Time Forfeit     I Insufficient Material to Mate (KKN, KK or KB*KB* same color)
 *    F Rule Forfeit     A Adjudicated (Insufficient Losing Chances) or Declared (75-Move Rule)
 */
namespace Command {
  using Engine;
  using Engine.Exceptions;

  using Exceptions;

  using System;
  using System.Diagnostics.CodeAnalysis;
  using System.Text;

  using static Logging.Logger;
  using static System.String;

  partial class UCI : ICommand {
    #region Constructors
    public UCI() {
      IsVerbose = false;
    }

    static UCI() {
      IsDebug = true;                   //[UCI]Default
      IsRegistrationChecked = true;     //[UCI]Default
      IsRegistered = false;             //[UCI]Default
      RegistrationCode = default;
      RegistrationName = default;
      DefaultEPD = sDefaultEPD;
      DefaultFEN = sDefaultFEN;
    }
    #endregion

    #region IDisposable Methods
    public void Dispose() {
      Dispose(true);
      GC.SuppressFinalize(this);
    }

    protected virtual void Dispose(bool disposing) {
      if (!disposed) {
        if (disposing) Parser?.Dispose();
        disposed = true;
      }
    }
    #endregion

    #region Command Processor
    /*
     * Please see https://ucichessengine.wordpress.com/2011/03/16/description-of-uci-protocol/ for details
     * of the UCI Command Interface as documented by Stefan Meyer-Kahlen
     * 
     * Commands
     * --------
     * uci => must respond w "id name" and "id author" then w option support below, ending w "uciok"
     * debug [on | off] => invites "info string" responses.
     * isready => starts Engine, which must respond w "readyok" (even when searching.)
     * setoption name Name [value Value]
     * ucinewgame
     * position [epd | fen | startpos] [moves ...]
     * position random
     * go [searchmoves | ponder | wtime | btime | winc | binc | movestogo |
     *     depth | nodes | mate | movetime | infinite] => respond w "bestmove ... [ponder ...]"
     * stop
     * ponderhit
     * register [later | name | code] => registration [checking | ok | error]
     * quit
     *
     * Info
     * ----
     * info [depth | seldepth | time | nodes | pw | multipv |
     *       score [cp | mate | lowerbound | upperbound] |
     *       currmove | currmovenumber | hashfull | nps | tbhits |
     *       cpuload | string | refutation | currline]
     *
     * Option
     * ------
     * option name [Hash | SyzygyPath | SyzygyCache | Nullmove | OwnBook | Ponder | MultiPV |
     *              UCI_ShowCurrLine | UCI_ShowShowRefutations | UCI_LimitStrength |
     *              UCI_Elo | UCI_AnalyseMode | UCI_Opponent]
     *
     * Type
     * ----
     * type [check | spin | combo | button | string] default | min | max | var*
     */
    public Boolean Dispatch(String sVerb) {
      if (Parser is null)
        throw new ChessException("Uninitialized Parser");

      var bContinue = true;
      switch (sVerb.ToLower()) {
      case "":                          // Empty Command String
        break;

      case "uci":                       //[UCI]
        Info();
        break;

      case "isready":                   //[UCI]
        initStaticFields();
        LogLine("readyok");
        break;

      case "register":                  //[UCI]
        parseRegister(Parser);
        break;

      case "timertest":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");

        State.OnMoveCommand();
        State.MovePosition.TimerTest();
        break;

      case "test":                      //[ToDo]End gracefully when a Search is in progress!
        if (State is null) newState();
        if (State.IsSearchInProgress)
          throw new ChessException("Search in progress");
        else
          newGameFEN(DefaultFEN);
        break;

      case "testepd":                   //[ToDo]End gracefully when a Search is in progress!
        if (State is null) newState();
        if (State.IsSearchInProgress)
          throw new ChessException("Search in progress");
        else
          newGameEPD(DefaultEPD);
        break;

      case "reset":                     // Intuitive
      case "ucinewgame":                //[UCI]
        if (State is null) newState();

        if (State.IsSearchInProgress)
          State.Stop();

        newGameFEN();
        break;

      case "position":                  //[UCI]
        if (State is null) newState();
        if (State.IsSearchInProgress)
          throw new ChessException("Search in progress");
        else {
          var newPosition = NewGame();

          //
          //[Note]PositionCommand() will parse and then make any
          // sequence of moves that follows the initial position.
          //
          var parsePosition = Parser.PositionCommand(newPosition);
          State.MovePosition = parsePosition;
        }
        break;

      case "board":                     //[Test]In the absence of a GUI
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");
        else
          State.MovePosition.Display();
        break;

      case "tabiya":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");

        Parser.TabiyaCommand(State.MovePosition);
        break;

      case "moves":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");

        State.OnMoveCommand();
        var movesPosition = State.MovePosition.ParsePACNMakeMoves(Parser);
        State.MovePosition = movesPosition;
        break;

      case "unmove":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");

        State.OnMoveCommand();
        if (ReferenceEquals(State.MovePosition, State.RootPosition))
          throw new ChessException("No Move has been made");

        State.Unmove();
        break;

      case "list":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");

        State.ListMovesFromRoot(State.MovePosition, Parser.ListCommand());
        break;

      case "getoption":                 //[Debug]
        Parser.GetOptionCommand();
        break;

      case "resetoption":               //[Debug]
        if (State is null) newState();  // Event Handler may require GameState
        Parser.ResetOptionCommand();
        break;

      case "setoption":                 //[UCI]
        if (State is null) newState();  // Event Handler may require GameState
        Parser.SetOptionCommand();
        break;

      case "debug":
        parseDebug(Parser);
        break;

      case "perft":                     //[Test]Look for corresponding Tabiya and run PerftCases
        if (State is null)
          throw new ChessException("Uninitialized Game");

        State.OnMoveCommand();
        Parser.ExpectEOL();
        State.Perft();
        break;

      case "go":                        //[UCI]
        if (State is null)
          throw new ChessException("Uninitialized Game");

        State.OnMoveCommand();
        State.Go(Parser);
        break;

      case "best":                      //[Test]
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.MovePosition is null)    //[Safe]
          throw new ChessException("Uninitialized Position");
        else if (State.BestMoves is not null) {
          var sb = new StringBuilder();
          //[Note]refreshPV() may not have been called
          sb.BestMove(State.BestMoves, State.MovePosition.Side, State.IsChess960);
          if (sb.Length > 0)
            throw new ChessException(sb.ToString());
        }
        break;

      case "ponderhit":                 //[UCI]
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else
          State.Ponderhit();
        break;
#if UseTask
      case "status":
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.EngineTask is null)
          throw new ChessException("No search in progress");

        throw new ChessException($"Search is {State.EngineTask.Status}");
      //[Unreachable]break;

      case "stop":                      //[UCI]
        if (State is null)
          throw new ChessException("Uninitialized Game");
        else if (State.EngineTask is null)
          throw new ChessException("No search in progress");

        if (State.IsSearchInProgress)
          State.Stop();
        break;
#endif
      case "exit":                      // Make it easy to quit
      case "quit":                      //[UCI]
        if (State is not null && State.IsSearchInProgress)
          State.Stop();
        bContinue = false;
        break;

      default:
        throw new ParseException($"Ignored {sVerb} command");
      }

      return bContinue;
    }

    public Boolean Execute(String sCommand) {
      var bContinue = true;

      //
      //[Note]BoardExceptions are not caught here,
      // because they should report a Stack Trace.
      //
      try {
        EnsureParser(sCommand);
        var sVerb = Parser.ParseVerb();
        bContinue = Dispatch(sVerb);
        Parser.ExpectEOL();
      }
      catch (ChessException ex) {
        LogLine(ex.Message);
      }
      catch (PositionException ex) {
        LogLine(ex.Message);
      }

      return bContinue;
    }

    [MemberNotNull(nameof(Parser))]
    protected void EnsureParser(String sCommand) {
      if (Parser is null)
        Parser = new Parser(Parser.NewScanner(sCommand), IsVerbose);
      else
        Parser.EnsureScanner(sCommand);
    }
    #endregion

    #region Command Helpers
    protected static void initStaticFields() {
      //
      //[Init]Reference a static field in the Position class to force invocation of its
      // static constructor, following similar invocations for each of its base classes.
      //
      var features = Position.PawnFeatures;
    }

    [MemberNotNull(nameof(State))]
    protected void newState() {
      State = new GameState(this);
    }

    public Position NewGame() {
      if (State is null)
        newState();
      else
        State.Clear();

      // Open new Root Position
      State.MovePosition =
        State.RootPosition = State.Push(null);

      return State.MovePosition;
    }

    protected void newGameEPD(String? sEPD = null) {
      NewGame().SetEPD(sEPD);
    }

    protected void newGameFEN(String? sFEN = null) {
      NewGame().SetFEN(sFEN);
    }

    protected void parseDebug(Parser parser) {
      if (parser.AcceptEOL()) {
        var sKeyword = IsDebug ? "on" : "off";
        LogLine($"debug is {sKeyword}");
      }
      else
        IsDebug = parser.ParseEnableKeyword();
    }
    #endregion

    #region Static Helpers
    public static void parseRegister(Parser parser) {
      var bValidCommand = true;
      var bLater = false;
      String? sCode = default;
      String? sName = default;

      if (parser.SpaceToken.Accept()) {
        while (bValidCommand && parser.RegisterKeywordToken.Accept()) {
          var bFoundKeyword = true;
          var sKeyword = parser.RegisterKeywordToken.Value;

          switch (sKeyword.ToLower()) {
          case "later":
            bLater = true;
            break;
          case "code":
            sCode = parser.ParseCode();
            break;
          case "name":
            sName = parser.ParseCode();
            break;
          default:
            bFoundKeyword = false;
            break;
          }

          bValidCommand &= bFoundKeyword;

          if (!bFoundKeyword)
            throw new ParseException($"Unknown {sKeyword} keyword");

          if (!parser.SpaceToken.Accept()) break;
        }
      }

      bValidCommand &= parser.AcceptEOL();

      if (bValidCommand)
        setRegistration(bLater, sCode, sName);
    }

    private static void setRegistration(Boolean bLater, String? sCode, String? sName) {
      if (IsRegistrationChecked && !bLater) {
        LogLine("registration checking");

        RegistrationCode = sCode;
        RegistrationName = sName;

        if (!IsNullOrEmpty(RegistrationCode) &&
            !IsNullOrEmpty(RegistrationName))
          //[ToDo]Validate RegistrationCode wrt RegistrationName
          IsRegistered = true;

        var sRegistered = IsRegistered ? "ok" : "error";
        LogLine($"registration {sRegistered}");
      }
    }

    protected static void showId() {
      LogLine($"id name {Product.ProductName} {Product.ProductVersion}");
      LogLine($"id author {Product.CompanyName}");
      LogLine($"id copyright {Product.Copyright}");
      LogLine($"id description {Product.Description}");
    }

    protected static void showOptions(Boolean IsDebug) {
      foreach (var uciControl in GameState.Controls) {
        if (uciControl is not null)
          if (IsDebug || !uciControl.Option.IsHidden)
            LogLine(uciControl.Option.ToString());
      }
    }

    public static void Info() {
      showId();                         // Respond with id and option strings
      showOptions(IsDebug);
      LogLine("uciok");
    }
    #endregion
  }
}
