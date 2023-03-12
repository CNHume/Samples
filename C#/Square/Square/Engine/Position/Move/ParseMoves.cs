//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
//#define DebugParse

namespace Engine {
  using Command;                        // For Scanner, Token
  using Command.Exceptions;

  using static Command.Parser;

  partial class Position : Board {
    #region Parse Move Methods
    //
    // The following advances through a line of PACN moves, pushing each position; and returns the final position
    //
    public Position ParsePACNMakeMoves(Parser parser) {
      //
      // Parse and make each PACN Move recursively, returning the final position:
      //
      if (parser.SpaceToken.Accept() &&
          parser.PACNMoveToken.Accept()) {
        var sMove = parser.PACNMoveToken.Value;
        var child = Push();             // See UCI.unmove()
        try {
          var move = ParsePACNMove(GamePly, sMove);

          if (!child.tryOrSkip(ref move)) {
            throw new MoveException(
              $"{Friend.MoveId(GamePly)}Illegal Move in {sMove}");
          }

          child.setName();
          return child.ParsePACNMakeMoves(parser);
        }
        catch {
          // Reclaim child if ParsePACNMove() should throw an Exception.
          Pop(ref child);
          throw;
        }
      }

      return this;
    }

    //
    // The following parses a line of PACN move alternatives at the current position
    //
    public void ParsePACNSearchMoves(Parser parser, List<Move> searchMoves) {
      searchMoves.Clear();
      var parseMoves = new List<Move>();

      if (parser.SpaceToken.Accept()) {
        var child = Push();
        try {
          //
          // Parse alternative moves wrt the current position,
          // without actually making any move.
          //
          while (parser.PACNMoveToken.Accept()) {
            var sMove = parser.PACNMoveToken.Value;
            var move = ParsePACNMove(GamePly, sMove);

            if (!child.tryOrSkip(ref move))
              throw new MoveException(
                $"{Friend.MoveId(GamePly)}Illegal Move in {sMove}");

            parseMoves.Add(move);

            if (!parser.SpaceToken.Accept()) break;
          }
        }
        finally {
          // Reclaim child if ParsePACNMove() should throw an Exception.
          Pop(ref child);
        }
      }

      searchMoves.AddRange(parseMoves.Distinct());
      if (searchMoves.Count == 0)
        throw new MoveException("No Search Move specified");
    }
    #endregion
  }
}
