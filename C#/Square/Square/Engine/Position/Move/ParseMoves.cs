//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//
// Conditionals:
//
//#define DebugParse

using System.Text;

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
      var position = this;

      if (parser.SpaceToken.Accept()) {
        //
        // Make each move sequentially, returning the final position
        //
        while (parser.PACNMoveToken.Accept()) {
          var sMove = parser.PACNMoveToken.Value;
          var child = position.Push();  // See UCI.unmove()
          try {
            var move = position.ParsePACNMove(position.GamePly, sMove);
            if (child.tryOrSkip(ref move)) {
              position = child;
              position.setName();
            }
            else
              throw new MoveException($"Illegal Move: {sMove}");
          }
          catch {
            // Reclaim *last* child if parsePACNMove() should fail to complete normally
            Pop(ref child);
            throw;
          }

          if (!parser.SpaceToken.Accept()) break;
        }
      }

      return position;
    }

    //
    // The following parses a line of PACN move alternatives at the current position
    //
    public void ParsePACNSearchMoves(List<Move> searchMoves, Token spaceToken, Token pacnMoveToken) {
      searchMoves.Clear();
      var parseMoves = new List<Move>();

      if (spaceToken.Accept()) {
        var child = Push();
        try {
          while (pacnMoveToken.Accept()) {
            //
            // Parse alternative moves, wrt the current position,
            // without actually making the moves.
            //
            var sMove = pacnMoveToken.Value;
            var move = ParsePACNMove(GamePly, sMove);
            if (child.tryOrSkip(ref move))
              parseMoves.Add(move);
            else
              throw new MoveException($"Illegal Move: {sMove}");

            if (!spaceToken.Accept()) break;
          }
        }
        finally {
          // Reclaim child if parsePACNMove() should fail to complete normally
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
