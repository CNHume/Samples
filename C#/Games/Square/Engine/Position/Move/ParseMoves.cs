//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2014-09-06 CNHume]Created File
//

namespace Engine;

using Commands;                          // For Scanner, Token

using Exceptions;

partial class Position : Board {
  #region Parse Move Methods
  //
  // Parse and make each PACN Move recursively, returning the final position:
  //
  public Position ParsePACNMakeMoves(Parser parser) {
    if (parser.SpaceToken.Accept() &&
        parser.PACNMoveToken.Accept()) {
      var child = Push();               // See UCI.unmove()
      try {
        var sMove = parser.PACNMoveToken.Value;
        var move = ParsePACNMove(sMove);

        if (!child.tryOrSkip(ref move))
          throw new MoveException(
            Friend.MoveError($"Illegal Move in {sMove}"));

        child.setName();

        // Tail Recursion
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
  // Parse PACN Move alternatives to be searched from the current position:
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
          var move = ParsePACNMove(sMove);

          if (!child.tryOrSkip(ref move))
            throw new MoveException(
              Friend.MoveError($"Illegal Move in {sMove}"));

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
      throw new ParseException("No Search Move specified");
  }
  #endregion                            // Parse Move Methods
}
