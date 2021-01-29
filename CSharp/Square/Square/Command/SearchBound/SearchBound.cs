//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-08-14 CNHume]Created Class
//
namespace Command {
  using Engine;
  using Exceptions;
  using static Parser;

  using System;
  using System.Text;
  using System.Threading;

  //
  // Type Aliases:
  //
  using PlyDepth = System.Byte;

  struct SearchBound {
    #region Constants
    private const Int32 nMinMoveTimeMS = 1000;
    #endregion

    #region Methods
    public void Clear(Boolean bWTM) {
      WTM = bWTM;
      WhiteTimeMS = BlackTimeMS = MoveTimeMS = default;
      WhiteIncrementMS = BlackIncrementMS = default;
      MovesToGo = MovesToMate = default;
      Nodes = default;
      Plies = default;
      IsPonder = IsInfinite = IsDepth = false;
    }

    public Int32 MoveTime(UInt16 wExpectedMovesToGo) {
      var nMoveTimeMS = Timeout.Infinite;
      if (IsInfinite || IsPonder)
        return nMoveTimeMS;

      if (MoveTimeMS.HasValue)
        nMoveTimeMS = (Int32)MoveTimeMS;
      else {
        var nMoves = MovesToGo.HasValue ? (Int32)MovesToGo : wExpectedMovesToGo;
        if (nMoves < 1)                 //[Safe]Preventing DBZ below
          nMoves = 1;

        if (WTM) {
          if (WhiteTimeMS.HasValue) {
            nMoveTimeMS = (Int32)WhiteTimeMS / nMoves;
            if (WhiteIncrementMS.HasValue && WhiteIncrementMS < WhiteTimeMS)
              nMoveTimeMS += WhiteIncrementMS.Value;
          }
        }
        else {
          if (BlackTimeMS.HasValue) {
            nMoveTimeMS = (Int32)BlackTimeMS / nMoves;
            if (BlackIncrementMS.HasValue && BlackIncrementMS < BlackTimeMS)
              nMoveTimeMS += BlackIncrementMS.Value;
          }
        }

        if (nMoveTimeMS != Timeout.Infinite && nMoveTimeMS < nMinMoveTimeMS)
          nMoveTimeMS = nMinMoveTimeMS;
      }

      return nMoveTimeMS;
    }

    public StringBuilder AppendBounds(StringBuilder sb) {
      if (Plies.HasValue && IsDepth)
        sb.AppendFormat(" {0}-Ply", Plies);

      if (MovesToMate.HasValue)
        sb.AppendFormat(" Mate in {0}", MovesToMate);

      return sb;
    }

    public Boolean ParseBounds(Parser parser, Position position) {
      var bValid = true;
      var bWTM = position.WTM();
      Clear(bWTM);

      var searchMoves = position.SearchMoves;
      if (searchMoves is not null)          // Clear any previous SearchMoves prior to a new go command
        searchMoves.Clear();

      IsDepth = false;
      IsInfinite = false;
      IsPonder = false;

      if (parser.SpaceLexeme.Accept()) {
        while (parser.GoKeywordLexeme.Accept()) {
          var bFoundKeyword = true;
          var sKeyword = parser.GoKeywordLexeme.Value;
          var sLower = sKeyword.ToLower();
          switch (sLower) {
          case "searchmoves":
            //
            // Parse a list of legal PACN moves to search from the current position:
            //
            if (searchMoves is null) searchMoves = position.newSearchMoves();
            position.ParsePACNSearchMoves(searchMoves, parser.SpaceLexeme, parser.PACNMoveLexeme);
            break;
          case "ponder":
            //
            // Enter an infinite Ponder Search, which may end with a "stop" command;
            // or which may continue as a BestMove Search after a "ponderhit" command.
            //
            // When the Engine reports its BestMove, it may include the best response
            // found for the opponent as the candidate move to Ponder.
            //
            // The UCI will then echo this move in the current go command, along with
            // the ponder keyword.  Thus, the UCI assumes the "predicted move" should
            // be considered, rather than considering all of the opponent's candidate
            // moves.
            //
            IsPonder = true;
            break;
          case "wtime":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            WhiteTimeMS = ParseUInt16(sLower, parser.UnsignedLexeme.Value);
            break;
          case "btime":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            BlackTimeMS = ParseUInt16(sLower, parser.UnsignedLexeme.Value);
            break;
          case "winc":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            WhiteIncrementMS = ParseUInt16(sLower, parser.UnsignedLexeme.Value);
            break;
          case "binc":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            BlackIncrementMS = ParseUInt16(sLower, parser.UnsignedLexeme.Value);
            break;
          case "movestogo":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            MovesToGo = ParseUInt16(sLower, parser.UnsignedLexeme.Value);
            break;
          case "depth":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            Plies = ParseByte(sLower, parser.UnsignedLexeme.Value);
            IsDepth = true;
            IsInfinite = false;
            break;
          case "nodes":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            Nodes = ParseUInt64(sLower, parser.UnsignedLexeme.Value);
            IsInfinite = false;
            break;
          case "mate":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            MovesToMate = ParseUInt16(sLower, parser.UnsignedLexeme.Value);

            //
            // Establish a default max number of plies to search,
            // in case the MovesToMate bound cannot be satisfied.
            //
            //[Note]One ply is added to verify Mate delivered on
            // the final ply.
            //
            if (!IsInfinite && !Plies.HasValue)
              Plies = (Byte)(2 * MovesToMate + 1);
            break;
          case "movetime":
            parser.SpaceLexeme.Expect();
            parser.UnsignedLexeme.Expect();
            MoveTimeMS = ParseUInt32(sLower, parser.UnsignedLexeme.Value);
            IsInfinite = false;
            break;
          case "infinite":
            IsDepth = false;
            IsInfinite = true;
            MoveTimeMS = default;
            MovesToMate = default;
            Plies = default;
            Nodes = default;
            break;
          default:
            bFoundKeyword = false;
            break;
          }

          if (!bFoundKeyword)
            throw new ParseException($"Unknown {sKeyword} keyword");

          if (!parser.SpaceLexeme.Accept()) break;
        }

        parser.ExpectEOL();
      }

      return bValid;
    }
    #endregion

    #region Primary Fields
    public UInt16? WhiteIncrementMS;
    public UInt16? BlackIncrementMS;
    public UInt32? WhiteTimeMS;
    public UInt32? BlackTimeMS;
    public UInt32? MoveTimeMS;

    public PlyDepth? Plies;
    public UInt16? MovesToGo;
    public UInt16? MovesToMate;
    public UInt64? Nodes;

    public Boolean IsDepth;             // Depth Bound explicitly set vs. implied by MovesToMate
    public Boolean IsInfinite;
    public Boolean IsPonder;
    public Boolean WTM;
    #endregion
  }
}
