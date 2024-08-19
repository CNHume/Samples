//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-08-14 CNHume]Created Class
//
using System.Text;

namespace Commands;

using Engine;

using Exceptions;

using static Parser;

//
// Type Aliases:
//
using PlyDepth = Byte;

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
      if (nMoves < 1)                   //[Safe]Preventing DBZ below
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
      sb.AppendFormat($" {Plies}-Ply");

    if (MovesToMate.HasValue)
      sb.AppendFormat($" Mate in {MovesToMate}");

    return sb;
  }

  public Boolean ParseBounds(Parser parser, Position position) {
    var bValid = true;
    var bWTM = position.WTM();
    Clear(bWTM);

    //
    // Clear any previous SearchMoves prior to a new go command:
    //
    var searchMoves = position.SearchMoves;
    if (searchMoves != null)
      searchMoves.Clear();

    IsDepth = false;
    IsInfinite = false;
    IsPonder = false;

    if (parser.SpaceToken.Accept()) {
      while (parser.GoKeywordToken.Accept()) {
        var bFoundKeyword = true;
        var sKeyword = parser.GoKeywordToken.Value;
        var sLower = sKeyword.ToLower();
        switch (sLower) {
        case "searchmoves":
          //
          // Parse a list of legal PACN moves to search from the current position:
          //
          if (searchMoves == null) searchMoves = position.newSearchMoves();
          position.ParsePACNSearchMoves(parser, searchMoves);
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
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          WhiteTimeMS = ParseUInt16(sLower, parser.UnsignedToken.Value);
          break;
        case "btime":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          BlackTimeMS = ParseUInt16(sLower, parser.UnsignedToken.Value);
          break;
        case "winc":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          WhiteIncrementMS = ParseUInt16(sLower, parser.UnsignedToken.Value);
          break;
        case "binc":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          BlackIncrementMS = ParseUInt16(sLower, parser.UnsignedToken.Value);
          break;
        case "movestogo":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          MovesToGo = ParseUInt16(sLower, parser.UnsignedToken.Value);
          break;
        case "depth":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          Plies = ParseByte(sLower, parser.UnsignedToken.Value);
          IsDepth = true;
          IsInfinite = false;
          break;
        case "nodes":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          Nodes = ParseUInt64(sLower, parser.UnsignedToken.Value);
          IsInfinite = false;
          break;
        case "mate":
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          MovesToMate = ParseUInt16(sLower, parser.UnsignedToken.Value);

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
          parser.SpaceToken.Expect();
          parser.UnsignedToken.Expect();
          MoveTimeMS = ParseUInt32(sLower, parser.UnsignedToken.Value);
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

        if (!parser.SpaceToken.Accept()) break;
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

  public Boolean IsDepth;               // Depth Bound explicitly set vs implied by MovesToMate
  public Boolean IsInfinite;
  public Boolean IsPonder;
  public Boolean WTM;
  #endregion
}
