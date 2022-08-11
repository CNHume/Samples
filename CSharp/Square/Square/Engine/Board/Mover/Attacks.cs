//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split Attack Methods into their own file
//
// Conditionals:
//
//#define BuildAtxTo
//#define Controlled
//#define DisplayAtxFrom
//#define DisplayAtxTo
//#define DisplayRayImage
//#define LoadUpdateTo

namespace Engine {
  using Exceptions;

  using System;
  using System.Diagnostics;

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Board {
    //
    // Attack Methods:
    //
    // incTo
    // decTo
    // BuildAtxTo
    //
    // rankPath - Returns mask for squares that must not be obstructed (or attacked)
    //
    // PieceAtxTo - Used by abbreviate to detect potential move ambiguity
    // PieceAtx - Called by BuildMove on behalf of parsePACNMove to verify From and To
    //
    // CanOO
    // CanOOO
    // CanCastle
    //
    // getPieceIndex - Returns Piece at any square
    //
    // Move Generators:
    //
    // addCastles
    // addPieceMoves
    // addPieceCaptures
    // addPawnMoves
    // addPawnCaptures
    //
    // Move Methods:
    //
    // movePiece and its related methods:
    // LowerPiece which is called by PlacePiece
    // RaisePiece which is called by RemovePiece
    //
    // Rotation Methods:
    //
    // [set|clr][Piece|Rotations]
    //
    #region Attacks
    //
    // Distinct AtxTo[] and AtxFrom[] may be of help in evaluating Piece
    // Mobility and Square Control.
    //
    // AtxTo[] and AtxFrom[] are duals of each other.
    // They satisfy the following pair of invariants:
    //
    // T[j] = { i | j in F[i] } attacks to square j include the piece on i
    // F[i] = { j | i in T[j] } attacks from a piece on i include square j
    //
    //[Note]These values depend on the type of each piece i and, in the
    // case of a ray piece, on whether the presence of other pieces may
    // impede its progress along a ray.
    //
    // In the method below, an AtxTo[] union is built by considering all
    // types of attack for either side.
    //
#if BuildAtxTo
    protected void BuildAtxTo(Plane qpPieceUpdate) {
      while (qpPieceUpdate != 0) {
        var nTo = RemoveLo(ref qpPieceUpdate);

        //
        // For each square being updated, AtxTo[] should be updated to
        // identify squares with pieces that attack the Indexed Square.
        //
        // These are squares that would be attacked by pieces standing
        // on the Indexed Square using each type of attack that pieces
        // on the AtxTo[] square are capable of:
        //
        var qpFrom = King & KingAtx[nTo] |
                     Knight & KnightAtx[nTo] |
                     DiagPiece & diagAtx(nTo) |
                     RectPiece & rectAtx(nTo);

        foreach (var side in Side)
          qpFrom |= side.PawnAtxTo(nTo);

        AtxTo[nTo] = qpFrom;
      }
    }
#endif
    private Int32 incTo(Plane qpAtxTo) {
      var nAtx = 0;
#if Controlled
      AttackedSum |= qpAtxTo;
#endif
      while (qpAtxTo != 0) {
        var n = RemoveLo(ref qpAtxTo);
#if Controlled
        ControlTo[n]++;
#endif
        nAtx++;
      }

      return nAtx;
    }

    private Int32 decTo(Plane qpAtxTo) {
      var nAtx = 0;
#if Controlled
      AttackedSum |= qpAtxTo;
#endif
      while (qpAtxTo != 0) {
        var n = RemoveLo(ref qpAtxTo);
#if Controlled
        ControlTo[n]--;
#endif
        nAtx++;
      }

      return nAtx;
    }

    //
    // The following is currently only needed for abbreviate().  It finds all
    // pieces of the specified type (for the side to move) which "attack" nTo.
    //
    protected Plane PieceAtxTo(Int32 nFrom, Int32 nTo, Byte vPiece, Boolean bCapture) {
      // Calculate AtxTo[nTo]
      Plane qpPiece;
      if (vPiece != vP6 && vPiece != vK6) {
        // King and Pawn are handled below
        qpPiece = Friend.Piece;

        //
        //[Future]IsLegal() might maintain LegalTo[] to ignore pinned pieces;
        // but abbreviating on this basis in a PGN may confuse some programs.
        //
        switch (vPiece) {               // All pieces of the type that moved
        case vN6:
          qpPiece &= Knight & KnightAtx[nTo];
          break;
        case vR6:
          qpPiece &= Rook & rectAtx(nTo);
          break;
        case vB6:
          qpPiece &= Bishop & diagAtx(nTo);
          break;
        case vQ6:
          qpPiece &= Queen;
          qpPiece = qpPiece & (diagAtx(nTo) | rectAtx(nTo));
          break;
        default:
          qpPiece = 0UL;
          throw new PieceException("Unexpected Piece [pieceAtxTo]");
        }
      }
      else if (vPiece == vP6 && bCapture)
        qpPiece = Friend.PawnAtxTo(nTo);
      else
        qpPiece = BIT0 << nFrom;        // King Moves and Pawn Advances are unambiguous

      return qpPiece;
    }

    //
    // The PieceAtx() and CanCastle() methods are used by parsePACNMove() to
    // validate moves entered in Pure Algebraic Coordinate Notation (PACN):
    //
    public Plane? PieceAtx(Byte vPiece, Int32 nFrom, Boolean bCapture) {
      Plane? qpPieceAtx = default;

      // Obtain possible Moves [and Captures]
      switch (vPiece) {
      case vP6:
        qpPieceAtx = Friend.PawnTo(nFrom, bCapture);
        break;
      case vK6:
        qpPieceAtx = KingAtx[nFrom];
        break;
      case vN6:
        qpPieceAtx = KnightAtx[nFrom];
        break;
      case vB6:
        qpPieceAtx = diagAtx(nFrom);
        break;
      case vR6:
        qpPieceAtx = rectAtx(nFrom);
        break;
      case vQ6:
        qpPieceAtx = diagAtx(nFrom) | rectAtx(nFrom);
        break;
      }

      return qpPieceAtx;
    }

    protected Boolean CanOO() {
      var rule = Friend.Rule;
      var bLegal = Friend.FlagsSide.Has(SideFlags.CanOO) &&
                   (rule.OOPath & RankPiece) == 0 &&
                   rule.OOSafe.HasValue &&
                   !Foe.IsAttacked(rule.OOSafe.Value);
      return bLegal;
    }

    protected Boolean CanOOO() {
      var rule = Friend.Rule;
      var bLegal = Friend.FlagsSide.Has(SideFlags.CanOOO) &&
                   (rule.OOOPath & RankPiece) == 0 &&
                   rule.OOOSafe.HasValue &&
                   !Foe.IsAttacked(rule.OOOSafe.Value);
      return bLegal;
    }

    // Used by parsePACNMove()
    protected Boolean CanCastle(Int32 nKingTo) {
      var bLegal = false;

      if (!InCheck()) {
        //
        // Verify Right, Path and Safety if castling
        //
        var rule = Friend.Rule;

        if (nKingTo == rule.KingOOTo)
          bLegal = CanOO();
        else if (nKingTo == rule.KingOOOTo)
          bLegal = CanOOO();
      }

      return bLegal;
    }

    public Boolean CanPromote() {
      var qpPawn = Friend.Piece & Pawn;
      var qpAdvance1 = qpPawn << Friend.Parameter.ShiftRank & ~RankPiece & Friend.Parameter.RankLast;
      var qpCapture = Foe.Piece & Friend.Parameter.RankLast;

      return qpAdvance1 != 0 ||
             (qpCapture & Friend.PawnA1H8Atx) != 0 ||
             (qpCapture & Friend.PawnA8H1Atx) != 0;
    }
    #endregion                          // Attacks
  }
}
