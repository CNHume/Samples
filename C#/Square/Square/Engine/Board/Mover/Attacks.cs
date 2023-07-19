//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
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

namespace Engine;

using Exceptions;

//
// Type Aliases:
//
using Plane = UInt64;

partial class Board {
  #region Methods
  //
  // Attack Methods:
  //
  // BuildAtxTo
  //
  // PieceAtxTo - Used by abbreviate to detect potential move ambiguity
  // PieceAtx - Called by BuildMove on behalf of parsePACNMove to verify From and To
  //
  // CanOO
  // CanOOO
  // CanCastle
  //
  // Count Methods:
  //
  // incTo
  // decTo
  //
  #region Attack Methods
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
  [Conditional("BuildAtxTo")]
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
                    OrthPiece & orthAtx(nTo);

      foreach (var side in Side)
        qpFrom |= side.PawnAtxTo(nTo);

      AtxTo[nTo] = qpFrom;
    }
  }
#endif
  //
  // The following is currently only needed for abbreviate().  It finds all
  // pieces of the specified type (for the side to move) which "attack" nTo.
  //
  protected Plane PieceAtxTo(Int32 nFrom, Int32 nTo, Piece piece, Boolean bCapture) {
    const String methodName = nameof(PieceAtxTo);
    // Calculate AtxTo[nTo]
    Plane qpPiece;
    if (piece != Piece.P && piece != Piece.K) {
      // King and Pawn are handled below
      qpPiece = Friend.Piece;

      //
      //[Future]IsLegal() might maintain LegalTo[] to ignore pinned pieces;
      // but abbreviating on this basis in a PGN may confuse some programs.
      //
      switch (piece) {                 // All pieces of the type that moved
      case Piece.N:
        qpPiece &= Knight & AtxKnight[nTo];
        break;
      case Piece.R:
        qpPiece &= Rook & RayOrth(nTo);
        break;
      case Piece.B:
        qpPiece &= Bishop & RayDiag(nTo);
        break;
      case Piece.Q:
        qpPiece &= Queen & Ray(nTo);
        break;
      default:
        qpPiece = 0UL;
        throw new PieceException($"Unexpected Piece = {piece} [{methodName}]");
      }
    }
    else if (piece == Piece.P && bCapture)
      qpPiece = Friend.PawnAtxTo(nTo);
    else
      qpPiece = bit(nFrom);             // King Moves and Pawn Advances are unambiguous

    return qpPiece;
  }

  //
  // The PieceAtx() and CanCastle() methods are used by parsePACNMove() to
  // validate moves entered in Pure Algebraic Coordinate Notation (PACN):
  //
  public Plane? PieceAtx(Byte vPiece, Int32 nFrom, Boolean bCapture) {
    // Obtain possible Moves [and Captures]
    var qpAtxTo = vPiece switch {
      vP6 => Friend.PawnTo(nFrom, bCapture),
      vK6 => AtxKing[nFrom],
      vN6 => AtxKnight[nFrom],
      vB6 => RayDiag(nFrom),
      vR6 => RayOrth(nFrom),
      vQ6 => Ray(nFrom),
      _ => default,
    };

    return qpAtxTo;
  }

  protected Boolean CanOO() {
    var rule = Friend.Parameter.Rule;
    var bLegal = Friend.FlagsSide.Has(SideFlags.CanOO) &&
                 (rule.OOPath & RankPiece) == 0 &&
                 rule.OOSafe.HasValue &&
                 !Foe.IsAttacked(rule.OOSafe.Value);
    return bLegal;
  }

  protected Boolean CanOOO() {
    var rule = Friend.Parameter.Rule;
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
      // Verify Right, Path, and Safety if castling
      //
      var rule = Friend.Parameter.Rule;

      if (nKingTo == rule.KingOOTo)
        bLegal = CanOO();
      else if (nKingTo == rule.KingOOOTo)
        bLegal = CanOOO();
    }

    return bLegal;
  }

  public Boolean CanPromote() {
    var qpPawn = Friend.Piece & Pawn;
    var parameter = Friend.Parameter;
    var qpAdvance1 = qpPawn << parameter.PawnStep & ~RankPiece & parameter.PromotionMask;
    var qpCapture = Foe.Piece & parameter.PromotionMask;

    return qpAdvance1 != 0 ||
           (qpCapture & Friend.PawnA1H8Atx) != 0 ||
           (qpCapture & Friend.PawnA8H1Atx) != 0;
  }
  #endregion                            // Attack Methods

  #region Count Methods
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
  #endregion                            // Count Methods
  #endregion                            // Methods
}
