//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split Attack Methods into their own file
//
// Conditionals:
//
//#define DebugMove
//#define ExemptPasserPin
//#define BuildAtxTo
//#define DisplayAtxFrom
//#define DisplayAtxTo
//#define DisplayRayImage
//#define LoadUpdateTo
//#define Controlled

namespace Engine {
  using Exceptions;

  using System;
  using System.Diagnostics;

  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Eval = Int16;
  using Plane = UInt64;

  partial class Position : Board {
    //
    // Attack Methods:
    //
    // restrictPiece - Called once a pin is detected
    // Checkers - Find pieces giving check
    // pieceAtxTo - Used by abbreviate to detect potential move ambiguity
    //
    // interpositions - Thorough version of pinRestrictions, used to find interpositions when in SingleCheck
    //*pinRestrictions - Expedited version of interpositions
    //
    // attacks - Thorough version of IsAttacked, used to preempt Illegal King Moves
    //*IsAttacked - Disallow castling through check
    // rankPath - Returns mask for squares that must not be obstructed (or attacked)
    //
    // pieceAtx - Called by buildMove on behalf of parsePACNMove to verify From and To
    // pawnAtx - Called by pieceAtx
    //
    // getPiece - Returns Piece at any square
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
    protected void buildAtxTo(Plane qpPieceUpdate) {
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
          qpFrom |= pawnAtxTo(side, nTo);

        AtxTo[nTo] = qpFrom;
      }
    }
#endif
    //
    // To assess Mobility and Square Control:
    // Pseudo Attacks are counted for both sides.
    // Pawn Advances and Castling are not included.
    //
    protected Eval mobility() {
      var nControlValue = 0;
      var nMobileValue = 0;
#if Controlled
      var nControlTotal = 0;
      var nControlDelta = 0;

      // The following bit planes are only used to determine which side controls a given square:
      AttackedSum =
        BlackControlled =
        WhiteControlled = 0UL;

      Array.Clear(ControlTo, 0, ControlTo.Length);
#endif
      var nWhiteAtx = atxCount(Side[White]);
      var nBlackAtx = atxCount(Side[Black]);
#if Controlled
      var qpAtx = AttackedSum;
      while (qpAtx != 0) {
        Plane qpTo;
        var n = RemoveLo(ref qpAtx, out qpTo);
        var z = ControlTo[n];
        if (z > 0) {
          WhiteControlled |= qpTo;
          nControlDelta += Importance[n];
          nControlTotal += Importance[n];
        }
        else if (z < 0) {
          BlackControlled |= qpTo;
          nControlDelta -= Importance[n];
          nControlTotal += Importance[n];
        }
      }
#if TestControlled
      DisplayCurrent("mobility()");

      LogLine("WhiteControlled\n");
      writeRect(WhiteControlled);
      LogLine();

      LogLine("BlackControlled\n");
      writeRect(BlackControlled);
      LogLine();

      var qpNeutral = AttackedSum & ~(WhiteControlled | BlackControlled);
      LogLine("Neutral\n");
      writeRect(qpNeutral);
      LogLine();
#endif
      nControlValue = 2 * mPawnWeight * nControlDelta / nControlTotal;
#endif
      var nMobileTotal = nWhiteAtx + nBlackAtx;
      if (nMobileTotal != 0) {
        var nMobileDelta = nWhiteAtx - nBlackAtx;
        nMobileValue = 2 * mPawnWeight * nMobileDelta / nMobileTotal;
      }

      return (Eval)(nControlValue + nMobileValue);
    }

    protected Int32 atxCount(BoardSide side) {
      var nAtx = 0;
      if (!side.KingPos.HasValue)
        throw new ArgumentException(nameof(side.KingPos), "Invalid King Position");
      incTo(KingAtx[side.KingPos.Value]);

      incTo(side.PawnA1H8Atx);
      incTo(side.PawnA8H1Atx);

      var qpAtxFrom = side.Piece & Knight;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += incTo(KnightAtx[n]);
      }

      qpAtxFrom = side.Piece & Bishop;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += incTo(diagAtx(n));
      }

      qpAtxFrom = side.Piece & Rook;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += incTo(rectAtx(n));
      }

      qpAtxFrom = side.Piece & Queen;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += incTo(diagAtx(n) | rectAtx(n));
      }

      return nAtx;
    }

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
    protected Plane pieceAtxTo(Int32 nFrom, Int32 nTo, Byte vPiece, Boolean bCapture) {
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
    // The pieceAtx() and canCastle() methods are used by parsePACNMove() to
    // validate moves entered in Pure Algebraic Coordinate Notation (PACN):
    //
    protected Plane? pieceAtx(Byte vPiece, Int32 nFrom, Boolean bCapture) {
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

    private Boolean canOO() {
      var rule = Friend.Rule;
      var bLegal = Friend.FlagsSide.Has(SideFlags.CanOO) &&
                   (rule.OOPath & RankPiece) == 0 &&
                   rule.OOSafe.HasValue &&
                   !Foe.IsAttacked(rule.OOSafe.Value);
      return bLegal;
    }

    private Boolean canOOO() {
      var rule = Friend.Rule;
      var bLegal = Friend.FlagsSide.Has(SideFlags.CanOOO) &&
                   (rule.OOOPath & RankPiece) == 0 &&
                   rule.OOOSafe.HasValue &&
                   !Foe.IsAttacked(rule.OOOSafe.Value);
      return bLegal;
    }

    // Used by parsePACNMove()
    protected Boolean canCastle(Int32 nKingTo) {
      var bLegal = false;

      if (!InCheck()) {
        //
        // Verify Right, Path and Safety if castling
        //
        var rule = Friend.Rule;

        if (nKingTo == rule.KingOOTo)
          bLegal = canOO();
        else if (nKingTo == rule.KingOOOTo)
          bLegal = canOOO();
      }

      return bLegal;
    }

    public Boolean canPromote() {
      var qpPawn = Friend.Piece & Pawn;
      var qpAdvance1 = qpPawn << Friend.Parameter.ShiftRank & ~RankPiece & Friend.Parameter.RankLast;
      var qpCapture = Foe.Piece & Friend.Parameter.RankLast;

      return qpAdvance1 != 0 ||
             (qpCapture & Friend.PawnA1H8Atx) != 0 ||
             (qpCapture & Friend.PawnA8H1Atx) != 0;
    }
    #endregion

    #region Pin Restrictions
    //
    // The following is used to generate Check Evasions;
    // and is similar to pinRestrictions()
    //
    //[Note]Exact intersections are needed here to choose interpositions;
    // but can be omitted where Restricted[] marks how a Piece is Pinned.
    // This is because pieces are not allowed to leap over their King.
    //
    protected Plane interpositions(Int32 nChx, Int32 nKing) {
      var qpRay = 0UL;
      var qpCheck = BIT0 << nChx;

      if ((RectPiece & qpCheck) != 0) { // Checker can move like a Rook
        if (((qpRay = rankAtx(nKing)) & qpCheck) != 0)
          return qpRay & rankAtx(nChx);
        else if (((qpRay = fileAtx(nKing)) & qpCheck) != 0)
          return qpRay & fileAtx(nChx);
      }

      // Look for diagAtx() in case qpCheck is a Queen
      if ((DiagPiece & qpCheck) != 0) { // Checker can move like a Bishop
        if (((qpRay = a1h8Atx(nKing)) & qpCheck) != 0)
          return qpRay & a1h8Atx(nChx);
        else if (((qpRay = a8h1Atx(nKing)) & qpCheck) != 0)
          return qpRay & a8h1Atx(nChx);
      }

      return qpRay;
    }

    //
    // The following is used to recognize Pin Restrictions;
    // and is similar to interpositions()
    //
    //[Note]Exact intersections are needed to find interpositions; but
    // can be omitted here, where Restricted[] marks a Piece as Pinned.
    // This is because pieces are not allowed to leap over their King.
    //
    protected Plane pinRestrictions(Plane qpCheck, Int32 nKing) {
      var qpRay = 0UL;                  // Return Value

      if ((RectPiece & qpCheck) != 0) { // Checker can move like a Rook
        if (((qpRay = rankAtx(nKing)) & qpCheck) != 0)
          return qpRay;
        else if (((qpRay = fileAtx(nKing)) & qpCheck) != 0)
          return qpRay;
      }

      // Look for diagAtx() in case qpCheck is a Queen
      if ((DiagPiece & qpCheck) != 0) { // Checker can move like a Bishop
        if (((qpRay = a1h8Atx(nKing)) & qpCheck) != 0)
          return qpRay;
        else if (((qpRay = a8h1Atx(nKing)) & qpCheck) != 0)
          return qpRay;
      }

      return qpRay;
    }

    //[Note]toggleWTM() inverts the conventional sense of Friend and Foe.
    protected void restrictPiece(Move move) {
#if DebugMove
      unpackMove1(move, out sq sqFrom, out sq sqTo, out Piece piece1, out Piece promotion, out Boolean bCapture);
#endif
      unpack1(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out Boolean _);
      var piece = (Piece)uPiece;
      var qpFrom = BIT0 << nFrom;
      if (piece == Piece.K || (qpFrom & PinnedPiece) != 0)
        return;

      //
      // Determine how a piece is pinned, given that it has just
      // made an Illegal Move, and restrict its further movement
      // so as not to violate the pin.  tryMove() skips PinnedPiece
      // moves not marked in Restricted[] as being allowed.
      //
      byte vKingPos = Foe.GetKingPos();
      var qpKing = Foe.Piece & King;
      var qpChx = Friend.Checkers(vKingPos, qpKing);

      //
      // Pieces in Chess move such that a given piece can be pinned to its
      // King by at most one attacker at a time.  So, a Double Check would
      // imply that the previous move was not only pinned but that it also
      // failed to evade a Check.
      //
      var bSingleCheck = IsOneOrNone(qpChx);
      if (bSingleCheck) {             //[Safe]
        if (qpChx != 0) {             // while loop unnecessary
          var nChx = RemoveLo(ref qpChx, out Plane qpCheck);
          var qpRay = pinRestrictions(qpCheck, vKingPos);

          //
          // A guard pawn on the 4th or 5th rank may be prevented from EP Capture of a passer due
          // to a pin against its King along their shared diagonal or along a shared rank or file.
          //
          // Removing the passer cannot discover a check along a diagonal, because there would
          // necessarily have been that same check along the diagonal in the position prior to
          // the passer advance.  That position would therefore have been Illegal.
          //
          // Removing a passer cannot constitute a pin along a file, because the guard pawn will
          // immediately reoccupy that file in a the square behind the passer which will prevent
          // discovery of any check along that file.
          //
          // The PasserPin exemption was introduced to prevent a mistaken Pin Restriction along
          // the 4th or 5th rank from being applied in cases where a guard pawn vacates its own
          // From Square as well as removing the passer.  Though the pin may render EP Captures
          // illegal, the regular pawn advance would not be pinned owing to the presence of the
          // adjacent passer.
          //
          // There can be no "pin" across a rank when two guard pawns surround a passer, because
          // the three Pawns will be adjacent; and either EP Capture removes at most one pair of
          // them.
          //
          // tryEP() was added for proper Draw3 handling; and has eliminated the need for the
          // PasserPin Exemption.  This is because LoFlags.Passed is only set if at least one
          // legal En Passant move can be played.
          //
          var bPasserPin = false;
#if ExemptPasserPin
          var uCapture = captured(move);
          var capture = (Piece)uCapture;
          if (capture == Piece.EP) {
            var nCaptureFrom = nTo + foe.Parameter.ShiftRank;
            var qpPasser = BIT0 << nCaptureFrom;
            bPasserPin = (qpRay & qpPasser) != 0;
          }
#endif
          if (bPasserPin) {
            Trace.Assert(!bPasserPin, "Passer Pin");
            DisplayCurrent("Passer Pin");
          }
          else
            addRestriction(nFrom, qpFrom, qpRay, qpCheck);
        }
      }
      else {
        // Illegal Move should not have been considered and the pin introduces a new Check
        Trace.Assert(bSingleCheck, "Pin found on Non Evading Move");
      }
    }

    private void addRestriction(Int32 nFrom, Plane qpFrom, Plane qpRay, Plane qpCheck) {
      // Test for Pin Restriction, subject to the PasserPin Exemption
      var bFromPin = (qpRay & qpFrom) != 0;
      if (bFromPin) {                   //[Safe]
        Restricted[nFrom] = qpCheck | qpRay;
        PinnedPiece |= qpFrom;          // Mark Restricted[nFrom] valid
      }
      else if (State.IsSearchInProgress) {
        // Diagnose Engine Generated Moves, not User Moves made via ParsePACNMakeMoves()
        Debug.Assert(bFromPin, "Move fails to evade check");
        //[Debug]DisplayCurrent("restrictPiece()");
      }
    }
    #endregion
  }
}
