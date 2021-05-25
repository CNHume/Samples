//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-02-19 CNHume]Split Attack Methods into their own file
//
// Conditionals:
//
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

  using static CastleRule;
  using static Logging.Logger;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;
  using Plane = System.UInt64;

  partial class Position : Board {
    //
    // Attack Methods:
    //
    // restrictPiece - Called once a pin is detected
    // checkers - Find pieces giving check
    // pieceAtxTo - Used by abbreviate to detect potential move ambiguity
    //
    // interpositions - Thorough version of pinRestrictions, used to find interpositions when in SingleCheck
    //*pinRestrictions - Expedited version of interpositions
    //
    // attacks - Thorough version of isAttacked, used to preempt Illegal King Moves
    //*isAttacked - Disallow castling through check
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
    // lowerPiece which is called by placePiece
    // raisePiece which is called by removePiece
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

      incTo(side.PawnA1H8Atx);
      incTo(side.PawnA8H1Atx);
      incTo(KingAtx[side.KingPos.Value]);

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
    // The following are used by abbreviate() to avoid the overhead of buildAtxTo():
    //
    protected Plane pawnAtxTo(BoardSide side, Int32 nTo) {
      var qpFrom = 0UL;
      var qpTo = BIT0 << nTo;

      if ((qpTo & side.PawnA1H8Atx) != 0) qpFrom |= BIT0 << nTo - side.Parameter.ShiftA1H8;
      if ((qpTo & side.PawnA8H1Atx) != 0) qpFrom |= BIT0 << nTo - side.Parameter.ShiftA8H1;

      return qpFrom;
    }

    //
    // The following is currently only needed for abbreviate().  It finds all
    // pieces of the specified type (for the side to move) which "attack" nTo.
    //
    protected Plane pieceAtxTo(Int32 nFrom, Int32 nTo, Byte vPiece, Boolean bCapture) {
      // Calculate AtxTo[nTo]
      Plane qpPiece;
      var bWTM = WTM();
      var side = getSide(bWTM);
      if (vPiece != vP6 && vPiece != vK6) {
        // King and Pawn are handled below
        qpPiece = side.Piece;

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
        qpPiece = pawnAtxTo(side, nTo);
      else
        qpPiece = BIT0 << nFrom;        // King Moves and Pawn Advances are unambiguous

      return qpPiece;
    }

    //
    // The pieceAtx() and canCastle() methods are used by parsePACNMove() to
    // validate moves entered in Pure Algebraic Coordinate Notation (PACN):
    //
    private Plane pawnAtx(Int32 nFrom, Boolean bCapture) {
      Plane qpPieceAtx;
      var bWTM = WTM();
      var side = getSide(bWTM);
      var qpFrom = BIT0 << nFrom;

      if (bCapture) {
        var qpA1H8Atx = shiftl(qpFrom & ~side.Parameter.FileRight, side.Parameter.ShiftA1H8);
        var qpA8H1Atx = shiftl(qpFrom & ~side.Parameter.FileLeft, side.Parameter.ShiftA8H1);
        qpPieceAtx = qpA1H8Atx | qpA8H1Atx;
      }
      else {
        var qpAdvance1 = shiftl(qpFrom, side.Parameter.ShiftRank) & ~RankPiece;
        var qpAdvance2 = shiftl(qpAdvance1 & side.Parameter.RankPass, side.Parameter.ShiftRank) & ~RankPiece;
        qpPieceAtx = qpAdvance1 | qpAdvance2;
      }

      return qpPieceAtx;
    }

    protected Plane? pieceAtx(Byte vPiece, Int32 nFrom, Boolean bCapture) {
      Plane? qpPieceAtx = default;

      // Obtain possible Moves [and Captures]
      switch (vPiece) {
      case vP6:
        qpPieceAtx = pawnAtx(nFrom, bCapture);
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

    //
    // checkers() is used to distinguish between single vs double checks,
    // to determine whether an Interposition (or Capture) may be possible
    // or whether only Evasion is to be considered.
    //
    // qpFrom returns squares with pieces that attack the opposing King.
    //
    // Pawn attacks are handled conventionally; but the non-directional,
    // symmetric nature of piece attacks allows attacks to be generated
    // at the To square.  An interesection can than be made with actual
    // pieces and the attacks generated for their type.
    //
    protected Plane checkers(Boolean bWTM) {
      (BoardSide friend, BoardSide foe) = getSides(bWTM);

      var qpFrom = 0UL;
      var vTo = friend.KingPos.Value;

      qpFrom |= foe.Piece & King & KingAtx[vTo];
      qpFrom |= foe.Piece & Knight & KnightAtx[vTo];
      qpFrom |= foe.Piece & DiagPiece & diagAtx(vTo);
      qpFrom |= foe.Piece & RectPiece & rectAtx(vTo);

      var qpTo = friend.Piece & King;
      if ((qpTo & foe.PawnA1H8Atx) != 0) qpFrom |= BIT0 << vTo - foe.Parameter.ShiftA1H8;
      if ((qpTo & foe.PawnA8H1Atx) != 0) qpFrom |= BIT0 << vTo - foe.Parameter.ShiftA8H1;

      return qpFrom;
    }

    //
    // The following is used to preempt Illegal King Moves.
    // Allowing the moves, then handling them like Illegal
    // Moves may be just as fast.
    //
    protected Plane attacks(BoardSide foe, Plane qpFriend) {
      var qpTo = 0UL;

      qpTo |= (qpFriend & foe.PawnA1H8Atx);
      qpTo |= (qpFriend & foe.PawnA8H1Atx);

      while (qpFriend != 0) {
        var n = RemoveLo(ref qpFriend, out Plane qp);

        var bAttacked =
          (foe.Piece & Knight & KnightAtx[n]) != 0 ||
          (foe.Piece & DiagPiece & diagAtx(n)) != 0 ||
          (foe.Piece & RectPiece & rectAtx(n)) != 0 ||
          (foe.Piece & King & KingAtx[n]) != 0;

        if (bAttacked)
          qpTo |= qp;
      }

      return qpTo;
    }

    //
    // isAttacked() is used by the Legal Move and Check tests
    // and to disallow castling through check:
    //
    protected Boolean isAttacked(BoardSide foe, Plane qpFriend) {
      Boolean bAttacked =
        (qpFriend & foe.PawnA1H8Atx) != 0 ||
        (qpFriend & foe.PawnA8H1Atx) != 0;

      while (!bAttacked && qpFriend != 0) {
        var n = RemoveLo(ref qpFriend);

        bAttacked =
          (foe.Piece & Knight & KnightAtx[n]) != 0 ||
          (foe.Piece & DiagPiece & diagAtx(n)) != 0 ||
          (foe.Piece & RectPiece & rectAtx(n)) != 0 ||
          (foe.Piece & King & KingAtx[n]) != 0;
      }

      return bAttacked;
    }

    private Boolean canOO(
      BoardSide friend, CastleRuleParameter friendRule, BoardSide foe) {
      var bLegal = ((friend.FlagsHi & HiFlags.CanOO) != 0) &&
                   ((friendRule.OOPath & RankPiece) == 0) &&
                   !isAttacked(foe, friendRule.OOSafe.Value);

      return bLegal;
    }

    private Boolean canOOO(
      BoardSide friend, CastleRuleParameter friendRule, BoardSide foe) {
      var bLegal = ((friend.FlagsHi & HiFlags.CanOOO) != 0) &&
                   ((friendRule.OOOPath & RankPiece) == 0) &&
                   !isAttacked(foe, friendRule.OOOSafe.Value);

      return bLegal;
    }

    // Used by parsePACNMove()
    protected Boolean canCastle(Boolean bWTM, Int32 nKingTo) {
      var bLegal = false;
      if (InCheck()) return bLegal;

      (BoardSide friend, BoardSide foe) = getSides(bWTM);
      var friendRule = getRule(bWTM);

      //
      // Verify Right, Path and Safety if castling
      //
      if (nKingTo == friendRule.KingOOTo)
        bLegal = canOO(friend, friendRule, foe);
      else if (nKingTo == friendRule.KingOOOTo)
        bLegal = canOOO(friend, friendRule, foe);

      return bLegal;
    }

    public Boolean canPromote() {
      var bWTM = WTM();
      (BoardSide friend, BoardSide foe) = getSides(bWTM);

      var qpPawn = friend.Piece & Pawn;
      var qpAdvance1 = qpPawn << friend.Parameter.ShiftRank & ~RankPiece & friend.Parameter.RankLast;
      var qpCapture = foe.Piece & friend.Parameter.RankLast;

      return qpAdvance1 != 0 ||
             (qpCapture & friend.PawnA1H8Atx) != 0 ||
             (qpCapture & friend.PawnA8H1Atx) != 0;
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

    protected void restrictPiece(Move move) {
      unpack1(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out Boolean _);
      var piece = (Piece)uPiece;
      var qpFrom = BIT0 << nFrom;

      if ((PinnedPiece & qpFrom) == 0 && piece != Piece.K) {
        var bWhiteMoved = !WTM();

        //
        // Determine how a piece is pinned, given that it has just
        // made an Illegal Move, and restrict its further movement
        // so as not to violate the pin.  tryMove() skips PinnedPiece
        // moves not marked in Restricted[] as being allowed.
        //
        var qpChx = checkers(bWhiteMoved);

        //
        // Pieces in Chess move such that a given piece can be pinned to its
        // King by at most one attacker at a time.  So, a Double Check would
        // imply that the previous move was not only pinned but that it also
        // failed to evade a Check.
        //
        var bSingleCheck = OneBitOrNone(qpChx);
        if (bSingleCheck) {             //[Safe]
          var vKingPos = getKingPos(bWhiteMoved);
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
            if (captured(move) == Piece.EP) {
              var foe = getSide(!bWhiteMoved);
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
    }

    private void addRestriction(Int32 nFrom, Plane qpFrom, Plane qpRay, Plane qpCheck) {
      // Test for Pin Restriction, subject to the PasserPin Exemption
      var bFromPin = (qpRay & qpFrom) != 0;
      if (bFromPin) {           //[Safe]
        Restricted[nFrom] = qpCheck | qpRay;
        PinnedPiece |= qpFrom;  // Mark Restricted[nFrom] valid
      }
      else if (!bFromPin) {
        // Illegal Move should not have been considered because it ignored an existing Check
        Debug.Assert(bFromPin, "Move failed to avoid check");
        //[Debug]DisplayCurrent("restrictPiece()");
      }
    }
    #endregion
  }
}
