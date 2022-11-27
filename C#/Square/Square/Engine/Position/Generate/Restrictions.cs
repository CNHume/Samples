//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2022-08-09 CNHume]Split Restriction Methods into their own file
//
// Conditionals:
//
//#define DebugMove
//#define ExemptPasserPin

namespace Engine {
  using System;
  using System.Diagnostics;

  //
  // Type Aliases:
  //
  using Plane = UInt64;

  partial class Position : Board {
    //
    // Pin Restriction Methods:
    //
    // restrictPiece - Called once a pin is detected
    // interpositions - Thorough version of pinRestrictions, used to find interpositions when in SingleCheck
    //*pinRestrictions - Expedited version of interpositions
    //
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
      var qpCheck = bit(nChx);

      if ((OrthPiece & qpCheck) != 0) { // Checker can move like a Rook
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

      if ((OrthPiece & qpCheck) != 0) { // Checker can move like a Rook
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

    //
    //[Note]playMove() has called toggleWTM() which inverts the
    // sense of Friend and Foe to complete the current position.
    //
    protected void restrictPiece(Move move) {
#if DebugMove
      unpackMove1(move, out sq sqFrom, out sq sqTo, out Piece piece1,
                  out Piece promotion, out Boolean bCapture);
#endif
      unpack1(move, out Int32 nFrom, out Int32 nTo,
              out UInt32 uPiece, out Boolean _);
      var piece = (Piece)uPiece;
      var qpFrom = bit(nFrom);
      if (piece == Piece.K || (qpFrom & PinnedPiece) != 0)
        return;

      //
      // Determine how a piece is pinned, given that it has just
      // made an Illegal Move, and restrict its further movement
      // so as not to violate the pin.
      //
      // tryMove() skips PinnedPiece moves not marked as allowed
      // in Restricted[].
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
          // Removing a passer cannot constitute a pin along a file because the guard pawn will
          // reoccupy the square behind the passer on that file, thus preventing discovery of a
          // check.
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
          // PasserPin Exemption.  TurnFlags.Passed is only set where a legal En Passant move
          // can be played.
          //
          var bPasserPin = false;
#if ExemptPasserPin
          var uCapture = captured(move);
          var capture = (Piece)uCapture;
          if (capture == Piece.EP) {
            var nCaptureFrom = nTo + foe.Parameter.ShiftRank;
            var qpPasser = bit(nCaptureFrom);
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
      else if (State!.IsSearchInProgress) {
        // Diagnose Engine Generated Moves, not User Moves made via ParsePACNMakeMoves()
        Debug.Assert(bFromPin, "Move fails to evade check");
        //[Debug]DisplayCurrent("restrictPiece()");
      }
    }
    #endregion                          // Pin Restrictions
  }
}