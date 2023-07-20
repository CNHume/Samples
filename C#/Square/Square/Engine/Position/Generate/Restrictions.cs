//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2022-08-09 CNHume]Split Restriction Methods into their own file
//
// Conditionals:
//
//#define DebugMove
//#define ExemptPasserPin

using System.Diagnostics;

namespace Engine;
//
// Type Aliases:
//
using Plane = UInt64;

partial class Position : Board {
  #region Methods
  //
  // Pin Restriction Methods:
  //
  // restrictPiece - Called once a pin is detected
  //
  #region Pin Restrictions
  //
  //[Note]PlayMove() has called toggleWTM() which inverts the
  // sense of Friend and Foe to complete the current position.
  //
  private void restrictPiece(Move move) {
#if DebugMove
    unpackMove1(move, out Sq sqFrom, out Sq sqTo, out Piece piece1,
                out Piece promotion, out Boolean bCapture);
#endif
    unpack1(move, out Int32 nFrom, out Int32 nTo,
            out UInt32 uPiece, out Boolean _);
    var piece = (Piece)uPiece;
    var qpFrom = bit(nFrom);
    if (piece == Piece.K || (qpFrom & pinnedPiece) != 0)
      return;

    //
    // Determine how a piece is pinned, given that it has just
    // made an Illegal Move, and restrict its further movement
    // so as not to violate the pin.
    //
    // tryMove() skips pinnedPiece moves not marked as allowed
    // in restricted[].
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
    var bSingleCheck = IsOneOrLess(qpChx);
    if (bSingleCheck) {                 //[Safe]
      if (qpChx != 0) {                 // while loop unnecessary
        var qpRay = pinRestrictions(qpChx, vKingPos);

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
        // PasserPin Exemption.  TurnFlags.EPLegal is set only if an En Passant move can be
        // legally played.
        //
        var bPasserPin = false;
#if ExemptPasserPin
        var uCapture = Captured(move);
        var capture = (Piece)uCapture;
        if (capture == Piece.EP) {
          var nCaptureFrom = nTo + Foe.Parameter.PawnStep;
          var qpPasser = bit(nCaptureFrom);
          bPasserPin = (qpRay & qpPasser) != 0;
        }
#endif
        if (bPasserPin) {
          Trace.Assert(!bPasserPin, "Passer Pin");
          DisplayCurrent("Passer Pin");
        }
        else
          addRestriction(nFrom, qpFrom, qpRay, qpChx);
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
    if (bFromPin) {                     //[Safe]
      restricted[nFrom] = qpCheck | qpRay;
      pinnedPiece |= qpFrom;            // Mark restricted[nFrom] valid
    }
    else if (State.IsSearchInProgress) {
      // Diagnose Engine Generated Moves, not User Moves made via ParsePACNMakeMoves()
      Debug.Assert(bFromPin, "Move fails to evade check");
      //[Debug]DisplayCurrent(nameof(addRestriction));
    }
  }
  #endregion                            // Pin Restrictions
  #endregion                            // Methods
}
