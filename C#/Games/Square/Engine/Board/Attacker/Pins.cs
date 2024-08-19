//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2022-12-05 CNHume]Created File
//
// Conditionals:
//

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
  // interpositions - Thorough version of pinRestrictions, used to find interpositions when in SingleCheck
  //*pinRestrictions - Expedited version of interpositions
  //
  #region Pin Restrictions
  //
  // The following is used to generate Check Evasions;
  // and is similar to pinRestrictions()
  //
  //[Note]Exact intersections are needed here to choose interpositions;
  // but can be omitted where restricted[] marks how a Piece is Pinned
  // because pieces are not allowed to leap over their King.
  //
  // Interpositions exclude nChx square of the checking piece.
  //
  private Plane interpositions(Int32 nChx, Int32 nKing) {
    var qpRay = 0UL;
    var qpCheck = bit(nChx);

    if ((OrthPiece & qpCheck) != 0) {   // Checker can move like a Rook
      if (((qpRay = RayRank(nKing)) & qpCheck) != 0)
        return qpRay & RayRank(nChx);
      else if (((qpRay = RayFile(nKing)) & qpCheck) != 0)
        return qpRay & RayFile(nChx);
    }

    // Look for RayDiag() in case qpCheck is a Queen
    if ((DiagPiece & qpCheck) != 0) {   // Checker can move like a Bishop
      if (((qpRay = RayA1H8(nKing)) & qpCheck) != 0)
        return qpRay & RayA1H8(nChx);
      else if (((qpRay = RayA8H1(nKing)) & qpCheck) != 0)
        return qpRay & RayA8H1(nChx);
    }

    return qpRay;
  }

  //
  // The following is used to recognize Pin Restrictions;
  // and is similar to interpositions()
  //
  //[Note]Exact intersections are needed to find interpositions; but
  // can be omitted here, where restricted[] marks a Piece as Pinned.
  // This is because pieces are not allowed to leap over their King.
  //
  // pinRestrictions include qpCheck of the checking piece.
  //
  //[Assume]IsOneOrLess(qpCheck)
  //
  private Plane pinRestrictions(Plane qpCheck, Int32 nKing) {
    var qpRay = 0UL;                    // Return Value

    if ((OrthPiece & qpCheck) != 0) {   // Checker can move like a Rook
      if (((qpRay = RayRank(nKing)) & qpCheck) != 0)
        return qpRay;
      else if (((qpRay = RayFile(nKing)) & qpCheck) != 0)
        return qpRay;
    }

    // Look for RayDiag() in case qpCheck is a Queen
    if ((DiagPiece & qpCheck) != 0) {   // Checker can move like a Bishop
      if (((qpRay = RayA1H8(nKing)) & qpCheck) != 0)
        return qpRay;
      else if (((qpRay = RayA8H1(nKing)) & qpCheck) != 0)
        return qpRay;
    }

    return qpRay;
  }
  #endregion                            // Pin Restrictions
  #endregion                            // Methods
}
