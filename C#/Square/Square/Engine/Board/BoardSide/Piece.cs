//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split Piece into its own file
//
// Conditionals:
//
//#define Magic
#define HashPieces
//#define VerifySquarePiece               // Ensure move from an occupied square to an empty square

using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;

namespace Engine;

using Exceptions;

//
// Type Aliases:
//
using Hashcode = UInt64;
using Plane = UInt64;

partial class Board {
  internal partial class BoardSide {
    #region Methods
    //
    // [clr|set]Piece
    // [raise|lower]
    // [Raise|Remove]Piece
    // [Lower|Place]Piece
    // EPGuard
    // ResetPawnAtx
    // RookCastles
    // SetInsufficient
    // testInsufficient
    // IsAlone
    // GetKingPos
    // Checkers - Find pieces giving check
    // Safe - Thorough version of IsAttacked, used to preempt Illegal King Moves
    //*IsAttacked - Disallow castling through check
    // PawnAtxTo
    // PawnTo - Called by PieceAtx
    // BuildMove
    // AddPawnCaptures
    // AddPawnCaptures2
    // AddPawnMoves
    // AddPromotionMoves
    // [inc|dec]SideCount
    // AtxCount
    // verifyKingCanCastle
    // GrantCastling
    // pieceHash
    // HashPiece
    //
    #region Mover Methods
    //
    // Return Friend Pawns able to capture the Foe Pawn
    // that passed through the En Passant Target square.
    //
    public Plane EPGuard(Byte vEPTarget) {
      var qpCaptureTo = bit(vEPTarget);

      var qpCaptureFrom =
        ShiftR(qpCaptureTo & PawnA1H8Atx, Parameter.PawnA1H8) |
        ShiftR(qpCaptureTo & PawnA8H1Atx, Parameter.PawnA8H1);

      return qpCaptureFrom;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void clrPiece(Plane qp) {
      Board.RankPiece &= ~qp;
      Piece &= ~qp;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void setPiece(Plane qp) {
      Board.RankPiece |= qp;
      Piece |= qp;
    }

    // Reset Pawn[A1H8|A8H1]Atx
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public void ResetPawnAtx() {
      var qpPawn = Piece & Board.Pawn;
      PawnA1H8Atx = ShiftL(qpPawn & ~Parameter.FileRight, Parameter.PawnA1H8);
      PawnA8H1Atx = ShiftL(qpPawn & ~Parameter.FileLeft, Parameter.PawnA8H1);
    }

    public Boolean RaisePiece(Byte vPiece, Int32 nFrom) {
      var qHash = pieceHash(vPiece, nFrom);
      Board.Hash ^= qHash;
      if (vPiece == vP6) Board.HashPawn ^= qHash;
      return raise(vPiece, nFrom);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Boolean raise(Byte vPiece, Int32 nFrom) {
      const String methodName = nameof(raise);
      var qp = bit(nFrom);
#if VerifySquarePiece
      if ((qp & Board.RankPiece) == 0) {
        var sb = new StringBuilder($"Square empty where {Parameter.SideName} Piece was expected at")
          .AppendSquares(qp);
        throw new MoveException(sb.ToString());
      }
      else if ((qp & Piece) == 0) {
        var sb = new StringBuilder($"{Parameter.SideName} Piece was expected at")
          .AppendSquares(qp);
        throw new MoveException(sb.ToString());
      }
#endif
      clrPiece(qp);
#if !Magic
      Board.ClrRotations(nFrom);
#endif
      var bLite = false;
      switch (vPiece) {
      case vP6:
        Board.Pawn &= ~qp;
        break;
      case vR6:
        Board.OrthPiece &= ~qp;
        if (nFrom == Parameter.Rule.RookOOFrom)
          ClrCanOO();
        else if (nFrom == Parameter.Rule.RookOOOFrom)
          ClrCanOOO();
        break;
      case vN6:
        Board.Knight &= ~qp;
        break;
      case vB6:
        Board.DiagPiece &= ~qp;
        bLite = (qp & SquareLite) != 0;
        break;
      case vQ6:
        Board.DiagPiece &= ~qp;
        Board.OrthPiece &= ~qp;
        break;
      case vK6:
        Board.King &= ~qp;
        ClrCanCastle();
        break;
      default:
        throw new PieceException($"Unexpected Piece = {vPiece} [{methodName}]");
      }

      return bLite;
    }

    public void RemovePiece(Byte vPiece, Int32 nFrom) {
      var bLite = RaisePiece(vPiece, nFrom);
      decSideCount(vPiece);

      //
      // Update Pair, assuming Board.DiagPiece is up to date
      //
      if (vPiece == vB6) {
        var qpBishop = Piece & Board.Bishop;

        if (bLite) {
          if ((qpBishop & SquareLite) == 0)
            clrLite();
        }
        else if ((qpBishop & SquareDark) == 0)
          clrDark();
#if HashPieces
        var u = (UInt32)(FlagsSide & SideFlags.Pair) >> nBishopPairBit;
        setTwoBits(ref PieceHash, 0, u);   // Piece == vHF
#endif
      }
#if HashPieces
      if (vP6 < vPiece && vPiece < vK6) {
        var u = PieceCount(vPiece);
        setTwoBits(ref PieceHash, vPiece - vHF, u % vMod4);
      }
#endif
      setInsufficient();
    }

    public Boolean LowerPiece(Byte vPiece, Int32 nTo) {
      var qHash = pieceHash(vPiece, nTo);
      Board.Hash ^= qHash;
      if (vPiece == vP6) Board.HashPawn ^= qHash;
      return lower(vPiece, nTo);
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Boolean lower(Byte vPiece, Int32 nTo) {
      const String methodName = nameof(lower);
      var qp = bit(nTo);
#if VerifySquarePiece
      foreach (var testSide in Board.Side) {
        if ((qp & testSide.Piece) != 0) {
          var sb = new StringBuilder();
          sb.Append($"{testSide.Parameter.SideName} Piece prevents placement of {Parameter.SideName} Piece at")
            .AppendSquares(qp);
          throw new MoveException(sb.ToString());
        }
      }
#endif
      setPiece(qp);
#if !Magic
      Board.SetRotations(nTo);
#endif
      var bLite = false;
      switch (vPiece) {
      case vP6:
        Board.Pawn |= qp;
        break;
      case vR6:
        Board.OrthPiece |= qp;
        break;
      case vN6:
        Board.Knight |= qp;
        break;
      case vB6:
        Board.DiagPiece |= qp;
        bLite = (qp & SquareLite) != 0;
        break;
      case vQ6:
        Board.DiagPiece |= qp;
        Board.OrthPiece |= qp;
        break;
      case vK6:
        Board.King |= qp;
        KingPos = (Byte)nTo;
        break;
      default:
        throw new PieceException($"Unexpected Piece = {vPiece} [{methodName}]");
      }

      return bLite;
    }

    public void PlacePiece(Byte vPiece, Int32 nTo) {
      var bLite = LowerPiece(vPiece, nTo);
      incSideCount(vPiece);

      //
      // Update Pair
      //
      if (vPiece == vB6) {
        if (bLite)
          setLite();
        else
          setDark();
#if HashPieces
        var u = (UInt32)(FlagsSide & SideFlags.Pair) >> nBishopPairBit;
        setTwoBits(ref PieceHash, 0, u);// Piece == vHF
#endif
      }
#if HashPieces
      if (vP6 < vPiece && vPiece < vK6) {
        var u = PieceCount(vPiece);
        setTwoBits(ref PieceHash, vPiece - vHF, u % vMod4);
      }
#endif
      setInsufficient();
    }

    //
    //[Chess960]Certain piece setups require both King and Rook to be raised before
    // either can be lowered into their new position.  While Orthodox Castling does
    // not require this, one or both of the To squares may be occupied in Chess 960.
    //
    public void RookCastles(Int32 nTo) {
      var rule = Parameter.Rule;

      if (rule.KingOOTo == nTo &&
          rule.RookOOFrom.HasValue) {
        RaisePiece(vR6, rule.RookOOFrom.Value);
        LowerPiece(vR6, rule.RookOOTo);
        return;
      }

      if (rule.KingOOOTo == nTo &&
          rule.RookOOOFrom.HasValue) {
        RaisePiece(vR6, rule.RookOOOFrom.Value);
        LowerPiece(vR6, rule.RookOOOTo);
        return;
      }

      //[Safe]
      throw new ChessException($"{Parameter.SideName} cannot castle");
    }
    #endregion                          // Mover Methods

    #region Attacker Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public Byte GetKingPos() {
      if (KingPos.HasValue) return KingPos.Value;
      throw new ArgumentNullException(nameof(KingPos));
    }

    //
    // Checkers() is used to distinguish between single vs double check
    // to determine whether Interposition or Capture may be possible or
    // whether only Evasion should be considered.
    //
    // qpFrom returns squares with pieces that attack the opposing King.
    //
    // Pawn attacks are handled conventionally; but the non-directional,
    // symmetric nature of piece attacks allows attacks to be generated
    // at the To square.  An interesection can than be made with actual
    // pieces and the attacks generated for their type.
    //
    public Plane Checkers(Byte vKingPos, Plane qpTo) {
      var qpFrom = 0UL;
      qpFrom |= Piece & Board.King & AtxKing[vKingPos];
      qpFrom |= Piece & Board.Knight & AtxKnight[vKingPos];
      qpFrom |= Piece & Board.DiagPiece & Board.RayDiag(vKingPos);
      qpFrom |= Piece & Board.OrthPiece & Board.RayOrth(vKingPos);

      if ((qpTo & PawnA1H8Atx) != 0) qpFrom |= bit(vKingPos - Parameter.PawnA1H8);
      if ((qpTo & PawnA8H1Atx) != 0) qpFrom |= bit(vKingPos - Parameter.PawnA8H1);

      return qpFrom;
    }

    //
    // The following is used to preempt Illegal King Moves.
    // Allowing the moves, then handling them like Illegal
    // Moves may be just as fast.
    //
    public Plane Safe(Plane qpFriend) {
      var qpAttacked = 0UL;

      qpAttacked |= (qpFriend & PawnA1H8Atx);
      qpAttacked |= (qpFriend & PawnA8H1Atx);

      while (qpFriend != 0) {
        var n = RemoveLo(ref qpFriend, out Plane qp);
        if ((Piece & Board.Knight & AtxKnight[n]) != 0 ||
            (Piece & Board.DiagPiece & Board.RayDiag(n)) != 0 ||
            (Piece & Board.OrthPiece & Board.RayOrth(n)) != 0 ||
            (Piece & Board.King & AtxKing[n]) != 0)
          qpAttacked |= qp;
      }

      return ~qpAttacked;
    }

    //
    // IsAttacked() is used by the Legal Move and Check tests
    // and to disallow castling through check:
    //
    public Boolean IsAttacked(Plane qpFriend) {
      if ((qpFriend & PawnA1H8Atx) != 0 ||
          (qpFriend & PawnA8H1Atx) != 0)
        return true;

      while (qpFriend != 0) {
        var n = RemoveLo(ref qpFriend);
        if ((Piece & Board.Knight & AtxKnight[n]) != 0 ||
            (Piece & Board.DiagPiece & Board.RayDiag(n)) != 0 ||
            (Piece & Board.OrthPiece & Board.RayOrth(n)) != 0 ||
            (Piece & Board.King & AtxKing[n]) != 0)
          return true;
      }

      return false;
    }

    //
    // The following is used by abbreviate() to avoid the overhead of BuildAtxTo():
    //
    public Plane PawnAtxTo(Int32 nTo) {
      var qpFrom = 0UL;
      var qpTo = bit(nTo);

      if ((qpTo & PawnA1H8Atx) != 0) qpFrom |= bit(nTo - Parameter.PawnA1H8);
      if ((qpTo & PawnA8H1Atx) != 0) qpFrom |= bit(nTo - Parameter.PawnA8H1);

      return qpFrom;
    }

    //
    // The PawnTo() method is used by parsePACNMove() to validate
    // moves entered in Pure Algebraic Coordinate Notation (PACN):
    //
    public Plane PawnTo(Int32 nFrom, Boolean bCapture) {
      Plane qpPawnTo;
      var qpFrom = bit(nFrom);

      if (bCapture) {
        var qpA1H8Atx = ShiftL(qpFrom & ~Parameter.FileRight, Parameter.PawnA1H8);
        var qpA8H1Atx = ShiftL(qpFrom & ~Parameter.FileLeft, Parameter.PawnA8H1);
        qpPawnTo = qpA1H8Atx | qpA8H1Atx;
      }
      else {
        var qpAdvance1 = ShiftL(qpFrom, Parameter.PawnStep) & ~Board.RankPiece;
        var qpAdvance2 = ShiftL(qpAdvance1 & Parameter.EnPassantMask, Parameter.PawnStep) & ~Board.RankPiece;
        qpPawnTo = qpAdvance1 | qpAdvance2;
      }

      return qpPawnTo;
    }
    #endregion                          // Attacker Methods

    #region Count Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void incSideCount(Byte vPiece) {
      Counts += 1U << vPiece * nPerNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    protected void decSideCount(Byte vPiece) {
      Counts -= 1U << vPiece * nPerNibble;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    public UInt32 PieceCount(Byte vPiece) {
      return Nibble(Counts >> vPiece * nPerNibble);
    }

    public Int32 AtxCount() {
      var nAtx = 0;
      if (!KingPos.HasValue)
        throw new ArgumentException("Invalid King Position", nameof(KingPos));
      Board.incTo(AtxKing[KingPos.Value]);

      Board.incTo(PawnA1H8Atx);
      Board.incTo(PawnA8H1Atx);

      var qpAtxFrom = Piece & Board.Knight;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += Board.incTo(AtxKnight[n]);
      }

      qpAtxFrom = Piece & Board.Bishop;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += Board.incTo(Board.RayDiag(n));
      }

      qpAtxFrom = Piece & Board.Rook;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += Board.incTo(Board.RayOrth(n));
      }

      qpAtxFrom = Piece & Board.Queen;
      while (qpAtxFrom != 0) {
        var n = RemoveLo(ref qpAtxFrom);
        nAtx += Board.incTo(Board.Ray(n));
      }

      return nAtx;
    }
    #endregion                          // Count Methods

    #region Grant Castling
    private Byte validateCastlesFrom() {
      var sideName = Parameter.SideName;
      if (!KingPos.HasValue)
        throw new ParsePositionException($"{sideName} must have a King");

      var nLower = x((Int32)Sq.a1);
      var nUpper = x((Int32)Sq.h1);

      var bCanCastleFrom =
        nLower < x(KingPos.Value) &&
        x(KingPos.Value) < nUpper &&
        y(KingPos.Value) == Parameter.PieceRank;

      if (!bCanCastleFrom) {
        var sq = (Sq)KingPos;
        throw new ParsePositionException($"{sideName} King cannot castle from {sq}");
      }

      return KingPos.Value;
    }

    public void GrantCastling(Byte vRookFrom) {
      var rule = Parameter.Rule;
      rule.CastlesFrom = validateCastlesFrom();

      var sideName = Parameter.SideName;
      var qpRook = Board.Rook & Piece;
      if (vRookFrom < KingPos) {
        if (rule.RookOOOFrom.HasValue)
          throw new ParsePositionException($"Multiple {sideName} Rooks for OOO");

        if ((qpRook & bit(vRookFrom)) == 0)
          throw new ParsePositionException($"No {sideName} Rook for OOO");

        rule.RookOOOFrom = vRookFrom;

        SetCanOOO();
      }
      else if (KingPos < vRookFrom) {
        if (rule.RookOOFrom.HasValue)
          throw new ParsePositionException($"Multiple {sideName} Rooks for OO");

        if ((qpRook & bit(vRookFrom)) == 0)
          throw new ParsePositionException($"No {sideName} Rook for OO");

        rule.RookOOFrom = vRookFrom;

        SetCanOO();
      }
    }
    #endregion                        // Grant Castling

    #region Hashcode Methods
    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Hashcode pieceHash(Byte vPiece, Int32 n) {
      if (nPieces <= vPiece) {
        Debug.Assert(vPiece < nPieces, "pieceHash(nPieces <= vPiece)");
      }

      if (n < 0) {
        Debug.Assert(n >= 0, "pieceHash(n < 0)");
      }
      else if (nSquares <= n) {
        Debug.Assert(n < nSquares, "pieceHash(nSquares <= n)");
      }

      var zobrist = Parameter.Zobrist;
      return zobrist[vPiece][n];
    }

    public Hashcode HashPiece(Plane qp, Byte vPiece) {
      var zobrist = Parameter.Zobrist;
      Hashcode qHash = 0;
      var qpPiece = qp & Piece;
      while (qpPiece != 0) {
        var n = RemoveLo(ref qpPiece);
        qHash ^= zobrist[vPiece][n];
      }
      return qHash;
    }

    public Hashcode CastlingRightsHash(SideFlags fsideCanCastle) {
      var n = (Int32)fsideCanCastle;
      return Parameter.ZobristRights[n];
    }

    [Conditional("HashCastlingRights")]
    public void HashCastlingRights() {
      Board.Hash ^= CastlingRightsHash(FlagsSide & SideFlags.CanCastle);
    }

    [Conditional("HashCastlingRights")]
    private void hashCastlingRights(SideFlags fsideOld, SideFlags fsideNew) {
      var fsideCanCastleOld = fsideOld & SideFlags.CanCastle;
      var fsideCanCastleNew = fsideNew & SideFlags.CanCastle;

      if (fsideCanCastleNew != fsideCanCastleOld) {
        //
        // A new Repetition Cycle begins whenever the Castling Rights change:
        //
        Board.SetDraw0();

        Board.Hash ^= CastlingRightsHash(fsideCanCastleOld) ^
                      CastlingRightsHash(fsideCanCastleNew);
      }
    }
    #endregion                          // Hashcode Methods
    #endregion                          // Methods
  }                                     // BoardSide
}                                       // Board
