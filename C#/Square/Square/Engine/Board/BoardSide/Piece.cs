//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-09-15 CNHume]Split Piece into its own file
//
// Conditionals:
//
#define EvalInsufficient
#define HashPieces
//#define TestPawnAdvances
//#define VerifySquarePiece               // Ensure move from an occupied square to an empty square

namespace Engine {
  using Command.Exceptions;
  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using Plane = UInt64;

  partial class Board {
    public partial class BoardSide {
      #region Methods
      //
      // [clr|set]Piece
      // [Raise|Remove]Piece
      // [Lower|Place]Piece
      // Passed
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
      // AddPromotions
      // [inc|dec]SideCount
      // AtxCount
      // pieceHash
      // HashPiece
      //
      #region Mover Methods
      //
      // Return Friend Pawns which may be able to capture En Passant
      // the Foe Pawn that just passed through the nEnPassant square.
      //
      public Plane Passed(Int32 nEnPassant) {
        var qpEnPassant = BIT0 << nEnPassant;

        var qpCaptureFrom =
          shiftr(qpEnPassant & PawnA1H8Atx, Parameter.ShiftA1H8) |
          shiftr(qpEnPassant & PawnA8H1Atx, Parameter.ShiftA8H1);

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

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public void ResetPawnAtx() {
        var qpPawn = Piece & Board.Pawn;
        PawnA1H8Atx = shiftl(qpPawn & ~Parameter.FileRight, Parameter.ShiftA1H8);
        PawnA8H1Atx = shiftl(qpPawn & ~Parameter.FileLeft, Parameter.ShiftA8H1);
      }

      public Boolean RaisePiece(Byte vPiece, Int32 nFrom) {
        var qHash = pieceHash(vPiece, nFrom);
        Board.Hash ^= qHash;
        if (vPiece == vP6) Board.HashPawn ^= qHash;

        var qp = BIT0 << nFrom;
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
          Board.RectPiece &= ~qp;
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
          bLite = (qp & LiteSquare) != 0;
          break;
        case vQ6:
          Board.DiagPiece &= ~qp;
          Board.RectPiece &= ~qp;
          break;
        case vK6:
          Board.King &= ~qp;
          ClrCanCastle();
          break;
        default:
          throw new PieceException("Unexpected Piece [raiseSide]");
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
            if ((qpBishop & LiteSquare) == 0)
              ClrLite();
          }
          else if ((qpBishop & DarkSquare) == 0)
            ClrDark();
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
#if EvalInsufficient
        setInsufficient();
#endif
      }

      public Boolean LowerPiece(Byte vPiece, Int32 nTo) {
        var qHash = pieceHash(vPiece, nTo);
        Board.Hash ^= qHash;
        if (vPiece == vP6) Board.HashPawn ^= qHash;
        var qp = BIT0 << nTo;
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
          Board.RectPiece |= qp;
          break;
        case vN6:
          Board.Knight |= qp;
          break;
        case vB6:
          Board.DiagPiece |= qp;
          bLite = (qp & LiteSquare) != 0;
          break;
        case vQ6:
          Board.DiagPiece |= qp;
          Board.RectPiece |= qp;
          break;
        case vK6:
          Board.King |= qp;
          KingPos = (Byte)nTo;
          break;
        default:
          throw new PieceException("Unexpected Piece [lowerSide]");
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
            SetLite();
          else
            SetDark();
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
#if EvalInsufficient
        setInsufficient();
#endif
      }

      //
      //[Chess 960]Certain castling configurations require that both the Board.King and the Rook
      // be removed before either is added back in their new positions.  Orthodox Castling
      // does not require this; but one or even both of the From squares may coincide with
      // the castling partner To square in Chess 960.
      //
      public void RookCastles(Int32 nTo) {
        var rule = Parameter.Rule;

        if (nTo == rule.KingOOTo && rule.RookOOFrom.HasValue) {
          RaisePiece(vR6, rule.RookOOFrom.Value);
          LowerPiece(vR6, rule.RookOOTo);
        }
        else if (nTo == rule.KingOOOTo && rule.RookOOOFrom.HasValue) {
          RaisePiece(vR6, rule.RookOOOFrom.Value);
          LowerPiece(vR6, rule.RookOOOTo);
        }
      }
      #endregion                        // Mover Methods

      #region Attacker Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Boolean IsAlone() {
        return IsOneOrNone(Piece);
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      public Byte GetKingPos() {
        if (KingPos.HasValue) return KingPos.Value;
        throw new ArgumentNullException(nameof(KingPos));
      }

      //
      // Checkers() is used to distinguish between single vs double checks,
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
      public Plane Checkers(Byte vKingPos, Plane qpTo) {
        var qpFrom = 0UL;
        qpFrom |= Piece & Board.King & KingAtx[vKingPos];
        qpFrom |= Piece & Board.Knight & KnightAtx[vKingPos];
        qpFrom |= Piece & Board.DiagPiece & Board.diagAtx(vKingPos);
        qpFrom |= Piece & Board.RectPiece & Board.rectAtx(vKingPos);

        if ((qpTo & PawnA1H8Atx) != 0) qpFrom |= BIT0 << vKingPos - Parameter.ShiftA1H8;
        if ((qpTo & PawnA8H1Atx) != 0) qpFrom |= BIT0 << vKingPos - Parameter.ShiftA8H1;

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
          if ((Piece & Board.Knight & KnightAtx[n]) != 0 ||
              (Piece & Board.DiagPiece & Board.diagAtx(n)) != 0 ||
              (Piece & Board.RectPiece & Board.rectAtx(n)) != 0 ||
              (Piece & Board.King & KingAtx[n]) != 0)
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
          if ((Piece & Board.Knight & KnightAtx[n]) != 0 ||
              (Piece & Board.DiagPiece & Board.diagAtx(n)) != 0 ||
              (Piece & Board.RectPiece & Board.rectAtx(n)) != 0 ||
              (Piece & Board.King & KingAtx[n]) != 0)
            return true;
        }

        return false;
      }

      //
      // The following is used by abbreviate() to avoid the overhead of BuildAtxTo():
      //
      public Plane PawnAtxTo(Int32 nTo) {
        var qpFrom = 0UL;
        var qpTo = BIT0 << nTo;

        if ((qpTo & PawnA1H8Atx) != 0) qpFrom |= BIT0 << nTo - Parameter.ShiftA1H8;
        if ((qpTo & PawnA8H1Atx) != 0) qpFrom |= BIT0 << nTo - Parameter.ShiftA8H1;

        return qpFrom;
      }

      //
      // The PawnTo() method is used by parsePACNMove() to validate
      // moves entered in Pure Algebraic Coordinate Notation (PACN):
      //
      public Plane PawnTo(Int32 nFrom, Boolean bCapture) {
        Plane qpPawnTo;
        var qpFrom = BIT0 << nFrom;

        if (bCapture) {
          var qpA1H8Atx = shiftl(qpFrom & ~Parameter.FileRight, Parameter.ShiftA1H8);
          var qpA8H1Atx = shiftl(qpFrom & ~Parameter.FileLeft, Parameter.ShiftA8H1);
          qpPawnTo = qpA1H8Atx | qpA8H1Atx;
        }
        else {
          var qpAdvance1 = shiftl(qpFrom, Parameter.ShiftRank) & ~Board.RankPiece;
          var qpAdvance2 = shiftl(qpAdvance1 & Parameter.RankPass, Parameter.ShiftRank) & ~Board.RankPiece;
          qpPawnTo = qpAdvance1 | qpAdvance2;
        }

        return qpPawnTo;
      }
      #endregion                        // Attacker Methods

      #region Move Builder
      public Move BuildMove(
        String sPACN, sq? sqFrom, sq? sqTo, Piece promotion, Int32 nFrom, Int32 nTo,
        Plane qpTo, Byte vPiece, Byte vCapture, Boolean bCapture) {

        //
        // Validate Non-Castling Move
        //
        var qpPieceAtx = Board.PieceAtx(vPiece, nFrom, bCapture);
        var piece = indexPiece(vPiece);

        if (!qpPieceAtx.HasValue)         //[Safe]
          throw new ParseException($"Unexpected Piece in Move: {sPACN}");
        else {
          qpPieceAtx &= ~Piece;
          if ((qpPieceAtx & qpTo) == 0)
            throw new MoveException($"{piece} cannot move from {sqFrom} to {sqTo}");
        }

        //
        // Validate Promotion
        //
        var bLastRank = (Parameter.RankLast & qpTo) != 0;
        var bRequired = vPiece == vP6 && bLastRank;
        var bSupplied = promotion != default;
        if (bRequired != bSupplied) {
          var sDiagnosis = bRequired ? "Required" : "Illegal";
          throw new MoveException($"Promotion Piece {sDiagnosis} for {sPACN}");
        }

        var move = promotionMove(promotion) | pieceMove(piece) | fromToMove(nFrom, nTo);
        if (bCapture) move |= captureMove(indexPiece(vCapture));
        return move;
      }
      #endregion

      #region Position Pawn Move Generators
      public void AddPawnCaptures(Position position, Plane qpTo) {
        var nEP = Board.IsPassed() ? ep(Board.FlagsTurn) : nSquares;
        AddPawnCaptures2(position, PawnA1H8Atx & qpTo, Parameter.ShiftA1H8, nEP);
        AddPawnCaptures2(position, PawnA8H1Atx & qpTo, Parameter.ShiftA8H1, nEP);
      }

      protected void AddPawnCaptures2(
        Position position, Plane qpAtx, Int32 nDiag, Int32 nEP) {
        var qpFrom = shiftr(qpAtx, nDiag);
        while (qpFrom != 0) {
          var nFrom = RemoveLo(ref qpFrom);
          var nTo = nFrom + nDiag;
          var bAbove = Parameter.IsAbove(nTo);
          var qpMoveTo = BIT0 << nTo;
          var bPromote = (Parameter.RankLast & qpMoveTo) != 0;
          var bEnPassant = nTo == nEP;
          position.AddPawnCapture(nFrom, nTo, bAbove, bPromote, bEnPassant);
        }
      }

      public void AddPawnMoves(Position position, Plane qpTo) {
        var qpPawn = Piece & Board.Pawn;

        //
        // Pawn Advances:
        //
        var qpAdvance1 = shiftl(qpPawn, Parameter.ShiftRank) & ~Board.RankPiece;
        var qpAdvance2 = shiftl(qpAdvance1 & Parameter.RankPass, Parameter.ShiftRank) & ~Board.RankPiece;
        var qpAdv1From = shiftr(qpAdvance1 & qpTo, Parameter.ShiftRank);
        var qpAdv2From = shiftr(qpAdvance2 & qpTo, 2 * Parameter.ShiftRank);
#if TestPawnAdvances
        LogLine("Pawn Advance:\n");
        writeRect(qpAdvance1 | qpAdvance2);
        LogLine();
#endif
        while (qpAdv1From != 0) {
          var nFrom = RemoveLo(ref qpAdv1From);
          var nTo = nFrom + Parameter.ShiftRank;
          var bAbove = Parameter.IsAbove(nTo);
          var qpMoveTo = BIT0 << nTo;
          var bPromote = (Parameter.RankLast & qpMoveTo) != 0;
          position.AddPawnMove(nFrom, nTo, bAbove, bPromote);
        }

        while (qpAdv2From != 0) {
          var nFrom = RemoveLo(ref qpAdv2From);
          var nTo = nFrom + 2 * Parameter.ShiftRank;
          position.AddPawnMove(nFrom, nTo, false, false);
        }
      }

      // The following method is used by generateMaterialMoves()
      public void AddPromotions(Position position, Plane qpTo) {
        var qpPawn = Piece & Board.Pawn;
        var qpAdvance1 = shiftl(qpPawn, Parameter.ShiftRank) & ~Board.RankPiece;
        var qpAdv1From = shiftr(qpAdvance1 & qpTo & Parameter.RankLast, Parameter.ShiftRank);

        while (qpAdv1From != 0) {
          var nFrom = RemoveLo(ref qpAdv1From);
          var nTo = nFrom + Parameter.ShiftRank;
          position.AddPawnMove(nFrom, nTo, true, true);
        }
      }
      #endregion                        // Move Generators

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
        return nibble(Counts >> vPiece * nPerNibble);
      }

      public Int32 AtxCount() {
        var nAtx = 0;
        if (!KingPos.HasValue)
          throw new ArgumentException(nameof(KingPos), "Invalid King Position");
        Board.incTo(KingAtx[KingPos.Value]);

        Board.incTo(PawnA1H8Atx);
        Board.incTo(PawnA8H1Atx);

        var qpAtxFrom = Piece & Board.Knight;
        while (qpAtxFrom != 0) {
          var n = RemoveLo(ref qpAtxFrom);
          nAtx += Board.incTo(KnightAtx[n]);
        }

        qpAtxFrom = Piece & Board.Bishop;
        while (qpAtxFrom != 0) {
          var n = RemoveLo(ref qpAtxFrom);
          nAtx += Board.incTo(Board.diagAtx(n));
        }

        qpAtxFrom = Piece & Board.Rook;
        while (qpAtxFrom != 0) {
          var n = RemoveLo(ref qpAtxFrom);
          nAtx += Board.incTo(Board.rectAtx(n));
        }

        qpAtxFrom = Piece & Board.Queen;
        while (qpAtxFrom != 0) {
          var n = RemoveLo(ref qpAtxFrom);
          nAtx += Board.incTo(Board.diagAtx(n) | Board.rectAtx(n));
        }

        return nAtx;
      }
      #endregion                        // Count Methods

      #region Grant Castling
      public void GrantCastling(Int32 nRookFile, Boolean bChess960) {
        var rule = Parameter.Rule;
        var sideName = Parameter.SideName;
        var nRank = Parameter.StartRank;
        var nRookFrom = nRank + nRookFile;
        var qpRook = Board.Rook & Piece;

        if (!rule.CastlesFrom.HasValue) {
          if (!KingPos.HasValue)
            throw new ParsePositionException($"{sideName} must have a King");

          if (bChess960) {
            if (KingPos <= nRank + (Int32)sq.a1 || nRank + (Int32)sq.h1 <= KingPos)
              throw new ParsePositionException($"{sideName} King cannot castle");
          }
          else {
            if (KingPos != nRank + (Int32)sq.e1)
              throw new ParsePositionException($"{sideName} King must castle from {sq.e1}");
          }

          rule.CastlesFrom = KingPos;
        }

        if (nRookFrom < KingPos) {
          if (rule.RookOOOFrom.HasValue)
            throw new ParsePositionException($"Redundant {sideName} OOO Ability");

          if ((qpRook & BIT0 << nRookFrom) == 0)
            throw new ParsePositionException($"No {sideName} Rook for OOO");

          rule.RookOOOFrom = nRookFrom;
          SetCanOOO();
        }
        else {
          if (rule.RookOOFrom.HasValue)
            throw new ParsePositionException($"Redundant {sideName} OO Ability");

          if ((qpRook & BIT0 << nRookFrom) == 0)
            throw new ParsePositionException($"No {sideName} Rook for OO");

          rule.RookOOFrom = nRookFrom;
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
          Board.Hash ^= CastlingRightsHash(fsideCanCastleOld) ^
                        CastlingRightsHash(fsideCanCastleNew);

          //
          // A new Transposition Group begins when Castling Rights change:
          //
          Board.SetDraw0();
        }
      }
      #endregion                        // Hashcode Methods
      #endregion                        // Methods
    }
  }
}
