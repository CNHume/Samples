//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2021-01-29 CNHume]Created Class
//
// Conditionals:
//
#define HashPieces
//#define VerifySquarePiece               // Ensure move from an occupied square to an empty square

namespace Engine {
  using Command.Exceptions;
  using Exceptions;

  using System;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;
  using System.Text;

  using static Engine.Position;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;
  using PieceCounter = UInt32;
  using PieceHashcode = UInt16;                // 10 bits
  using Plane = UInt64;

  partial class Board {
    public class BoardSide {
      #region Virtual Fields
      public SideFlags FlagsSide;       //[fside]BishopMask | CanCastleMask

      public PieceCounter Counts;

      public Byte? KingPos;

      public Plane PawnA1H8Atx;         // Attacked by Pawns
      public Plane PawnA8H1Atx;

      public Plane Piece;               // Pieces belonging to side
#if HashPieces
      public PieceHashcode PieceHash;
#endif
      #endregion

      #region Read-Only Properties
      public Board Board { get; }
      public PositionParameter Parameter { get; }
      public CastleRuleParameter Rule { get; }
      #endregion

      #region Constructors
      public BoardSide(
        Board board,
        PositionParameter parameter,
        CastleRuleParameter ruleParameter) {
        Board = board;
        Parameter = parameter;
        Rule = ruleParameter;
      }
      #endregion

      #region Methods
      #region Init Methods
      public void Clear() {
        PawnA8H1Atx = PawnA1H8Atx = Piece = 0UL;
        KingPos = default;

        //
        // Counts is used by IsValid() and eval()
        //
        Counts = 0U;
#if HashPieces
        PieceHash = 0;
#endif
      }
      #endregion

      #region Attacker Methods
      public Boolean IsAlone() {
        return IsOneOrNone(Piece);
      }

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

          var bAttacked =
            (Piece & Board.Knight & KnightAtx[n]) != 0 ||
            (Piece & Board.DiagPiece & Board.diagAtx(n)) != 0 ||
            (Piece & Board.RectPiece & Board.rectAtx(n)) != 0 ||
            (Piece & Board.King & KingAtx[n]) != 0;

          if (bAttacked)
            qpAttacked |= qp;
        }

        return ~qpAttacked;
      }

      //
      // IsAttacked() is used by the Legal Move and Check tests
      // and to disallow castling through check:
      //
      public Boolean IsAttacked(Plane qpFriend) {
        Boolean bAttacked =
          (qpFriend & PawnA1H8Atx) != 0 ||
          (qpFriend & PawnA8H1Atx) != 0;

        while (!bAttacked && qpFriend != 0) {
          var n = RemoveLo(ref qpFriend);

          bAttacked =
            (Piece & Board.Knight & KnightAtx[n]) != 0 ||
            (Piece & Board.DiagPiece & Board.diagAtx(n)) != 0 ||
            (Piece & Board.RectPiece & Board.rectAtx(n)) != 0 ||
            (Piece & Board.King & KingAtx[n]) != 0;
        }

        return bAttacked;
      }

      //
      // The following are used by abbreviate() to avoid the overhead of buildAtxTo():
      //
      public Plane PawnAtxTo(Int32 nTo) {
        var qpFrom = 0UL;
        var qpTo = BIT0 << nTo;

        if ((qpTo & PawnA1H8Atx) != 0) qpFrom |= BIT0 << nTo - Parameter.ShiftA1H8;
        if ((qpTo & PawnA8H1Atx) != 0) qpFrom |= BIT0 << nTo - Parameter.ShiftA8H1;

        return qpFrom;
      }
      #endregion

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
      protected void clrPiece(Plane qp) {
        Board.RankPiece &= ~qp;
        Piece &= ~qp;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void setPiece(Plane qp) {
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
        var qHash = hashPiece(vPiece, nFrom);
        if (vPiece == vP6) Board.HashPawn ^= qHash;
        Board.Hash ^= qHash;

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
          if (nFrom == Rule.RookOOFrom)
            FlagsSide &= ~SideFlags.CanOO;
          else if (nFrom == Rule.RookOOOFrom)
            FlagsSide &= ~SideFlags.CanOOO;
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
          FlagsSide &= ~SideFlags.CanCastleMask;
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
        // Update BishopMask, assuming Board.DiagPiece is up to date
        //
        if (vPiece == vB6) {
          var qpBishop = Piece & Board.Bishop;

          if (bLite) {
            if ((qpBishop & LiteSquare) == 0)
              FlagsSide &= ~SideFlags.Lite;
          }
          else if ((qpBishop & DarkSquare) == 0)
            FlagsSide &= ~SideFlags.Dark;
#if HashPieces
          var u = (UInt32)(FlagsSide & SideFlags.Pair) >> nBishopPairBit;
          setTwoBits(ref PieceHash, 0, u);   // Piece == vHF
#endif
        }
#if HashPieces
        if (vP6 < vPiece && vPiece < vK6) {
          var u = nibble(Counts >> vPiece * nPerNibble);
          setTwoBits(ref PieceHash, vPiece - vHF, u % vMod4);
        }
#endif
      }

      public Boolean LowerPiece(Byte vPiece, Int32 nTo) {
        var qHash = hashPiece(vPiece, nTo);
        if (vPiece == vP6) Board.HashPawn ^= qHash;
        Board.Hash ^= qHash;
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
        // Update BishopMask
        //
        if (vPiece == vB6) {
          FlagsSide |= bLite ? SideFlags.Lite : SideFlags.Dark;
#if HashPieces
          var u = (UInt32)(FlagsSide & SideFlags.Pair) >> nBishopPairBit;
          setTwoBits(ref PieceHash, 0, u);   // Piece == vHF
#endif
        }
#if HashPieces
        if (vP6 < vPiece && vPiece < vK6) {
          var u = nibble(Counts >> vPiece * nPerNibble);
          setTwoBits(ref PieceHash, vPiece - vHF, u % vMod4);
        }
#endif
      }

      //
      //[Chess 960]Certain castling configurations require that both the Board.King and the Rook
      // be removed before either is added back in their new positions.  Orthodox Castling
      // does not require this; but one or even both of the From squares may coincide with
      // the castling partner To square in Chess 960.
      //
      public void RookCastles(Int32 nTo) {
        if (nTo == Rule.KingOOTo && Rule.RookOOFrom.HasValue) {
          RaisePiece(vR6, Rule.RookOOFrom.Value);
          LowerPiece(vR6, Rule.RookOOTo);
        }
        else if (nTo == Rule.KingOOOTo && Rule.RookOOOFrom.HasValue) {
          RaisePiece(vR6, Rule.RookOOOFrom.Value);
          LowerPiece(vR6, Rule.RookOOOTo);
        }
      }
      #endregion

      #region Hashcode Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected Hashcode hashPiece(Byte vPiece, Int32 n) {
        if (nPieces <= vPiece) {
          Debug.Assert(vPiece < nPieces, "hashPiece(nPieces <= vPiece)");
        }

        if (n < 0) {
          Debug.Assert(n >= 0, "hashPiece(n < 0)");
        }
        else if (nSquares <= n) {
          Debug.Assert(n < nSquares, "hashPiece(nSquares <= n)");
        }

        var zobrist = Parameter.Zobrist;
        return zobrist[vPiece][n];
      }

      public Hashcode HashPiece(Plane qpPiece, Byte vPiece) {
        var zobrist = Parameter.Zobrist;
        Hashcode qHash = 0;
        while (qpPiece != 0) {
          var n = RemoveLo(ref qpPiece);
          qHash ^= zobrist[vPiece][n];
        }
        return qHash;
      }
      #endregion

      #region Count Methods
      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void incSideCount(Byte vPiece) {
        Counts += 1U << vPiece * nPerNibble;
      }

      [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
      protected void decSideCount(Byte vPiece) {
        Counts -= 1U << vPiece * nPerNibble;
      }
      #endregion
      #endregion
    }
  }
}
