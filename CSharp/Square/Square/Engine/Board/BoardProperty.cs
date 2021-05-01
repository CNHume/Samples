//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// Conditionals:
//
//#define Magic
//#define TestMagic
#define InitDeBruijn
#define HalfDeBruijn                    // Half de Bruijn: Avoiding 64-Bit Multiplication of slight benefit
//#define CryptoServiceProvider
//#define TestZobrist

namespace Engine {
  using System;
  using System.Collections.Generic;
#if CryptoServiceProvider
  using System.Security.Cryptography;
#endif
  using static Position;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;
  using Hashcode = System.UInt64;
  using PieceCounter = System.UInt32;
  using PieceHashcode = System.UInt16;  // 10 bits
  using Plane = System.UInt64;
  using Ply = System.UInt16;

  partial class Board {
    protected static String PieceSymbols;

    #region Constant Fields
    public static Plane LiteSquare;
    public static Plane DarkSquare;

    public static Plane[] KingAtx;
    public static Plane[] KnightAtx;

    public static Plane[][] RankAtx;
    public static Plane[][] FileAtx;
    public static Plane[][] A1H8Atx;
    public static Plane[][] A8H1Atx;

    protected static Byte[] RankOffset;
#if Magic
    protected static Int32[] A1H8Lo;
    protected static Int32[] A8H1Lo;
    protected static Byte[] A1H8Magic;
    protected static Byte[] A8H1Magic;
    protected static Byte[] FileMagic;
#if TestMagic
    public static Byte[] FileState;
    public static Byte[] A1H8State;
    public static Byte[] A8H1State;
#endif
#else
    protected static Byte[] FileOffset;
    protected static Byte[] DiagOffset; // Helps build A1H8Offset and A8H1Offset
    protected static Byte[] A1H8Offset;
    protected static Byte[] A8H1Offset;

    protected static Plane[] RankBit;
    protected static Plane[] FileBit;
    protected static Plane[] A1H8Bit;
    protected static Plane[] A8H1Bit;
#endif
#if InitDeBruijn
    protected static Byte[] deBruijnByte;
#if HalfDeBruijn
    protected static Byte[] deBruijnHalf;
#else
    protected static Byte[] deBruijnFull;
#endif
#endif
#if CryptoServiceProvider
    protected static RNGCryptoServiceProvider ZobristRNG;
#else
    protected static Random ZobristRandom;
#endif
    protected static Byte[] ZobristBuffer;
    protected static Hashcode ZobristTurn;
    protected static Hashcode[] ZobristDraw;
    protected static Hashcode[] ZobristFile;
    protected static Hashcode[] ZobristRights;
    protected static Hashcode[][] ZobristWhite;
    protected static Hashcode[][] ZobristBlack;
    protected static Hashcode[] ZobristExcludedFrom;
    protected static Hashcode[] ZobristExcludedTo;
    protected static Hashcode[] ZobristExcludedPromotion;
    protected static Hashcode ZobristExcludedCastles;
#if TestZobrist
    protected static List<Hashcode> Zobrists;
#endif
    #endregion

    #region Virtual Fields
    protected readonly PositionSide[] Side;

    public Ply NullPly;                 //[Test]May be used to limit recursive Null Move Pruning
    public Ply GamePly;
    public Byte HalfMoveClock;
    public LoFlags FlagsLo;             //[flo]Final | InCheck | Illegal | Passed | WTM | EPFile
    public EGFlags FlagsEG;             //[feg]OutsideSquare | KBN | KingAlone
    public DrawFlags FlagsDraw;         //[fdr]DrawMask | Fence
    public ModeFlags FlagsMode;         //[fmd]Trace | NullMade | Reduced
    public Hashcode Hash;               //[Note]Hash also appears in Transposition Table entries
    public Hashcode HashPawn;

    public Plane Pawn;                  // Piece Types
    public Plane King;
    public Plane Knight;
    public Plane DiagPiece;
    public Plane RectPiece;
    public Plane RankPiece;             // Piece Union
#if !Magic
    public Plane FilePiece;             // Rotations
    public Plane A1H8Piece;
    public Plane A8H1Piece;
#endif
    #endregion

    #region Properties
    //
    // State may well be made static.  This minimal overhead leaves
    // open the possibility of analyzing multiple games in parallel.
    //
    internal GameState State { get; set; }

    public Ply SearchPly {
      //
      // GamePly, and therefore also SearchPly is advanced by toggleWTM().
      // GamePly should be even iff WTM.
      //
      get { return (Ply)(GamePly - State.MovePly); }
    }

    public Plane Bishop { get { return DiagPiece & ~RectPiece; } }
    public Plane Queen { get { return DiagPiece & RectPiece; } }
    public Plane Rook { get { return RectPiece & ~DiagPiece; } }

    public String Name { get; set; }
    public Dictionary<String, List<String>> Operations { get; set; }
    #endregion
  }
}
