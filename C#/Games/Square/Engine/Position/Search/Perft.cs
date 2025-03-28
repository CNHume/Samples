﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[2011-06-12 CNHume]Created File
//
// Conditionals:
//
#define CountCheckmates
//#define DebugMove
//#define VerifyMaterialMoves

using System.Diagnostics;
using System.Text;

namespace Engine;

//
// Type Aliases:
//
using PlyDepth = Byte;

partial class Position : Board {
  #region Search Methods
  [Conditional("VerifyMaterialMoves")]
  private void verifyMaterialMoves(List<Move> moves) {
    var filteredMoves = moves.Where(m => m.Has(Move.Material));
    var filtered = filteredMoves.ToArray();
#if OrderMoves
    Array.Sort<Move>(filtered, 0, filtered.Length);
#endif
    List<Move> materialMoves = [];
    generateMaterialMoves(materialMoves);

    var material = materialMoves.ToArray();
#if OrderMoves
    Array.Sort<Move>(material, 0, material.Length);
#endif
    if (filtered.Length == material.Length) {
      for (var n = 0; n < filtered.Length; n++)
        if (filtered[n] != material[n]) {
          Trace.Assert(filtered[n] == material[n], $"filtered[{n}] != material[{n}]");
        }
    }
    else {
      Trace.Assert(filtered.Length == material.Length, "filtered.Length != material.Length");
      DisplayCurrent("Filtered vs Material Moves:");
      var sb = new StringBuilder()
        .MapMoves(Extension.AppendAN, filteredMoves, Side, State.IsChess960)
        .AppendLine()
        .FlushLine()

        .MapMoves(Extension.AppendAN, materialMoves, Side, State.IsChess960)
        .AppendLine()
        .FlushLine();
    }
  }

  //
  // For some published Perft Results, see http://www.chessprogramming.org/Perft_Results
  //
  private void perft(PlyDepth vPlies) {
    var moves = PseudoMoves;
    var vPlies1 = (PlyDepth)(vPlies - 1);
    var pc = State.Case;

    generate(moves, !Swaps);
    verifyMaterialMoves(moves);         //[Conditional]

    var child = Push();                 // Push Position to make the moves
                                        //[Debug]var sFEN = child.Parent?.ToString(PositionType.FEN);
    try {
      var uLegalMoves = 0U;
      foreach (var mov in moves) {
        var move = mov;
#if DebugMove
        unpackMove1(move, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
        //unpackMove2(move, out Sq sqFrom, out Sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
        if (!child.tryMove(ref move, NotFindRepetition))
          continue;

        uLegalMoves++;                  // Count Legal Moves for Final annotation
        pc.TotalNodes++;                // TotalNodes is reset for each Test Case

        //
        //[Note]Annotation is made from the child position resulting from a move;
        // and after IsLegal() has been called:
        //
        //var moveNoted = child.annotate(move);
        //
        if (vPlies > 0) {               // Non-Leaf Node: Recurse
          child.perft(vPlies1);
#if AnnotateFinal
          if (child.IsFinal()) move |= Move.NoteFinal;
#endif
        }
        else                            // Leaf Node: Increment appropriate count and return
          child.countLeaf(move);
      }                                 //[Next]Pseudo Move

      if (uLegalMoves == 0)             // No Move Found
        SetFinal();                     // Mark Game Leaf for annotation
    }
    finally {
      Pop(ref child);                   // Pop Position used for this Ply
    }
  }

  private void countLeaf(Move move) {
    var pc = State.Case;
    pc.LeafNodes++;

    var bCapture = IsCapture(move);
    if (bCapture) {
      pc.Captures++;
      if ((Piece)Captured(move) == Piece.EP)
        pc.EnPassant++;
    }
    else if (IsCastles(move))
      pc.Castles++;

    if ((move & Move.PromoteMask) != 0)
      pc.Promotions++;

    if (InCheck()) {
      pc.Checks++;
#if CountCheckmates                     //[Speed]
      if (isLeaf()) pc.Checkmates++;
#endif
    }
  }
  #endregion                            // Search Methods
}
