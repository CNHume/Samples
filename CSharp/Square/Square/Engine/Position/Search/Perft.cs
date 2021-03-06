﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2011-06-12 CNHume]Created File
//
// Conditionals:
//
#define CountCheckmates
//#define DebugMove
//#define VerifyMaterialMoves

namespace Engine {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Linq;                    // For verifyMaterialMoves()
  using System.Text;

  //
  // Type Aliases:
  //
  using PlyDepth = System.Byte;

  partial class Position : Board {
    #region Search Methods
    protected Boolean isFinal() {
      var moves = PseudoMoves;
      generate(moves, NoSwaps);
      var child = Push();               // Push Position to find a legal move
      try {
        var uLegalMoves = 0U;
        foreach (var mov in moves) {
          var move = mov;
          if (child.tryMove(ref move, NotFindRepetition)) {
            uLegalMoves++;
            break;
          }
        }

        return uLegalMoves == 0;
      }
      finally {
        Pop(ref child);                 // Pop Position used for this test
      }
    }

    [Conditional("VerifyMaterialMoves")]
    protected void verifyMaterialMoves(List<Move> moves) {
      var filteredMoves = moves.Where(m => (m & Move.Material) != 0);
      var filtered = filteredMoves.ToArray();
#if OrderMoves
      Array.Sort<Move>(filtered, 0, filtered.Length);
#endif
      var materialMoves = new List<Move>();
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
        var sb = new StringBuilder();
        Trace.Assert(filtered.Length == material.Length, "filtered.Length != material.Length");
        DisplayCurrent("Filtered vs. Material Moves:");

        sb.MapMoves(Extension.AppendAN, filteredMoves, State.Rule);
        sb.AppendLine();
        sb.FlushLine();

        sb.MapMoves(Extension.AppendAN, materialMoves, State.Rule);
        sb.AppendLine();
        sb.FlushLine();
      }
    }

    //
    // For some published Perft Results, see http://www.chessprogramming.org/Perft_Results
    //
    protected void perft(PlyDepth vPlies) {
      var moves = PseudoMoves;
      var vPlies1 = (PlyDepth)(vPlies - 1);
      var pc = State.Case;

      generate(moves, NoSwaps);
      verifyMaterialMoves(moves);       // Conditional

      var child = Push();               // Push Position to make the moves
      var sFEN = child.Parent.ToString(PositionType.FEN);
      try {
        var uLegalMoves = 0U;
        foreach (var mov in moves) {
          var move = mov;
#if DebugMove
          unpackMove1(move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Boolean bCapture);
          //unpackMove2(move, out sq sqFrom, out sq sqTo, out Piece piece, out Piece promotion, out Piece capture, out Boolean bCastles, out Boolean bCapture);
#endif
          if (!child.tryMove(ref move, NotFindRepetition))
            continue;

          uLegalMoves++;                // Count Legal Moves for Final annotation
          pc.TotalNodes++;              // TotalNodes is reset for each Test Case

          //
          //[Note]Annotation is made from the child position resulting from a move;
          // and after IsLegal() has been called:
          //
          //var moveNoted = child.annotate(move);
          //
          if (vPlies > 0) {             // Non-Leaf Node: Recurse
            child.perft(vPlies1);
#if AnnotateFinal
            if (child.IsFinal()) move |= Move.NoteFinal;
#endif
          }
          else                          // Leaf Node: Increment appropriate count and return
            child.countLeaf(move);
        }                               //[Next]Pseudo Move

        if (uLegalMoves == 0)           // No Move Found
          setFinal();                   // Mark Game Leaf for annotation
      }
      finally {
        Pop(ref child);                 // Pop Position used for this Ply
      }
    }

    protected void countLeaf(Move move) {
      var pc = State.Case;
      pc.LeafNodes++;

      var bCapture = isCapture(move);
      if (bCapture) {
        pc.Captures++;
        if ((Piece)captured(move) == Piece.EP)
          pc.EnPassant++;
      }
      else if (isCastles(move))
        pc.Castles++;

      if ((move & Move.PromoteMask) != 0)
        pc.Promotions++;

      if (InCheck()) {
        pc.Checks++;
#if CountCheckmates                     //[Speed]
        if (isFinal()) pc.Checkmates++;
#endif
      }
    }
    #endregion
  }
}
