//
// Copyright (C) 2010-2022, Christopher N. Hume.  All rights reserved.
//
//[2012-02-12 CNHume]Created Class
//
namespace Engine.Test {
  using static Logging.Logger;
  using System;
  using System.Collections.Generic;

  //
  // Type Aliases:
  //
  using PlyDepth = Byte;

  class PerfCase {
    #region Count Properties
    public PlyDepth Plies { get; set; }
    public UInt64? TotalNodes { get; set; }
    public UInt64? LeafNodes { get; set; }
    public UInt64? Captures { get; set; }
    public UInt64? EnPassant { get; set; }
    public UInt64? Castles { get; set; }
    public UInt64? Promotions { get; set; }
    public UInt64? Checks { get; set; }
    public UInt64? Checkmates { get; set; }
    #endregion

    #region Methods
    public void Clear() {
      TotalNodes = 0UL;
      LeafNodes = 0UL;
      Captures = 0UL;
      EnPassant = 0UL;
      Castles = 0UL;
      Promotions = 0UL;
      Checks = 0UL;
      Checkmates = 0UL;
    }

    public Boolean Passed(PerfCase pc) {
      var bPassed = true;

      if (pc.TotalNodes.HasValue &&
          pc.TotalNodes != TotalNodes) {
        LogLine($"TotalNodes = {pc.TotalNodes}");
        if (TotalNodes.HasValue)
          bPassed = false;
      }

      if (pc.LeafNodes.HasValue &&
          pc.LeafNodes != LeafNodes) {
        LogLine($"LeafNodes = {pc.LeafNodes}");
        if (LeafNodes.HasValue)
          bPassed = false;
      }

      if (pc.Captures.HasValue &&
          pc.Captures != Captures) {
        LogLine($"Captures = {pc.Captures}");
        if (Captures.HasValue)
          bPassed = false;
      }

      if (pc.EnPassant.HasValue &&
          pc.EnPassant != EnPassant) {
        LogLine($"EnPassant = {pc.EnPassant}");
        if (EnPassant.HasValue)
          bPassed = false;
      }

      if (pc.Castles.HasValue &&
          pc.Castles != Castles) {
        LogLine($"Castles = {pc.Castles}");
        if (Castles.HasValue)
          bPassed = false;
      }

      if (pc.Promotions.HasValue &&
          pc.Promotions != Promotions) {
        LogLine($"Promotions = {pc.Promotions}");
        if (Promotions.HasValue)
          bPassed = false;
      }

      if (pc.Checks.HasValue &&
          pc.Checks != Checks) {
        LogLine($"Checks = {pc.Checks}");
        if (Checks.HasValue)
          bPassed = false;
      }

      if (pc.Checkmates.HasValue &&
          pc.Checkmates != Checkmates) {
        LogLine($"Checkmates = {pc.Checkmates}");
        if (Checkmates.HasValue)
          bPassed = false;
      }

      return bPassed;
    }
    #endregion
  }
}
