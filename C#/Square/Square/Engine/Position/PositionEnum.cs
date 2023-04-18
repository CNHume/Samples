//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2022-12-04 CNHume]Created File for Position Enum Types and Constants
//
// Conditionals:
//

namespace Engine;

#region Enumerations
#region EvalType Enum
//
// A Value for the Best Move will be Exact when it is within the Search Window,
// i.e., if it falls on the closed interval which includes both Alpha and Beta.
//
// Upper Bounds result from "Failing Low", with values below Alpha
// Lower Bounds result from "Failing High", with values above Beta
//
// Upper approximates LUB, Supremum, Join
// Lower approximates GLB, Infimum, Meet
//
//[Note]The EvalType values matter here.  Negating two bits results in
// alternation of Lower and Upper while preserving Exact and Undefined:
//
public enum EvalType : byte { Exact, Lower, Undefined, Upper };
#endregion                              // EvalType Enum

#region MoveType Enum
public enum MoveType : byte {
  PawnAboveCapture,
  PawnBelowCapture,
  DiagAboveCapture,
  DiagBelowCapture,
  OrthAboveCapture,
  OrthBelowCapture,
  KnightCapture,
  KingCapture,

  KnightMove,
  DiagAboveMove,
  DiagBelowMove,
  OrthAboveMove,
  OrthBelowMove,
  KingMove,
  PawnAboveMove,
  PawnBelowMove,
}
#endregion                              // MoveType Enum

#region PositionType Enum
public enum PositionType : byte {
  Prefix, FEN, EPD }
#endregion                              // PositionType Enum
#endregion                              // Enumerations
