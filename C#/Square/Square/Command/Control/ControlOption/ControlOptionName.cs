//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2023-06-08 CNHume]Created Type
//
// Conditionals:
//
//#define ExampleOptionTypes
//#define SyzygyControls

using System.ComponentModel;

namespace Command;

partial class Control {
  #region Enumerations
  public enum ControlOptionName : byte {
    None = 0,
    [Description("Clear Hash")]
    Clear_Hash,
    MultiPV,              //[UCI]
    QXPLength,
    QXPBuckets,
    XPLength,
    XPBuckets,
    XPMLength,
    XPMBuckets,
    ExpectedMoves,
    Contempt,
    Late,
    Checks,
    Threat,
    Singular,
    Aspiration,
    Flip,
    Futility,
    NullMove,
    Occam,
    Pure,                 //[Debug]
    Heartbeat,            //[Debug]
    HeartbeatMS,          //[Debug]
    Ponder,               //[UCI]
    UCI_AnalyseMode,      //[UCI]
    UCI_ShowCurrLine,     //[UCI]
    UCI_Opponent,         //[UCI]
    Language,
    LoggerLevel,
    LoggerPath,
#if SyzygyControls
    SyzygyPath,           //[UCI]
    SyzygyCache,          //[UCI]
    Syzygy50MoveRule,
    SyzygyProbeDepth,
    SyzygyProbeLimit,
#endif
#if ExampleOptionTypes
    Selectivity,
    Style,
#endif
  }
  #endregion
}
