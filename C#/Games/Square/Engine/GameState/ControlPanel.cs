//
// Copyright (C) 2010-2024, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created File
//
// Conditionals:
//
//#define SyzygyControls
//#define ExampleOptionTypes

namespace Engine;

using Command;

using static Board;
using static Command.Control.ControlName;
using static Logging.Logger;

partial class GameState {
  #region Fields
  //
  // New Control Checklist [cf. Event Handler Implementation in Control class]
  // ---------------------
  // 1) Define Control Name, Type, Limits, and Default Value here
  // 2) Define Property in GameStateProperty.cs or an appropriate class
  // 3) Define Property Changed Event Handler below
  // 4) Define Wireup Method for the Event Handler and define the default value
  // 5) Wireup in the GameState() constructor; [ToDo]Automate this via reflection
  // 6) Use the Property, potentially reporting initial state in appendOptions()
  //
  public static readonly Control[] ControlPanel = {
    new Button {
      Name = Clear_Hash                 // Example had compound name of "Clear Hash"
    },
    new SpinSetting {                   // Step 1/6: Define Control
      Name = MultiPV,                   //[UCI]# of variations sought
      Default = "1",
      Min = 1,
      Max = 64                          // High Max for proof of mate solutions
    },
    new SpinSetting {
      Name = QXPLength,
      Default = nQXPSelectionDefault.ToString(),
      Min = 1,
      Max = 256
    },
    new SpinSetting {
      Name = QXPBuckets,
      Default = "4",
      Min = 1,
      Max = 16
    },
    new SpinSetting {
      Name = XPLength,                  //[UCI]Hash = XPLength * XPBuckets / sizeof Transposition
      Default = nXPSelectionDefault.ToString(),
      Min = 1,
      Max = 256
    },
    new SpinSetting {
      Name = XPBuckets,
      Default = "2",
      Min = 1,
      Max = 16
    },
    new SpinSetting {
      Name = XPMLength,
      Default = nXPMSelectionDefault.ToString(),
      Min = 1,
      Max = 256
    },
    new SpinSetting {
      Name = XPMBuckets,
      Default = "6",
      Min = 1,
      Max = 16
    },
    new SpinSetting {
      Name = ExpectedMoves,             // ExpectedMovesToGo for "sudden death" time controls
      Default = "32",
      Min = 1,
      Max = 128
    },
    new SpinSetting {
      Name = Contempt,
      Default = "0",
      Min = -250,
      Max = 250
    },
    new SpinSetting {
      Name = Late,
      Default = "2",
      Min = 0,
      Max = 4
    },
    new SpinSetting {
      Name = Checks,
      // 4 sufficient for Caruana v Gustafsson Mate [in 12-ply] w zMateDepthMin = 4
      // 6 sufficient to solve Johannessen v Fischer #8 [in 11-ply]
      // 6 solves Perpetual [in 13-ply]
      // 8 solves Perpetual faster and finds Kramnik v Meier 2012-07-22 [in 12-ply]
      Default = "6",
      Min = 0,
      Max = 15
    },
    new SpinSetting {
      Name = Threat,                    // Looks for Mate Threats, currently
      Default = "0",
      Min = 0,
      Max = 3
    },
    new SpinSetting {
      Name = Singular,
      Default = "2",
      Min = 0,
      Max = 15
    },
    new CheckSetting {
      Name = Aspiration,
      Default = "false"
    },
    new CheckSetting {
      Name = Flip,
      Default = "false"
    },
    new CheckSetting {
      Name = Futility,
      Default = "true",
      IsHidden = false
    },
    new CheckSetting {
      Name = NullMove,
      Default = "true",
      IsHidden = false
    },
    new CheckSetting {
      Name = Occam,                     // Forward Pruning not recommended
      Default = "false"
    },
    new CheckSetting {
      // Pure Algebraic Coordinate Notation (PACN) vs Algebraic Notation (AN)
      Name = Pure,
      Default = "false",
      IsHidden = true                   //[Debug]
    },
    new CheckSetting {
      Name = Heartbeat,
      Default = "false",
      IsHidden = true                   //[Debug]
    },
    new SpinSetting {
      Name = HeartbeatMS,               // Heartbeat Period in msec
      Default = "7500",
      Min = 250,
      Max = 60000,
      IsHidden = true                   //[Debug]
    },
    new CheckSetting {
      Name = Ponder,                    //[UCI]In case Time Management may be affected
      Default = "true"
    },
    new CheckSetting {
      Name = UCI_AnalyseMode,           //[UCI]Engine is not playing a game
      Default = "false"
    },
    new CheckSetting {
      Name = UCI_ShowCurrLine,          //[UCI]
      Default = "true"
    },
    new StringSetting {
      Name = UCI_Opponent,              //[UCI]
      Default = "Human"
    },
    new ComboSetting {
      Name = Language,
      Items = Locales.Select(locale => locale.Language).ToArray(),
      Default = "English",
      IsHidden = true
    },
    new ComboSetting {
      Name = LoggerLevel,              //[Logger]
      Items = Enum.GetValues(typeof(LogLevel))
                  .Cast<LogLevel>()
                  .Select(o => o.ToString())
                  .ToArray<String>(),
      Default = LogLevel.data.ToString()
    },
    new StringSetting {
      Name = LoggerPath,               //[Logger]
      Default = PathDefault
    },
#if SyzygyControls
  new StringSetting {
      Name = SyzygyPath,            //[UCI]
      Default = @"c:\Syzygy"
    },
    new SpinSetting {
      Name = SyzygyCache,           //[UCI]
      Default = "4",
      Min = 1,
      Max = 32
    },
    new CheckSetting {
      Name = Syzygy50MoveRule,
      Default = "true"
    },
    new SpinSetting {
      Name = SyzygyProbeDepth,
      Default = "14",
      Min = 1,
      Max = 32
    },
    new SpinSetting {
      Name = SyzygyProbeLimit,
      Default = "5",
      Min = 3,
      Max = 8
    },
#endif
#if ExampleOptionTypes
    new SpinSetting {
      Name = Selectivity,
      Default = "2",
      Min = 0,
      Max = 4
    },
    new ComboSetting {
      Name = Style,
      Items = new String[] { "Solid", "BestMove", "Risky" },
      Default = "BestMove"
    },
#endif
  };
  #endregion                            // Fields
}
