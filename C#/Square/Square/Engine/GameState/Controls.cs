//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created File
//
// Conditionals:
//
//#define ThreadSafeTank
//#define SyzygyControls
//#define ExampleOptionTypes

using System.Diagnostics.CodeAnalysis;

using Command;

namespace Engine;

using Exceptions;

using static Board;
using static Control;
using static Control.ControlName;
using static Logging.Logger;
using static Position;

//
// Type Aliases:
//
using Eval = Int16;

partial class GameState {
  #region Constants
  //
  // New Control Checklist
  // ---------------------
  // 1) Define Control Name, Type, Limits, and Default Value here
  // 2) Define Property in GameStateProperty.cs or an appropriate class
  // 3) Define Property Changed Event Handler below
  // 4) Define Wireup Method for the Event Handler and define the default value
  // 5) Wireup in the GameState() constructor; [ToDo]Automate this via reflection
  // 6) Use the Property, potentially reporting initial state in appendOptions()
  //
  public static readonly Control[] Controls = {
    new Button {
      Name = Clear_Hash                 // Example had compound name of "Clear Hash"
    },
    new SpinSetting {                   // Step 1/6: Define Control
      Name = MultiPV,                   //[UCI]
      Default = "1",
      Min = 1,
      Max = 12
    },
    new SpinSetting {
      Name = QXPLength,
      Default = nQXPSelectionDefault.ToString(),
      Min = 1,
      Max = 128
    },
    new SpinSetting {
      Name = QXPBuckets,
      Default = "4",
      Min = 1,
      Max = 8
    },
    new SpinSetting {
      Name = XPLength,                  //[UCI]Hash = XPLength * XPBuckets / sizeof Transposition
      Default = nXPSelectionDefault.ToString(),
      Min = 1,
      Max = 128
    },
    new SpinSetting {
      Name = XPBuckets,
      Default = "2",
      Min = 1,
      Max = 8
    },
    new SpinSetting {
      Name = XPMLength,
      Default = nXPMSelectionDefault.ToString(),
      Min = 1,
      Max = 128
    },
    new SpinSetting {
      Name = XPMBuckets,
      Default = "6",
      Min = 1,
      Max = 8
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
      Default = "6",                    // 8
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
      Name = Futility,                  // Only an option for testing purposes
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
      Name = UCI_AnalyseMode,           //[UCI]This means the Engine is not playing a game
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
  #endregion                            // Constants

  #region Event Handlers
  protected void ClearHashButton_Click(Object? sender, EventArgs e) {
    var button = (Button?)sender;
#if !ThreadSafeTank
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");
#endif
    XPTank.Clear(0, XPTank.LookupBuckets);
    QXPTank.Clear(0, QXPTank.LookupBuckets);
  }

  // Step 5/6: Define an Event Handler, following the standard naming convention
  protected void MultiPVValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      newVariations(Convert.ToInt32(value));
  }

  protected void QXPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var value = setting?.GetValue();
    if (value != null)
      QXPTank.Init(Convert.ToInt32(value));
  }

  protected void QXPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting?.GetValue();
    if (value != null)
      QXPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var value = setting?.GetValue();
    if (value != null)
      XPTank.Init(Convert.ToInt32(value));
  }

  protected void XPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting?.GetValue();
    if (value != null)
      XPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPMLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var value = setting?.GetValue();
    if (value != null)
      XPMTank.Init(Convert.ToInt32(value));
  }

  protected void XPMBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting?.GetValue();
    if (value != null)
      XPMTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void ExpectedMovesToGoValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      ExpectedMovesToGo = Convert.ToUInt16(value);
  }

  protected void ContemptValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      ContemptValue = (Eval)Convert.ToInt16(value);
  }

  protected void LateValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      SetNibble(ref ExtensionLimit, vLate, Convert.ToUInt16(value));
  }

  protected void ChecksValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      SetNibble(ref ExtensionLimit, vCheck, Convert.ToUInt16(value));
  }

  protected void ThreatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      SetNibble(ref ExtensionLimit, vThreat, Convert.ToUInt16(value));
  }

  protected void SingularValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      SetNibble(ref ExtensionLimit, vSingular, Convert.ToUInt16(value));
  }

  protected void AspirationValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsAspiration = Convert.ToBoolean(value);
  }

  protected void FlipValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsFlip = Convert.ToBoolean(value);
  }

  protected void FutilityValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsFutility = Convert.ToBoolean(value);
  }

  protected void NullPruneValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsNullPrune = Convert.ToBoolean(value);
  }

  protected void OccamValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsOccam = Convert.ToBoolean(value);
  }

  protected void PureValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsPure = Convert.ToBoolean(value);
  }

  protected void HeartbeatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsHeartbeat = Convert.ToBoolean(value);
  }

  protected void HeartbeatMSValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      HeartbeatPeriodMS = Convert.ToUInt16(value);
  }

  protected void PonderValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsPonderEnabled = Convert.ToBoolean(value);
  }

  protected void AnalyseModeValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsAnalyseMode = Convert.ToBoolean(value);
  }

  protected void ShowingLineValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (CheckSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      IsShowingLine = Convert.ToBoolean(value);
  }

  protected void OpponentValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (StringSetting?)sender;
    Opponent = setting?.GetValue();
  }

  protected void LogLevelValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (ComboSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      Level = (LogLevel)Enum.Parse(typeof(LogLevel), value);
  }

  protected void LogPathValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (StringSetting?)sender;
    Path = setting?.GetValue();
  }

  protected void LanguageValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    var setting = (ComboSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      SetLanguage(value);
  }
  #endregion                            // Event Handlers

  #region Event Handler Subscriptions
  protected static Control? findControl(ControlName optionName) {
    return FindControl(Controls, optionName);
  }

  private void wireClearHash() {
    var button = (Button?)findControl(Clear_Hash);
    if (button != null)
      button.Click += ClearHashButton_Click;
  }

  [MemberNotNull(nameof(Variation))]
  private void wireMultiPV() {
    newVariations();

    var setting = (SpinSetting?)findControl(MultiPV);
    if (setting != null) {
      // Step 6/6: Dynamically subscribe handler to the event
      setting.PropertyChanged += MultiPVValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(QXPTank))]
  private void wireQXPBuckets() {
    newQXPTank();

    var setting = (SpinSetting?)findControl(QXPBuckets);
    if (setting != null) {
      setting.PropertyChanged += QXPBucketsValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(QXPTank))]
  private void wireQXP() {
    //
    //[Note]Establish BucketsDefault before calling Tank.Init()
    //
    wireQXPBuckets();

    var setting = (SpinSetting?)findControl(QXPLength);
    if (setting != null) {
      setting.PropertyChanged += QXPLengthValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(XPTank))]
  private void wireXPBuckets() {
    newXPTank();

    var setting = (SpinSetting?)findControl(XPBuckets);
    if (setting != null) {
      setting.PropertyChanged += XPBucketsValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(XPTank))]
  private void wireXP() {
    //
    //[Note]Establish BucketsDefault before calling Tank.Init()
    //
    wireXPBuckets();

    var setting = (SpinSetting?)findControl(XPLength);
    if (setting != null) {
      setting.PropertyChanged += XPLengthValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(XPMTank))]
  private void wireXPMBuckets() {
    newXPMTank();

    var setting = (SpinSetting?)findControl(XPMBuckets);
    if (setting != null) {
      setting.PropertyChanged += XPMBucketsValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  [MemberNotNull(nameof(XPMTank))]
  private void wireXPM() {
    //
    //[Note]Establish BucketsDefault before calling Tank.Init()
    //
    wireXPMBuckets();

    var setting = (SpinSetting?)findControl(XPMLength);
    if (setting != null) {
      setting.PropertyChanged += XPMLengthValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireExpectedMovesToGo() {
    var setting = (SpinSetting?)findControl(ExpectedMoves);
    if (setting != null) {
      setting.PropertyChanged += ExpectedMovesToGoValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireContempt() {
    var setting = (SpinSetting?)findControl(Contempt);
    if (setting != null) {
      setting.PropertyChanged += ContemptValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireLate() {
    var setting = (SpinSetting?)findControl(Late);
    if (setting != null) {
      setting.PropertyChanged += LateValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireChecks() {
    var setting = (SpinSetting?)findControl(Checks);
    if (setting != null) {
      setting.PropertyChanged += ChecksValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireThreat() {
    var setting = (SpinSetting?)findControl(Threat);
    if (setting != null) {
      setting.PropertyChanged += ThreatValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireSingular() {
    var setting = (SpinSetting?)findControl(Singular);
    if (setting != null) {
      setting.PropertyChanged += SingularValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireAspiration() {
    var setting = (CheckSetting?)findControl(Aspiration);
    if (setting != null) {
      setting.PropertyChanged += AspirationValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireFlip() {
    var setting = (CheckSetting?)findControl(Flip);
    if (setting != null) {
      setting.PropertyChanged += FlipValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireFutility() {
    var setting = (CheckSetting?)findControl(Futility);
    if (setting != null) {
      setting.PropertyChanged += FutilityValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireNullPrune() {
    var setting = (CheckSetting?)findControl(NullMove);
    if (setting != null) {
      setting.PropertyChanged += NullPruneValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireOccam() {
    var setting = (Setting?)findControl(Occam);
    if (setting != null) {
      setting.PropertyChanged += OccamValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wirePure() {
    var setting = (CheckSetting?)findControl(Pure);
    if (setting != null) {
      setting.PropertyChanged += PureValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireHeartbeatMS() {
    var setting = (SpinSetting?)findControl(HeartbeatMS);
    if (setting != null) {
      setting.PropertyChanged += HeartbeatMSValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireHeartbeat() {
    var setting = (CheckSetting?)findControl(Heartbeat);
    if (setting != null) {
      setting.PropertyChanged += HeartbeatValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wirePonder() {
    var setting = (CheckSetting?)findControl(Ponder);
    if (setting != null) {
      setting.PropertyChanged += PonderValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireAnalyseMode() {
    var setting = (CheckSetting?)findControl(UCI_AnalyseMode);
    if (setting != null) {
      setting.PropertyChanged += AnalyseModeValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireShowingLine() {
    var setting = (CheckSetting?)findControl(UCI_ShowCurrLine);
    if (setting != null) {
      setting.PropertyChanged += ShowingLineValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireOpponent() {
    var setting = (StringSetting?)findControl(UCI_Opponent);
    if (setting != null) {
      setting.PropertyChanged += OpponentValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireLanguage() {
    var setting = (ComboSetting?)findControl(Language);
    if (setting != null) {
      setting.PropertyChanged += LanguageValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireLogLevel() {
    var setting = (ComboSetting?)findControl(LoggerLevel);
    if (setting != null) {
      setting.PropertyChanged += LogLevelValue_PropertyChanged;
      setting.SetDefault();
    }
  }

  private void wireLogPath() {
    var setting = (StringSetting?)findControl(LoggerPath);
    if (setting != null) {
      setting.PropertyChanged += LogPathValue_PropertyChanged;
      setting.SetDefault();
    }
  }
  #endregion                            // Event Handler Subscriptions
}
