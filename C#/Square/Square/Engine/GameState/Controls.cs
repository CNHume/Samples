//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created File
//
// Conditionals:
//
//#define ThreadSafeTank
//#define TestOptionTypes
//#define SyzygyControls

using System.Diagnostics.CodeAnalysis;

namespace Engine {
  using Command;

  using Exceptions;

  using static Board;
  using static Command.Control;
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
    // 1) Define Control its Limits and Default Value here
    // 2) Define Property in GameStateProperty.cs or an appropriate class
    // 3) Define Property Changed Event Handler below
    // 4) Define Wireup Method for the Event Handler and define the default value
    // 5) Wireup in the GameState() constructor; [ToDo]Automate this via reflection
    // 6) Use the Property, potentially reporting initial state in appendOptions()
    //
    public static readonly Control[] Controls = {
#if TestOptionTypes
      new Setting {
        Option = new ControlOption {
          Name = "Selectivity",         //[Example]
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 4
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Style",               //[Example]
          Type = OptionType.combo,
          Items = new String[] { "Solid", "BestMove", "Risky" },
          Default = "BestMove"
        }
      },
#endif
      new Button {
        Option = new ControlOption {
          Name = "Clear Hash",          // Example had compound name of "Clear Hash"
          Type = OptionType.button
        }
      },
      new Setting {                     // Step 1/6: Define Control its Limits and Default Value here
        Option = new ControlOption {
          Name = "MultiPV",             //[UCI]
          Type = OptionType.spin,
          Default = "1",
          Min = 1,
          Max = 12
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "QXPLength",
          Type = OptionType.spin,
          Default = "16",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "QXPBuckets",
          Type = OptionType.spin,
          Default = "4",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "XPLength",            //[UCI]Hash = XPLength * XPBuckets / sizeof Transposition
          Type = OptionType.spin,
          Default = "48",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "XPBuckets",
          Type = OptionType.spin,
          Default = "2",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "XPMLength",
          Type = OptionType.spin,
          Default = "8",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "XPMBuckets",
          Type = OptionType.spin,
          Default = "6",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "ExpectedMoves",       // ExpectedMovesToGo for "sudden death" time controls
          Type = OptionType.spin,
          Default = "32",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Contempt",
          Type = OptionType.spin,
          Default = "0",
          Min = -250,
          Max = 250
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Late",
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 4
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Checks",
          Type = OptionType.spin,
          Default = "6",                // 8
          Min = 0,
          Max = 15
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Threat",              // Looks for Mate Threats, currently
          Type = OptionType.spin,
          Default = "0",
          Min = 0,
          Max = 3
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Singular",
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 15
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Aspiration",
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Flip",
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Futility",            // Only an option for testing purposes
          Type = OptionType.check,
          Default = "true",
          IsHidden = false
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "NullMove",
          Type = OptionType.check,
          Default = "true",
          IsHidden = false
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Occam",               // Forward Pruning is not a good idea
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Pure",                // PACN vs. AN [UCI vs. Command Line]
          Type = OptionType.check,
          Default = "false",
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Heartbeat",
          Type = OptionType.check,
          Default = "false",
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "HeartbeatMS",         // Heartbeat Period in msec
          Type = OptionType.spin,
          Default = "7500",
          Min = 250,
          Max = 60000,
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Ponder",              //[UCI]In case Time Management may be affected
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "UCI_AnalyseMode",     //[UCI]This means the Engine is not playing a game
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "UCI_ShowCurrLine",    //[UCI]
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "UCI_Opponent",        //[UCI]
          Type = OptionType.@string,
          Default = "Human"
        }
      },
#if SyzygyControls
      new Setting {
        Option = new ControlOption {
          Name = "SyzygyPath",          //[UCI]
          Type = OptionType.@string,
          Default = @"c:\Syzygy"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "SyzygyCache",         //[UCI]
          Type = OptionType.spin,
          Default = "4",
          Min = 1,
          Max = 32
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "Syzygy50MoveRule",
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "SyzygyProbeDepth",
          Type = OptionType.spin,
          Default = "14",
          Min = 1,
          Max = 32
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "SyzygyProbeLimit",
          Type = OptionType.spin,
          Default = "5",
          Min = 3,
          Max = 8
        }
      },
#endif
      new Setting {
        Option = new ControlOption {
          Name = "Language",
          Type = OptionType.combo,
          Items = Locales.Select(locale => locale.Language).ToArray(),
          Default = "English",
          IsHidden = true
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "LogLevel",            //[Logger]
          Type = OptionType.combo,
          Items = Enum.GetValues(typeof(Level))
                    .Cast<Level>()
                    .Select(o => o.ToString())
                    .ToArray<String>(),
          Default = Level.data.ToString()
        }
      },
      new Setting {
        Option = new ControlOption {
          Name = "LogPath",             //[Logger]
          Type = OptionType.@string,
          Default = LogPathDefault
        }
      }
    };
    #endregion

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
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue)
        newVariations(setting.Selection.Value);
    }

    protected void QXPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      if (setting != null && setting.Selection.HasValue)
        QXPTank.Init(setting.Selection.Value);
    }

    protected void QXPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      if (setting != null && setting.Selection.HasValue)
        QXPTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void XPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      if (setting != null && setting.Selection.HasValue)
        XPTank.Init(setting.Selection.Value);
    }

    protected void XPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      if (setting != null && setting.Selection.HasValue)
        XPTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void XPMLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      if (setting != null && setting.Selection.HasValue)
        XPMTank.Init(setting.Selection.Value);
    }

    protected void XPMBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      if (setting != null && setting.Selection.HasValue)
        XPMTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void ExpectedMovesToGoValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue)
        ExpectedMovesToGo = (UInt16)setting.Selection;
    }

    protected void ContemptValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue)
        Contempt = (Eval)setting.Selection;
    }

    protected void LateValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue) {
        var uSelection = (UInt32)setting.Selection.Value;
        SetNibble(ref ExtensionLimit, vLate, uSelection);
      }
    }

    protected void ChecksValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue) {
        var uSelection = (UInt32)setting.Selection.Value;
        SetNibble(ref ExtensionLimit, vCheck, uSelection);
      }
    }

    protected void ThreatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue) {
        var uSelection = (UInt32)setting.Selection.Value;
        SetNibble(ref ExtensionLimit, vThreat, uSelection);
      }
    }

    protected void SingularValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue) {
        var uSelection = (UInt32)setting.Selection.Value;
        SetNibble(ref ExtensionLimit, vSingular, uSelection);
      }
    }

    protected void AspirationValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsAspiration = setting.IsChecked.Value;
    }

    protected void FlipValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsFlip = setting.IsChecked.Value;
    }

    protected void FutilityValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsFutility = setting.IsChecked.Value;
    }

    protected void NullPruneValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsNullPrune = setting.IsChecked.Value;
    }

    protected void OccamValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsOccam = setting.IsChecked.Value;
    }

    protected void PureValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsPure = setting.IsChecked.Value;
    }

    protected void HeartbeatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsHeartbeat = setting.IsChecked.Value;
    }

    protected void HeartbeatMSValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.Selection.HasValue)
        HeartbeatMS = (UInt16)setting.Selection.Value;
    }

    protected void PonderValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsPonderEnabled = setting.IsChecked.Value;
    }

    protected void AnalyseModeValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsAnalyseMode = setting.IsChecked.Value;
    }

    protected void ShowingLineValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null && setting.IsChecked.HasValue)
        IsShowingLine = setting.IsChecked.Value;
    }

    protected void OpponentValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null)
        Opponent = setting.Text;
    }

    protected void LogLevelValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting?.Text != null) {
        LogLevel = (Level)Enum.Parse(typeof(Level), setting.Text);
      }
    }

    protected void LogPathValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null)
        LogPath = setting.Text;
    }

    protected void LanguageValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
      var setting = (Setting?)sender;
      if (setting != null)
        SetLanguage(setting.Text);
    }
    #endregion

    #region Event Handler Subscriptions
    protected static Control? findControl(String sName) {
      return Control.FindControl(Controls, sName);
    }

    private void wireClearHash() {
      var button = (Button?)findControl("Clear Hash");
      if (button != null)
        button.Click += ClearHashButton_Click;
    }

    private void wireMultiPV() {
      var setting = (Setting?)findControl("MultiPV");
      if (setting != null) {
        // Step 6/6: Dynamically subscribe handler to the event
        setting.PropertyChanged += MultiPVValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(QXPTank))]
    private void wireQXPBuckets() {
      newQXPTank();

      var setting = (Setting?)findControl("QXPBuckets");
      if (setting != null) {
        setting.PropertyChanged += QXPBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(QXPTank))]
    private void wireQXP() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireQXPBuckets();

      var setting = (Setting?)findControl("QXPLength");
      if (setting != null) {
        setting.PropertyChanged += QXPLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(XPTank))]
    private void wireXPBuckets() {
      newXPTank();

      var setting = (Setting?)findControl("XPBuckets");
      if (setting != null) {
        setting.PropertyChanged += XPBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(XPTank))]
    private void wireXP() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireXPBuckets();

      var setting = (Setting?)findControl("XPLength");
      if (setting != null) {
        setting.PropertyChanged += XPLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(XPMTank))]
    private void wireXPMBuckets() {
      newXPMTank();

      var setting = (Setting?)findControl("XPMBuckets");
      if (setting != null) {
        setting.PropertyChanged += XPMBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    [MemberNotNull(nameof(XPMTank))]
    private void wireXPM() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireXPMBuckets();

      var setting = (Setting?)findControl("XPMLength");
      if (setting != null) {
        setting.PropertyChanged += XPMLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireExpectedMovesToGo() {
      var setting = (Setting?)findControl("ExpectedMoves");
      if (setting != null) {
        setting.PropertyChanged += ExpectedMovesToGoValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireContempt() {
      var setting = (Setting?)findControl("Contempt");
      if (setting != null) {
        setting.PropertyChanged += ContemptValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLate() {
      var setting = (Setting?)findControl("Late");
      if (setting != null) {
        setting.PropertyChanged += LateValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireChecks() {
      var setting = (Setting?)findControl("Checks");
      if (setting != null) {
        setting.PropertyChanged += ChecksValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireThreat() {
      var setting = (Setting?)findControl("Threat");
      if (setting != null) {
        setting.PropertyChanged += ThreatValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireSingular() {
      var setting = (Setting?)findControl("Singular");
      if (setting != null) {
        setting.PropertyChanged += SingularValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireAspiration() {
      var setting = (Setting?)findControl("Aspiration");
      if (setting != null) {
        setting.PropertyChanged += AspirationValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireFlip() {
      var setting = (Setting?)findControl("Flip");
      if (setting != null) {
        setting.PropertyChanged += FlipValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireFutility() {
      var setting = (Setting?)findControl("Futility");
      if (setting != null) {
        setting.PropertyChanged += FutilityValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireNullPrune() {
      var setting = (Setting?)findControl("NullMove");
      if (setting != null) {
        setting.PropertyChanged += NullPruneValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireOccam() {
      var setting = (Setting?)findControl("Occam");
      if (setting != null) {
        setting.PropertyChanged += OccamValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wirePure() {
      var setting = (Setting?)findControl("Pure");
      if (setting != null) {
        setting.PropertyChanged += PureValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireHeartbeatMS() {
      var setting = (Setting?)findControl("HeartbeatMS");
      if (setting != null) {
        setting.PropertyChanged += HeartbeatMSValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireHeartbeat() {
      var setting = (Setting?)findControl("Heartbeat");
      if (setting != null) {
        setting.PropertyChanged += HeartbeatValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wirePonder() {
      var setting = (Setting?)findControl("Ponder");
      if (setting != null) {
        setting.PropertyChanged += PonderValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireAnalyseMode() {
      var setting = (Setting?)findControl("UCI_AnalyseMode");
      if (setting != null) {
        setting.PropertyChanged += AnalyseModeValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireShowingLine() {
      var setting = (Setting?)findControl("UCI_ShowCurrLine");
      if (setting != null) {
        setting.PropertyChanged += ShowingLineValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireOpponent() {
      var setting = (Setting?)findControl("UCI_Opponent");
      if (setting != null) {
        setting.PropertyChanged += OpponentValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLogLevel() {
      var setting = (Setting?)findControl("LogLevel");
      if (setting != null) {
        setting.PropertyChanged += LogLevelValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLogPath() {
      var setting = (Setting?)findControl("LogPath");
      if (setting != null) {
        setting.PropertyChanged += LogPathValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLanguage() {
      var setting = (Setting?)findControl("Language");
      if (setting != null) {
        setting.PropertyChanged += LanguageValue_PropertyChanged;
        setting.SetDefault();
      }
    }
    #endregion
  }
}
