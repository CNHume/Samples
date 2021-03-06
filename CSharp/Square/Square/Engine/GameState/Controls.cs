﻿//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2013-08-28 CNHume]Created File
//
// Conditionals:
//
//#define ThreadSafeTank
//#define TestOptionTypes
//#define SyzygyControls

namespace Engine {
  using static Board;
  using Command;
  using Exceptions;
  using static Logging.Logger;
  using static Position;

  using System;
  using System.Linq;

  //
  // Type Aliases:
  //
  using Eval = System.Int16;

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
        Option = new Option {
          Name = "Selectivity",         //[Example]
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 4
        }
      },
      new Setting {
        Option = new Option {
          Name = "Style",               //[Example]
          Type = OptionType.combo,
          Items = new String[] { "Solid", "BestMove", "Risky" },
          Default = "BestMove"
        }
      },
#endif
      new Button {
        Option = new Option {
          Name = "Clear Hash",          // Example had compound name of "Clear Hash"
          Type = OptionType.button
        }
      },
      new Setting {                     // Step 1/6: Define Control its Limits and Default Value here
        Option = new Option {
          Name = "MultiPV",             //[UCI]
          Type = OptionType.spin,
          Default = "1",
          Min = 1,
          Max = 12
        }
      },
      new Setting {
        Option = new Option {
          Name = "QXPLength",
          Type = OptionType.spin,
          Default = "4",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new Option {
          Name = "QXPBuckets",
          Type = OptionType.spin,
          Default = "4",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new Option {
          Name = "XPLength",            //[UCI]Hash = XPLength * XPBuckets / sizeof Transposition
          Type = OptionType.spin,
          Default = "16",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new Option {
          Name = "XPBuckets",
          Type = OptionType.spin,
          Default = "2",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new Option {
          Name = "XPMLength",
          Type = OptionType.spin,
          Default = "8",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new Option {
          Name = "XPMBuckets",
          Type = OptionType.spin,
          Default = "6",
          Min = 1,
          Max = 8
        }
      },
      new Setting {
        Option = new Option {
          Name = "ExpectedMoves",       // ExpectedMovesToGo for "sudden death" time controls
          Type = OptionType.spin,
          Default = "32",
          Min = 1,
          Max = 128
        }
      },
      new Setting {
        Option = new Option {
          Name = "Contempt",
          Type = OptionType.spin,
          Default = "0",
          Min = -250,
          Max = 250
        }
      },
      new Setting {
        Option = new Option {
          Name = "Late",
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 4
        }
      },
      new Setting {
        Option = new Option {
          Name = "Checks",
          Type = OptionType.spin,
          Default = "6",                // 8
          Min = 0,
          Max = 15
        }
      },
      new Setting {
        Option = new Option {
          Name = "Threat",              // Looks for Mate Threats, currently
          Type = OptionType.spin,
          Default = "0",
          Min = 0,
          Max = 3
        }
      },
      new Setting {
        Option = new Option {
          Name = "Singular",
          Type = OptionType.spin,
          Default = "2",
          Min = 0,
          Max = 15
        }
      },
      new Setting {
        Option = new Option {
          Name = "Aspiration",
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new Option {
          Name = "Flip",
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new Option {
          Name = "Futility",            // Only an option for testing purposes
          Type = OptionType.check,
          Default = "true",
          IsHidden = false
        }
      },
      new Setting {
        Option = new Option {
          Name = "NullMove",
          Type = OptionType.check,
          Default = "true",
          IsHidden = false
        }
      },
      new Setting {
        Option = new Option {
          Name = "Occam",               // Forward Pruning is not a good idea
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new Option {
          Name = "Pure",                // PACN vs. AN [UCI vs. Command Line]
          Type = OptionType.check,
          Default = "false",
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new Option {
          Name = "Heartbeat",
          Type = OptionType.check,
          Default = "false",
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new Option {
          Name = "HeartbeatMS",         // Heartbeat Period in msec
          Type = OptionType.spin,
          Default = "7500",
          Min = 250,
          Max = 60000,
          IsHidden = true               //[Debug]
        }
      },
      new Setting {
        Option = new Option {
          Name = "Ponder",              //[UCI]In case Time Management may be affected
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new Option {
          Name = "UCI_AnalyseMode",     //[UCI]This means the Engine is not playing a game
          Type = OptionType.check,
          Default = "false"
        }
      },
      new Setting {
        Option = new Option {
          Name = "UCI_ShowCurrLine",    //[UCI]
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new Option {
          Name = "UCI_Opponent",        //[UCI]
          Type = OptionType.@string,
          Default = "Human"
        }
      },
#if SyzygyControls
      new Setting {
        Option = new Option {
          Name = "SyzygyPath",          //[UCI]
          Type = OptionType.@string,
          Default = @"c:\Syzygy"
        }
      },
      new Setting {
        Option = new Option {
          Name = "SyzygyCache",         //[UCI]
          Type = OptionType.spin,
          Default = "4",
          Min = 1,
          Max = 32
        }
      },
      new Setting {
        Option = new Option {
          Name = "Syzygy50MoveRule",
          Type = OptionType.check,
          Default = "true"
        }
      },
      new Setting {
        Option = new Option {
          Name = "SyzygyProbeDepth",
          Type = OptionType.spin,
          Default = "14",
          Min = 1,
          Max = 32
        }
      },
      new Setting {
        Option = new Option {
          Name = "SyzygyProbeLimit",
          Type = OptionType.spin,
          Default = "5",
          Min = 3,
          Max = 8
        }
      },
#endif
      new Setting {
        Option = new Option {
          Name = "Language",
          Type = OptionType.combo,
          Items = Board.Locales.Select(locale => locale.Language).ToArray<String>(),
          Default = "English",
          IsHidden = true
        }
      },
      new Setting {
        Option = new Option {
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
        Option = new Option {
          Name = "LogPath",             //[Logger]
          Type = OptionType.@string,
          Default = sLogPathDefault
        }
      }
    };
    #endregion

    #region Event Handlers
    protected void ClearHashButton_Click(Object sender, EventArgs e) {
      var button = (Button)sender;
#if !ThreadSafeTank
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");
#endif
      XPTank.Clear(0, XPTank.LookupBuckets);
      QXPTank.Clear(0, QXPTank.LookupBuckets);
    }

    // Step 5/6: Define an Event Handler, following the standard naming convention
    protected void MultiPVValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      newVariations(setting.Selection.Value);
    }

    protected void QXPLengthValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      QXPTank.Init(setting.Selection.Value);
    }

    protected void QXPBucketsValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      QXPTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void XPLengthValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      XPTank.Init(setting.Selection.Value);
    }

    protected void XPBucketsValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      XPTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void XPMLengthValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      XPMTank.Init(setting.Selection.Value);
    }

    protected void XPMBucketsValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      if (IsSearchInProgress)
        throw new PositionException("Search in progress");

      //[Note]BucketsDefault must be updated before Tank.init() is called
      XPMTank.BucketsDefault = (UInt16)setting.Selection;
    }

    protected void ExpectedMovesToGoValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      ExpectedMovesToGo = (UInt16)setting.Selection;
    }

    protected void ContemptValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      Contempt = (Eval)setting.Selection;
    }

    protected void LateValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      var uSelection = (UInt32)setting.Selection.Value;
      setNibble(ref ExtensionLimit, vLate, uSelection);
    }

    protected void ChecksValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      var uSelection = (UInt32)setting.Selection.Value;
      setNibble(ref ExtensionLimit, vCheck, uSelection);
    }

    protected void ThreatValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      var uSelection = (UInt32)setting.Selection.Value;
      setNibble(ref ExtensionLimit, vThreat, uSelection);
    }

    protected void SingularValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      var uSelection = (UInt32)setting.Selection.Value;
      setNibble(ref ExtensionLimit, vSingular, uSelection);
    }

    protected void AspirationValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsAspiration = setting.IsChecked.Value;
    }

    protected void FlipValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsFlip = setting.IsChecked.Value;
    }

    protected void FutilityValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsFutility = setting.IsChecked.Value;
    }

    protected void NullPruneValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsNullPrune = setting.IsChecked.Value;
    }

    protected void OccamValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsOccam = setting.IsChecked.Value;
    }

    protected void PureValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsPure = setting.IsChecked.Value;
    }

    protected void HeartbeatValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsHeartbeat = setting.IsChecked.Value;
    }

    protected void HeartbeatMSValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      HeartbeatMS = (UInt16)setting.Selection.Value;
    }

    protected void PonderValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsPonderEnabled = setting.IsChecked.Value;
    }

    protected void AnalyseModeValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsAnalyseMode = setting.IsChecked.Value;
    }

    protected void ShowingLineValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      IsShowingLine = setting.IsChecked.Value;
    }

    protected void OpponentValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      Opponent = setting.Text;
    }

    protected void LogLevelValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      LogLevel = (Level)Enum.Parse(typeof(Level), setting.Text);
    }

    protected void LogPathValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      LogPath = setting.Text;
    }

    protected void LanguageValue_PropertyChanged(Object sender, PropertyChangedEventArgs e) {
      var setting = (Setting)sender;
      SetLanguage(setting.Text);
    }
    #endregion

    #region Event Handler Subscriptions
    protected static Control findControl(String sName) {
      return Control.FindControl(Controls, sName);
    }

    private void wireClearHash() {
      var button = (Button)findControl("Clear Hash");
      if (button is not null)
        button.Click += ClearHashButton_Click;
    }

    private void wireMultiPV() {
      var setting = (Setting)findControl("MultiPV");
      if (setting is not null) {
        // Step 6/6: Dynamically subscribe handler to the event
        setting.PropertyChanged += MultiPVValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireQXPBuckets() {
      newQXPTank();

      var setting = (Setting)findControl("QXPBuckets");
      if (setting is not null) {
        setting.PropertyChanged += QXPBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireQXP() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireQXPBuckets();

      var setting = (Setting)findControl("QXPLength");
      if (setting is not null) {
        setting.PropertyChanged += QXPLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireXPBuckets() {
      newXPTank();

      var setting = (Setting)findControl("XPBuckets");
      if (setting is not null) {
        setting.PropertyChanged += XPBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireXP() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireXPBuckets();

      var setting = (Setting)findControl("XPLength");
      if (setting is not null) {
        setting.PropertyChanged += XPLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireXPMBuckets() {
      newXPMTank();

      var setting = (Setting)findControl("XPMBuckets");
      if (setting is not null) {
        setting.PropertyChanged += XPMBucketsValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireXPM() {
      //
      //[Note]Establish BucketsDefault before calling Tank.init()
      //
      wireXPMBuckets();

      var setting = (Setting)findControl("XPMLength");
      if (setting is not null) {
        setting.PropertyChanged += XPMLengthValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireExpectedMovesToGo() {
      var setting = (Setting)findControl("ExpectedMoves");
      if (setting is not null) {
        setting.PropertyChanged += ExpectedMovesToGoValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireContempt() {
      var setting = (Setting)findControl("Contempt");
      if (setting is not null) {
        setting.PropertyChanged += ContemptValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLate() {
      var setting = (Setting)findControl("Late");
      if (setting is not null) {
        setting.PropertyChanged += LateValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireChecks() {
      var setting = (Setting)findControl("Checks");
      if (setting is not null) {
        setting.PropertyChanged += ChecksValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireThreat() {
      var setting = (Setting)findControl("Threat");
      if (setting is not null) {
        setting.PropertyChanged += ThreatValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireSingular() {
      var setting = (Setting)findControl("Singular");
      if (setting is not null) {
        setting.PropertyChanged += SingularValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireAspiration() {
      var setting = (Setting)findControl("Aspiration");
      if (setting is not null) {
        setting.PropertyChanged += AspirationValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireFlip() {
      var setting = (Setting)findControl("Flip");
      if (setting is not null) {
        setting.PropertyChanged += FlipValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireFutility() {
      var setting = (Setting)findControl("Futility");
      if (setting is not null) {
        setting.PropertyChanged += FutilityValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireNullPrune() {
      var setting = (Setting)findControl("NullMove");
      if (setting is not null) {
        setting.PropertyChanged += NullPruneValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireOccam() {
      var setting = (Setting)findControl("Occam");
      if (setting is not null) {
        setting.PropertyChanged += OccamValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wirePure() {
      var setting = (Setting)findControl("Pure");
      if (setting is not null) {
        setting.PropertyChanged += PureValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireHeartbeatMS() {
      var setting = (Setting)findControl("HeartbeatMS");
      if (setting is not null) {
        setting.PropertyChanged += HeartbeatMSValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireHeartbeat() {
      var setting = (Setting)findControl("Heartbeat");
      if (setting is not null) {
        setting.PropertyChanged += HeartbeatValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wirePonder() {
      var setting = (Setting)findControl("Ponder");
      if (setting is not null) {
        setting.PropertyChanged += PonderValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireAnalyseMode() {
      var setting = (Setting)findControl("UCI_AnalyseMode");
      if (setting is not null) {
        setting.PropertyChanged += AnalyseModeValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireShowingLine() {
      var setting = (Setting)findControl("UCI_ShowCurrLine");
      if (setting is not null) {
        setting.PropertyChanged += ShowingLineValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireOpponent() {
      var setting = (Setting)findControl("UCI_Opponent");
      if (setting is not null) {
        setting.PropertyChanged += OpponentValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLogLevel() {
      var setting = (Setting)findControl("LogLevel");
      if (setting is not null) {
        setting.PropertyChanged += LogLevelValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLogPath() {
      var setting = (Setting)findControl("LogPath");
      if (setting is not null) {
        setting.PropertyChanged += LogPathValue_PropertyChanged;
        setting.SetDefault();
      }
    }

    private void wireLanguage() {
      var setting = (Setting)findControl("Language");
      if (setting is not null) {
        setting.PropertyChanged += LanguageValue_PropertyChanged;
        setting.SetDefault();
      }
    }
    #endregion
  }
}
