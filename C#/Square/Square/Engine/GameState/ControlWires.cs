//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2023-06-20 CNHume]Created File
//

using System.Diagnostics.CodeAnalysis;

namespace Engine;

using Command;

using static Command.Control;
using static Command.Control.ControlName;

partial class GameState {
  #region Methods
  protected static Control? findControl(ControlName optionName) {
    return FindControl(Controls, optionName);
  }

  #region Event Handler Subscriptions
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
      // Step 6/6: Subscribe to Event Handler in Wireup method
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
  #endregion                            // Methods
}
