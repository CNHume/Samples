//
// Copyright (C) 2010-2026, Christopher N. Hume.  All rights reserved.
//
//[2023-06-20 CNHume]Created File
//
// Conditionals:
//
//#define ThreadSafeTank

namespace Engine;

using Commands;
using Commands.Events;

using Exceptions;

using static Board;
using static Logging.Logger;
using static Position;

//
// Type Aliases:
//
using Eval = Int16;

partial class GameState {
  #region Event Handlers
  protected void ClearHashButton_Click(Object? sender, EventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
#if !ThreadSafeTank
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");
#endif
    //var button = (Button)sender;
    //var value = (Boolean?)button.GetValue();
    XPTank.Clear(0, XPTank.LookupBuckets);
    QXPTank.Clear(0, QXPTank.LookupBuckets);
  }

  // Step 5/6: Define an Event Handler, following the standard naming convention
  protected void MultiPVValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    newVariations(Convert.ToInt32(value));
  }

  protected void QXPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    QXPTank.Init(Convert.ToInt32(value));
  }

  protected void QXPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting)sender;
    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting.GetValue();
    QXPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    XPTank.Init(Convert.ToInt32(value));
  }

  protected void XPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    //[Note]BucketsDefault must be set before Tank.Init() is called
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    XPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPMLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    XPMTank.Init(Convert.ToInt32(value));
  }

  protected void XPMBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting)sender;
    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting?.GetValue();
    XPMTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void ExpectedMovesToGoValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    ExpectedMovesToGo = Convert.ToUInt16(value);
  }

  protected void ContemptValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    ContemptValue = (Eval)Convert.ToInt16(value);
  }

  protected void LateValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    SetNibble(ref ExtensionLimit, vLate, Convert.ToUInt16(value));
  }

  protected void ChecksValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    SetNibble(ref ExtensionLimit, vCheck, Convert.ToUInt16(value));
  }

  protected void ThreatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    SetNibble(ref ExtensionLimit, vThreat, Convert.ToUInt16(value));
  }

  protected void SingularValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    SetNibble(ref ExtensionLimit, vSingular, Convert.ToUInt16(value));
  }

  protected void AspirationValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsAspiration = Convert.ToBoolean(value);
  }

  protected void FlipValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsFlip = Convert.ToBoolean(value);
  }

  protected void FutilityValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsFutility = Convert.ToBoolean(value);
  }

  protected void NullPruneValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsNullPrune = Convert.ToBoolean(value);
  }

  protected void OccamValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsOccam = Convert.ToBoolean(value);
  }

  protected void PureValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsPure = Convert.ToBoolean(value);
  }

  protected void HeartbeatValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsDisplayHeartbeat = Convert.ToBoolean(value);
  }

  protected void HeartbeatMSValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (SpinSetting)sender;
    var value = setting.GetValue();
    if (value != null) {
      var uMilliseconds = Convert.ToUInt16(value);
      var tsHeartbeat = TimeSpan.FromMilliseconds(uMilliseconds);
      HeartbeatTicks = tsHeartbeat.Ticks;
    }
  }

  protected void PonderValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsPonderEnabled = Convert.ToBoolean(value);
  }

  protected void AnalyseModeValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsAnalyseMode = Convert.ToBoolean(value);
  }

  protected void DisplayCurrentLineValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (CheckSetting)sender;
    var value = setting.GetValue();
    IsDisplayCurrentLine = Convert.ToBoolean(value);
  }

  protected void OpponentValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (StringSetting)sender;
    Opponent = setting.GetValue();
  }

  protected void LogLevelValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (ComboSetting)sender;
    var value = setting.GetValue();
    if (value != null)
      Level = (LogLevel)Enum.Parse(typeof(LogLevel), value);
  }

  protected void LogPathValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (StringSetting)sender;
    Path = setting.GetValue();
  }

  protected void LanguageValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    ArgumentNullException.ThrowIfNull(sender, nameof(sender));
    var setting = (ComboSetting)sender;
    var value = setting.GetValue();
    SetLanguage(value);
  }
  #endregion                            // Event Handlers
}
