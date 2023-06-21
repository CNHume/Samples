//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2023-06-20 CNHume]Created File
//
// Conditionals:
//
//#define ThreadSafeTank

namespace Engine;

using Command;
using Command.Events;

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
#if !ThreadSafeTank
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");
#endif
    var button = (Button?)sender;
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
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      QXPTank.Init(Convert.ToInt32(value));
  }

  protected void QXPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting?)sender;
    //[Note]BucketsDefault must be updated before Tank.Init() is called
    var value = setting?.GetValue();
    if (value != null)
      QXPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      XPTank.Init(Convert.ToInt32(value));
  }

  protected void XPBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    //[Note]BucketsDefault must be set before Tank.Init() is called
    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      XPTank.BucketsDefault = Convert.ToUInt16(value);
  }

  protected void XPMLengthValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting?)sender;
    var value = setting?.GetValue();
    if (value != null)
      XPMTank.Init(Convert.ToInt32(value));
  }

  protected void XPMBucketsValue_PropertyChanged(Object? sender, PropertyChangedEventArgs e) {
    if (IsSearchInProgress)
      throw new PositionException("Search in progress");

    var setting = (SpinSetting?)sender;
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
}
