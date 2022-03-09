//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[2014-10-02 CNHume]Created File
//
// Conditionals:
//
#define TankInit
//#define TankRecycle

namespace Cache {
  using System;

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;

  #region Enumerations
  [Flags]
  public enum ProbeResult : byte { None, Match, Value, Renew, Valid };
  #endregion

  #region Interfaces
  public interface ITankable<T> {
    Hashcode Hash { get; set; }
    UInt64 Data { get; set; }
    Boolean IsEmpty { get; }
    Boolean IsNew(T store);
    ProbeResult Result(ref T match);
#if TankInit
    void Init();
#endif
    Boolean Match(T entry);
#if TankRecycle
    void Recycle(T store);
#endif
  }
  #endregion
}
