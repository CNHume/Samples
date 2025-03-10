﻿//
// Copyright (C) 2010-2025, Christopher N. Hume.  All rights reserved.
//
//[CNH 27-Nov-12]Created Memo Class to implement fast Hashtables
//

namespace Cache;

using static Engine.Board;              // For OneBitOrNone()

using Hashcode = UInt64;
using MemoHashcode = UInt32;
using PieceHashcode = UInt16;           // 10 bits

class Memo<T> {
  #region Properties
  public T[]? Entries { get; set; }
  public UInt32 LookupLength { get; set; }
  public SimpleCounter Counts { get; set; }
  #endregion

  #region Indexers
  public T this[MemoHashcode uHash] {
    get {
      ArgumentNullException.ThrowIfNull(Entries);
      return Entries[Index(uHash)];
    }

    set {
      ArgumentNullException.ThrowIfNull(Entries);
      Entries[Index(uHash)] = value;
    }
  }
  #endregion                            // Indexers

  #region Constructors
  public Memo(String Name, UInt32 uLength) {
    Counts = new(Name);
    Allocate(uLength);
  }
  #endregion                            // Constructors

  #region Methods
  protected void Allocate(UInt32 uLength) {
    if (Entries == null || uLength > Entries.Length)
      AllocateNew(uLength);
    else if (LookupLength < Entries.Length)
      Clear(LookupLength);
  }

  protected virtual void AllocateNew(UInt32 uLength) {
    Entries = new T[uLength];
    LookupLength = uLength;
  }

  public void Clear(UInt32 uLength) {
    if (Entries != null)
      Array.Clear(Entries, 0, (Int32)uLength);
  }

  protected virtual UInt32 Index(MemoHashcode qHash) {
    var u = qHash % LookupLength;
    return u;
  }
  #endregion                            // Methods
}

class Memo2<T> : Memo<T> {
  #region Indexers
  public T this[PieceHashcode wHash] {// For GetCX2()
    get {
      ArgumentNullException.ThrowIfNull(Entries);
      return Entries[index(wHash)];
    }

    set {
      ArgumentNullException.ThrowIfNull(Entries);
      Entries[index(wHash)] = value;
    }
  }

  public T this[Hashcode qHash] {
    get {
      ArgumentNullException.ThrowIfNull(Entries);
      return Entries[index(qHash)];
    }

    set {
      ArgumentNullException.ThrowIfNull(Entries);
      Entries[index(qHash)] = value;
    }
  }
  #endregion                            // Indexers

  #region Constructors
  public Memo2(String Name, UInt32 uLength) : base(Name, uLength) {
  }

  protected override void AllocateNew(UInt32 uLength) {
    if (!IsOneOrLess(uLength))
      throw new ApplicationException("Memo2 Length must be a power of two");

    base.AllocateNew(uLength);
  }
  #endregion                            // Constructors

  #region Methods
  private UInt32 index(PieceHashcode wHash) {
    var u = wHash & LookupLength - 1;
    return u;
  }

  protected override UInt32 Index(MemoHashcode uHash) {
    var u = uHash & LookupLength - 1;
    return u;
  }

  private UInt32 index(Hashcode qHash) {
    var q = qHash & LookupLength - 1;
    return (UInt32)q;
  }
  #endregion                            // Methods
}
