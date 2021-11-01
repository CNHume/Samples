//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
//[CNH 27-Nov-12]Created Memo Class to implement fast Hashtables
//
namespace Cache {
  using System;
  using System.Diagnostics;

  using static Engine.Board;            // For OneBitOrNone()

  using Hashcode = System.UInt64;
  using MemoHashcode = System.UInt32;
  using PieceHashcode = System.UInt16;  // 10 bits

  class Memo<T> {
    #region Properties
    public T[] Entries { get; set; }
    public UInt32 LookupLength { get; set; }
    public SimpleCounter Counts { get; set; }
    #endregion

    #region Indexer
    public T this[MemoHashcode uHash] {
      get {
        return Entries[index(uHash)];
      }

      set {
        Entries[index(uHash)] = value;
      }
    }
    #endregion

    #region Constructor
    public Memo(String Name, UInt32 uLength) {
      Counts = new SimpleCounter(Name);
      allocate(uLength);
    }
    #endregion

    #region Methods
    protected void allocate(UInt32 uLength) {
      if (Entries is null || uLength > Entries.Length)
        allocateNew(uLength);
      else if (LookupLength < Entries.Length)
        Clear(LookupLength);
    }

    protected virtual void allocateNew(UInt32 uLength) {
      Entries = new T[uLength];
      LookupLength = uLength;
    }

    public void Clear(UInt32 uLength) {
      Array.Clear(Entries, 0, (Int32)uLength);
    }

    internal virtual UInt32 index(MemoHashcode qHash) {
      var u = qHash % LookupLength;
      return u;
    }
    #endregion
  }

  class Memo2<T> : Memo<T> {
    #region Indexers
    public T this[PieceHashcode wHash] {// for GetCX2()
      get {
        return Entries[index(wHash)];
      }

      set {
        Entries[index(wHash)] = value;
      }
    }

    public T this[Hashcode qHash] {
      get {
        return Entries[index(qHash)];
      }

      set {
        Entries[index(qHash)] = value;
      }
    }
    #endregion

    #region Constructor
    public Memo2(String Name, UInt32 uLength) : base(Name, uLength) {
    }

    protected override void allocateNew(uint uLength) {
      if (!IsOneOrNone(uLength))
        throw new ApplicationException("Memo2 Length must be a power of two");

      base.allocateNew(uLength);
    }
    #endregion

    #region Methods
    internal UInt32 index(PieceHashcode wHash) {
      var u = wHash & LookupLength - 1;
      return u;
    }

    internal override UInt32 index(MemoHashcode uHash) {
      var u = uHash & LookupLength - 1;
      return u;
    }

    internal UInt32 index(Hashcode qHash) {
      var q = qHash & LookupLength - 1;
      return (UInt32)q;
    }
    #endregion
  }
}
