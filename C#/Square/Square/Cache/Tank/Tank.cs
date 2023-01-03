//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2014-07-16 CNHume]Converted to a Generic Class
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define LinkedTranspositions
//#define LotsForLittle                   // Allocates Lots of Little Buckets for Locality of Reference; but this was ~5% slower
//#define ThreadSafeTank                  // Allow multi-threaded Tank access (performance cost is ~3%)
#define TankInit
#define RenewLXPMInOrder
//#define HalfHash                        // Avoiding 64-Bit Division of no benefit on a 3 GHz Pentium 4
//#define HashPowerOfTwo
//#define PreAllocated
//#define TankRecycle
//#define TestHashMath

namespace Cache {
  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Runtime.CompilerServices;

  using Engine;

  using static Engine.Board;            // For OneBitOrNone()

  //
  // Type Aliases:
  //
  using Hashcode = UInt64;

  partial class Tank<T> where T : ITankable<T>, new()
#if PreAllocated
    , new()
#endif
    {
    #region Constants
    const UInt64 BIT0 = 1UL;
    #endregion

    #region Properties
#if ThreadSafeTank                      // One lock for each instance of Tank
    private readonly Object bucketLock = new object();
#endif
    public ProbeCounter Counts { get; set; }
    public UInt16 BucketsDefault { get; set; }
    public UInt16 LookupBuckets { get; set; }

    private UInt32 _LookupLength;
    public UInt32 LookupLength {
      get { return _LookupLength; }

      protected set {
        _LookupLength = value;

        //
        //[Test]The following is only used by indexTest()
        //
        // Given a common modulus: if A1 is congruent to A2; and B1 is congruent to B2
        // then A1*B1 will be congruent to A2*B2
        //
        LengthRem = (UInt32)(BIT32 % LookupLength);
      }
    }

    public UInt32 LengthRem { get; protected set; }
#if LinkedTranspositions
    public LinkedList<T>[] Entries;
#else
    protected T[][] Entries;
#endif
    #endregion

    #region Constructor
    public Tank(String Name) {
      Counts = new ProbeCounter(Name);
    }
    #endregion

    #region Allocation
    private void allocate(UInt32 uLength, UInt16 wBuckets) {
#if HashPowerOfTwo
      if (!OneBitOrNone(uLength))
        throw new ApplicationException("LookupLength must be a power of two");
#endif
      if (Entries == null ||
#if LotsForLittle
          uLength > Entries.Length ||
          wBuckets > Entries[0].Length)
#else
          wBuckets > Entries.Length ||
          uLength > Entries[0].Length)
#endif
        allocateNew(uLength, wBuckets);
      else if (LookupBuckets < wBuckets)
        Clear(LookupBuckets, wBuckets);

      LookupLength = uLength;
      LookupBuckets = wBuckets;
    }

    private void allocateNew(UInt32 uLength, UInt16 wBuckets) {
#if LinkedTranspositions
      Buckets = new LinkedList<T>[uLength];
#else
#if LotsForLittle
      Entries = new T[uLength][];
      for (var uIndex = 0U; uIndex < uLength; uIndex++) {
        Entries[uIndex] = new T[wBuckets];
        if (!typeof(T).IsValueType) {
#if TankInit
          for (var uIndex = 0U; uIndex < uLength; uIndex++)
            Entries[nBucket][uIndex].Init();
#endif
        }
        else {
#if PreAllocated
          for (var nBucket = 0; nBucket < wBuckets; nBucket++)
            Entries[uIndex][nBucket] = new T();
#endif
        }
      }
#else
      Entries = new T[wBuckets][];
      for (var nBucket = 0; nBucket < wBuckets; nBucket++) {
        Entries[nBucket] = new T[uLength];
        if (!typeof(T).IsValueType) {
#if TankInit
          for (var uIndex = 0U; uIndex < uLength; uIndex++)
            Entries[nBucket][uIndex].Init();
#endif
        }
        else {
#if PreAllocated
          for (var uIndex = 0U; uIndex < uLength; uIndex++)
            Entries[nBucket][uIndex] = new T();
#endif
        }
      }
#endif                                  // LotsForLittle
#endif                                  // LinkedTranspositions
    }
    #endregion

    #region Static Methods
    // Used by findMatch() to expedite search where many Buckets are empty.
    // Relies on Init() to establish IsEmpty, since this is based on a non-default value.
    private static Boolean isNullOrEmpty(T xp) {
      return xp == null || xp.IsEmpty;
    }
    #endregion

    #region Hash Methods
    private UInt32 indexTest(Hashcode qHash) {
      var qHi = ((qHash >> 32) * LengthRem) % LookupLength;
      var uLo = (UInt32)qHash % LookupLength;
      return (UInt32)(qHi + uLo) % LookupLength;
    }

    private UInt32 index2(Hashcode qHash) {
      var uHi = (UInt32)(qHash >> 32);  // Avoiding 64-Bit Division
      var uLo = (UInt32)qHash;
      return (uHi ^ uLo) % LookupLength;
    }
#if HashPowerOfTwo
    private UInt32 index(Hashcode qHash) {
      return (UInt32)(qHash & LookupLength - 1);
    }
#else
    private UInt32 index(Hashcode qHash) {
      return (UInt32)(qHash % LookupLength);
    }
#endif
    #endregion

    #region Initializers
    public void Init(Int32 nSelection) {
      //
      // LookupLength selections are expressed in "binary million" rather
      // than decimal million entries: 1M == 1 << 20 == 1,048,576 entries.
      //
      var uLength = (UInt32)nSelection << 20;
      allocate(uLength, BucketsDefault);
    }

    public void Clear(Int32 nFromPos, Int32 nToPos) {
      //
      //[Note]The full Entries dimension should be cleared,
      // because LookupLength may be increased later.
      //
      //[Assume]Entries are by Value.  The current implementation
      // is based on Array.Clear().  A recycling strategy will be
      // needed otherwise: to reduce GC overhead.
      //
#if ThreadSafeTank
      lock (bucketLock) {               //[Safe]In case a search may be in progress
#else
      {
#endif
#if LotsForLittle
        Debug.Assert(nToPos <= Entries[0].Length, "nToPos > Entries[0].Length");
        var nLength = nToPos - nFromPos;
        for (var uIndex = 0U; uIndex < Entries.Length; uIndex++)
          Array.Clear(Entries[uIndex], nFromPos, nLength);
#else
        Debug.Assert(nToPos <= Entries.Length, "nToPos > Entries.Length");
        var nLength = Entries[0].Length;
        for (var nBucket = nFromPos; nBucket < nToPos; nBucket++)
          Array.Clear(Entries[nBucket], 0, nLength);
#endif
      }
    }
    #endregion

    #region Table Accessors
    //
    // Replacement Strategy:
    //
    // 1. Prevent duplicate positions, favoring position of greater depth.
    //    This is the only case where the new position might not be saved.
    //      However, improvement to an Upper or Lower bound should be saved.
    // 2. Append each new element until all buckets are full.
    // 3. Otherwise, remove the Least Recently Used [LRU] initial element.
    // 4. Renew matched elements by moving them to the final bucket.
    //
    public void Save(T store) {
      Counts.SetReads++;
      //[Test]return;
      var bReplace = true;              // Assume final write needed
#if ThreadSafeTank
      lock (bucketLock) {
#else
      {
#endif
        if (!findMatch(out T found, out UInt32 uIndex, out Int32 nBucket, store)) {
          found = store;                // Non-Match Case: Add new Entry
          Counts.Added++;

          if (nBucket < LookupBuckets) {// Bucket Not Full: Add newest and return
#if LotsForLittle
            Entries[uIndex][nBucket] = found;
#else
            Entries[nBucket][uIndex] = found;
#endif
            return;
          }
          else
            nBucket = 0;                // Bucket Full: Remove Oldest Entry
        }                               // Non-Match Case
        else if (found.IsNew(store)) {  // Unsatisfactory Match: Replace
#if TankRecycle
          found.Recycle(store);
          bReplace = false;
#else
          found = store;
#endif
        }                               // Unsatisfactory Match
        else {                          // Satisfactory Match, a.k.a. Set Hit
          bReplace = false;             // found only written if it moves
          Counts.SetHits++;
        }

        renew(found, uIndex, nBucket, bReplace);
      }
    }

    public Boolean LoadFirst(ref T match) {
      var bValid = false;
      Counts.GetReads++;

#if ThreadSafeTank
      lock (bucketLock) {
#else
      {
#endif
        if (findMatch(out T found, out UInt32 uIndex, out Int32 nBucket, match)) {
          var pr = found.Result(ref match);
          if (bValid = pr.Has(ProbeResult.Valid))
            Counts.GetHits++;

          // Renew Useful Entries:
          if (pr.Has(ProbeResult.Renew))
            renew(found, uIndex, nBucket);
        }

        Counts.GetReads++;
      }

      return bValid;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Boolean findMatch(out T found, out UInt32 uIndex, out Int32 nBucket, T match) {
      var bFound = false;
      found = new T();
      var qHash = match.Hash;
#if HalfHash
      uIndex = index2(qHash);
#else
      uIndex = index(qHash);
#if TestHashMath
      Debug.Assert(indexTest(qHash) == uIndex, "indexTest() != index()");
#endif
#endif
      for (nBucket = 0; nBucket < LookupBuckets; nBucket++) {
#if LotsForLittle
        found = Entries[uIndex][nBucket];
#else
        found = Entries[nBucket][uIndex];
#endif
        if (isNullOrEmpty(found))
          break;
        if (match.Match(found)) {
          bFound = true;
          break;
        }
      }

      return bFound;
    }

    public Boolean Load(T match, List<T> matches) {
      var bValid = false;
      Counts.GetReads++;

      UInt32 uIndex;
      Int32 nBucket = 0;
#if ThreadSafeTank
      lock (bucketLock) {
#else
      {
#endif
        while (findNextMatch(out T found, out uIndex, ref nBucket, match)) {
          var pr = found.Result(ref match);
          if (bValid |= (pr & ProbeResult.Valid) != 0)
            Counts.GetHits++;

          // Renew useful Entries below
          if (pr.Has(ProbeResult.Renew))
            matches.Add(found);

          Counts.GetReads++;
        }
#if RenewLXPMInOrder
        matches.Reverse();
#endif
        nBucket = 0;
        foreach (var found2 in matches)
          renew(found2, uIndex, nBucket);
#if RenewLXPMInOrder
        matches.Reverse();
#endif
      }

      return bValid;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Boolean findNextMatch(out T found, out UInt32 uIndex, ref Int32 nBucket, T match) {
      var bFound = false;
      found = new T();
      var qHash = match.Hash;
#if HalfHash
      uIndex = index2(qHash);
#else
      uIndex = index(qHash);
#if TestHashMath
      Debug.Assert(indexTest(qHash) == uIndex, "indexTest() != index()");
#endif
#endif
      for (; nBucket < LookupBuckets; nBucket++) {
#if LotsForLittle
        found = Entries[uIndex][nBucket];
#else
        found = Entries[nBucket][uIndex];
#endif
        if (isNullOrEmpty(found))
          break;
        if (match.Match(found)) {
          bFound = true;
          nBucket++;
          break;
        }
      }

      return bFound;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private void renew(T found, UInt32 uIndex, Int32 nBucket, Boolean bReplace = false) {
      for (; nBucket + 1 < LookupBuckets; nBucket++) {
#if LotsForLittle
        var xp = Entries[uIndex][nBucket + 1];
#else
        var xp = Entries[nBucket + 1][uIndex];
#endif
        if (isNullOrEmpty(xp))
          break;
#if LotsForLittle
        Entries[uIndex][nBucket] = xp;
#else
        Entries[nBucket][uIndex] = xp;
#endif
        bReplace = true;
      }

      if (bReplace) {                   // No replace needed if found was already renewed
#if LotsForLittle
        Entries[uIndex][nBucket] = found;
#else
        Entries[nBucket][uIndex] = found;
#endif
      }
    }
    #endregion
  }
}
