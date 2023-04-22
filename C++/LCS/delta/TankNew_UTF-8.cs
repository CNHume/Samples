//
// (C) Copyright 2014, Christopher N. Hume.  All rights reserved.   
//
//[2014-07-16 CNHume]Converted to a Generic Class
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define LotsForLittle			// Allocates Lots of Little Buckets for Locality of Reference; but this was ~5% slower
//#define ThreadSafeTank
#define TranspositionByValue
//#define PreAllocated
//#define TestHashMath
//#define HalfHash			// Avoiding 64-Bit Division of no benefit on a 3 GHz Pentium 4
//#define HashPowerOfTwo

namespace Cache {
  using Logger;

  using System;
  using System.Collections.Generic;
  using System.Diagnostics;
  //using System.Linq;
  using System.Runtime.CompilerServices;

  //
  // Type Aliases:
  //
  using Hashcode = System.UInt64;

  partial class Tank<T> where T : ITankable<T> {
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
        LengthRem = (UInt32)((BIT0 << 32) % LookupLength);
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
    protected void allocate(UInt32 uLength, UInt16 wBuckets) {
#if HashPowerOfTwo
      var bPowerOfTwo = (uLength - 1 & uLength) == 0;
      if (!bPowerOfTwo)
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
        clear(LookupBuckets, wBuckets);

      LookupLength = uLength;
      LookupBuckets = wBuckets;
    }

    protected void allocateNew(UInt32 uLength, UInt16 wBuckets) {
#if LinkedTranspositions
      Buckets = new LinkedList<T>[uLength];
#else
#if LotsForLittle
      Entries = new T[uLength][];
      for (var uIndex = 0U; uIndex < uLength; uIndex++) {
	Entries[uIndex] = new T[wBuckets];
#if PreAllocated && !TranspositionByValue
	for (var nBucket = 0; nBucket < wBuckets; nBucket++)
	  Entries[uIndex][nBucket] = new T();
#endif
      }
#else
      Entries = new T[wBuckets][];
      for (var nBucket = 0; nBucket < wBuckets; nBucket++) {
        Entries[nBucket] = new T[uLength];
#if PreAllocated && !TranspositionByValue
	for (var uIndex = 0U; uIndex < uLength; uIndex++)
	  Entries[nBucket][uIndex] = new T();
#endif
      }
#endif
#endif
    }
    #endregion

    #region Static Methods
    protected static Boolean IsNullOrEmpty(T xp) {
      return xp == null || xp.IsEmpty;
    }
    #endregion

    #region Hash Methods
    protected UInt32 indexTest(Hashcode hash) {
      var qHi = ((hash >> 32) * LengthRem) % LookupLength;
      var uLo = (UInt32)hash % LookupLength;
      return (UInt32)(qHi + uLo) % LookupLength;
    }

    protected UInt32 index2(Hashcode hash) {
      var uHi = (UInt32)(hash >> 32);	// Avoiding 64-Bit Division
      var uLo = (UInt32)hash;
      return (uHi ^ uLo) % LookupLength;
    }
#if HashPowerOfTwo
    protected UInt32 index(Hashcode hash) {
      return (UInt32)(hash & LookupLength - 1);
    }
#else
    protected UInt32 index(Hashcode hash) {
      return (UInt32)(hash % LookupLength);
    }
#endif
    #endregion

    #region Initializers
    public void init(Int32 nSelection) {
      //
      // LookupLength selections are expressed in "binary million" rather
      // than decimal million entries: 1M == 1 << 20 == 1,048,576 entries.
      //
      var uLength = (UInt32)nSelection << 20;
      allocate(uLength, BucketsDefault);
    }

    public void clear(Int32 nFromPos, Int32 nToPos) {
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
        Debug.Assert(nToPos < Entries[0].Length, "nToPos >= Entries[0].Length");
        var nLength = nToPos - nFromPos;
        for (var uIndex = 0U; uIndex < Entries.Length; uIndex++)
	  Array.Clear(Entries[uIndex], nFromPos, nLength);
#else
        Debug.Assert(nToPos <= Entries.Length, "nToPos >= Entries.Length");
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
    //	  However, improvement to an Upper or Lower bound should be saved.
    // 2. Append each new element until all buckets are full.
    // 3. Otherwise, remove the Least Recently Used [LRU] initial element.
    // 4. Renew matched elements by moving them to the final bucket.
    //
    public void store(T store) {
      Counts.SetReads++;
      //[Test]return;
      T found;
      UInt32 uIndex;
      Int32 nBucket;
      var bReplace = false;		// Assume final write not needed
#if ThreadSafeTank
      lock (bucketLock) {               // Allow multi-thread access (performance cost is ~1.5%)
#else
      {
#endif
        if (!lookup(out found, out uIndex, out nBucket, store.Hashcode)) {
          bReplace = true;              // Non-Match Case: Add new Entry
          found = store;
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
            nBucket = 0;		// Bucket Full: Remove Oldest Entry
        }				// Non-Match Case
        else if (found.IsNew(store)) {	// Unsatisfactory Match: Recycle or Replace
#if TranspositionByValue
          bReplace = true;
          found = store;
#else
	found.recycle(store);
#endif
        }				  // Unsatisfactory Match
        else				  // Satisfactory Match, a.k.a. Set Hit
          Counts.SetHits++;               // store is not written in this case

        renew(found, uIndex, nBucket, bReplace);
      }
    }

    public Boolean get(ref T match) {
      var bValid = false;
      Counts.GetReads++;

      T found;
      UInt32 uIndex;
      Int32 nBucket;
#if ThreadSafeTank
      lock (bucketLock) {               // Allow multi-thread access (performance cost is ~1.5%)
#else
      {
#endif
        if (lookup(out found, out uIndex, out nBucket, match.Hashcode)) {
          var pr = found.result(ref match);
          if (bValid = (pr & ProbeResult.Valid) != 0)
            Counts.GetHits++;

          // Renew Useful Entries:
          if ((pr & ProbeResult.Renew) != 0)
            renew(found, uIndex, nBucket);
        }
      }

      return bValid;
    }

    [MethodImplAttribute(MethodImplOptions.AggressiveInlining)]
    private Boolean lookup(out T found, out UInt32 uIndex, out Int32 nBucket, Hashcode qHashcode) {
      var bFound = false;
      found = default(T);
#if HalfHash
      uIndex = index2(qHashcode);
#else
      uIndex = index(qHashcode);
#if TestHashMath
      Debug.Assert(indexTest(qHashcode) == uIndex, "indexTest() != index()");
#endif
#endif
      for (nBucket = 0; nBucket < LookupBuckets; nBucket++) {
#if LotsForLittle
	var xp = Entries[uIndex][nBucket];
#else
        var xp = Entries[nBucket][uIndex];
#endif
        if (IsNullOrEmpty(xp))
          break;

        if (xp.Hashcode == qHashcode) {
          found = xp;
          bFound = true;
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
        if (IsNullOrEmpty(xp))
          break;
#if LotsForLittle
	Entries[uIndex][nBucket] = xp;
#else
        Entries[nBucket][uIndex] = xp;
#endif
        bReplace = true;
      }

      if (bReplace) {
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
