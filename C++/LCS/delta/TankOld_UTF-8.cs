//
// (C) Copyright 2014, Christopher N. Hume.  All rights reserved.
//
//[2014-07-16 CNHume]Converted to a Generic Class
//[2011-06-09 CNHume]Created File
//
// Conditionals:
//
//#define LotsForLittle			// Allocates Lots of Little Groups for Locality of Reference, but this seems slower
#define TranspositionByValue
//#define PreAllocated
//#define TestHashMath
//#define HalfHash			// Avoiding 64-Bit Division of no benefit on a 3 GHz Pentium 4
//#define HashPowerOfTwo

namespace Engine.Play {
  using Logger;
  using Mask;
  using Value;

  using System;
  using System.Diagnostics;
  //using System.Linq;
  //using System.Text;

  //
  // Type Aliases:
  //
  using Depth = System.Int16;
  using Eval = System.Int16;
  using Hashcode = System.UInt64;

  #region Interfaces
  public interface ITankable<T> {
    EvalType EvalType { get; set; }
    Hashcode Hashcode { get; }
    Boolean IsNew(T store);
    ProbeResult result(ref T match);
  }
  #endregion

  #region Enumerations
  public enum ProbeResult : byte { Undefined, Match, Move, Value, Valid };
  #endregion

  partial class Tank<T> where T : ITankable<T> {
    #region Properties
    public UInt16 GroupLengthValue { get; set; }
    public UInt16 GroupLookupLength { get; set; }

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
        LengthRem = (UInt32)((Board.BIT0 << 32) % LookupLength);
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
    public Tank() { }
    #endregion
 
    #region Allocation
    protected void allocate(UInt32 uGroups, UInt16 wGroupSize) {
      if (Entries == null ||
#if LotsForLittle
	  uGroups > Entries.Length ||
	  wGroupSize > Entries[0].Length)
#else
          wGroupSize > Entries.Length ||
          uGroups > Entries[0].Length)
#endif
        allocate2(uGroups, wGroupSize);
      else if (GroupLookupLength < wGroupSize)
        clear(GroupLookupLength, wGroupSize);

      LookupLength = uGroups;
      GroupLookupLength = wGroupSize;
    }

    protected void allocate2(UInt32 uGroups,
                             UInt16 wGroupSize) {
#if HashPowerOfTwo
      var bPowerOfTwo = (uGroups - 1 & uGroups) == 0;
      if (!bPowerOfTwo)
	throw new PositionException("Groups Length must be a power of two");
#endif
#if LinkedTranspositions
      Groups = new LinkedList<T>[uGroups];
#else
#if LotsForLittle
      Entries = new T[uGroups][];
      for (var uIndex = 0U; uIndex < uGroups; uIndex++) {
	Entries[uIndex] = new T[wGroupSize];
#if PreAllocated && !TranspositionByValue
	for (var nPos = 0; nPos < wGroupSize; nPos++)
	  Entries[uIndex][nPos] = new T();
#endif
      }
#else
      Entries = new T[wGroupSize][];
      for (var nPos = 0; nPos < wGroupSize; nPos++) {
        Entries[nPos] = new T[uGroups];
#if PreAllocated && !TranspositionByValue
	for (var uIndex = 0U; uIndex < uGroups; uIndex++)
	  Entries[nPos][uIndex] = new T();
#endif
      }
#endif
#endif
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
      // Units for the Hash and HashQuiet selections are in binary millions of hash table entries,
      // rather than decimal MB, where a setting of one million == 1 << 20 == 1,048,576 entries.
      //
      var uGroups = (UInt32)nSelection << 20;
      allocate(uGroups, GroupLengthValue);
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
#if LotsForLittle
      Debug.Assert(nToPos < Entries[0].Length,
		   "nToPos >= Entries[0].Length");
      var nLength = nToPos - nFromPos;
      for (var uIndex = 0U; uIndex < Entries.Length; uIndex++)
	Array.Clear(Entries[uIndex], nFromPos, nLength);
#else
      Debug.Assert(nToPos <= Entries.Length,
                   "nToPos >= Entries.Length");
      var nLength = Entries[0].Length;
      for (var nPos = nFromPos; nPos < nToPos; nPos++)
        Array.Clear(Entries[nPos], 0, nLength);
#endif
    }
    #endregion

    #region Table Accessors
    //
    // Replacement Strategy:
    //
    // 1. Prevent duplicate positions, favoring greater depth if position match.
    //    This is the only case where the new position might not be saved.
    //	  However, improvement to an Upper or Lower bound should be saved.
    // 2. Insert if group is not full, appending this most recent element.
    // 3. Replace Least Recently Used [LRU], i.e., first element of group.
    // 4. Matches update recency of use by moving element to end of group.
    //
    public Boolean store(T store) {
      var bHit = false;
      var qHashcode = store.Hashcode;
      //[Test]return;
#if HalfHash
      var uIndex = index2(qHashcode);
#else
      var uIndex = index(qHashcode);
#if TestHashMath
      Debug.Assert(indexTest(qDynamic) == uIndex, "indexTest() != index()");
#endif
#endif
      var found = default(T);
      var nPos = 0;
      for (; nPos < GroupLookupLength; nPos++) {
#if LotsForLittle
	var xp = Entries[uIndex][nPos];
#else
        var xp = Entries[nPos][uIndex];
#endif
#if TranspositionByValue || PreAllocated
        var bDefault = xp.EvalType == EvalType.Undefined;
#else
	var bDefault = xp == default(T);
#endif
        if (bDefault)
          break;

        if (xp.Hashcode == qHashcode) {
          found = xp;
          break;
        }
      }					// Group Search

      var bNew = true;			// Assume final write, i.e., no match nor recycle()
#if TranspositionByValue
      if (found.EvalType == EvalType.Undefined) {
#else
      if (found == null) {
#endif
        found = store;	                // Non-Match Case: Add new Entry
        //[ToDo]XPUsed++;

        if (GroupLookupLength <= nPos)
          nPos = 0;			// Group Full: Recycle Oldest Entry
        else {				// Group Not Full: Add newest and return here
#if LotsForLittle
	  Entries[uIndex][nPos] = found;
#else
          Entries[nPos][uIndex] = found;
#endif
          return bHit;
        }
      }					// Non-Match
      else {				// Match Cases:
        if (found.IsNew(store)) {	// Unsatisfactory Match: Recycle
#if TranspositionByValue
          found = store;
#else
	  bNew = false;
	  found.recycle(store);
#endif
        }				// Unsatisfactory Match
        else {				// Satisfactory Match, a.k.a. Set Hit
          bHit = true;
          bNew = false;
        }
      }

      // Rejuvenate Matched Entry:
      for (; nPos + 1 < GroupLookupLength; nPos++) {
#if LotsForLittle
	var xp = Entries[uIndex][nPos + 1];
#else
        var xp = Entries[nPos + 1][uIndex];
#endif
#if TranspositionByValue || PreAllocated
        if (xp.EvalType == EvalType.Undefined)
          break;
#else
	if (xp == default(T))
	  break;
#endif
        Debug.Assert(xp.Hashcode != qHashcode, "set() duplicate");
#if LotsForLittle
	Entries[uIndex][nPos] = xp;
#else
        Entries[nPos][uIndex] = xp;
#endif
        bNew = true;
      }					// Rejuvenate

      if (bNew) {
#if LotsForLittle
	Entries[uIndex][nPos] = found;
#else
        Entries[nPos][uIndex] = found;
#endif
      }

      return bHit;
    }

    public ProbeResult get(ref T match) {
      var pr = ProbeResult.Undefined;
      var qHashcode = match.Hashcode;
#if HalfHash
      var uIndex = index2(qHashcode);
#else
      var uIndex = index(qHashcode);
#if TestHashMath
      Debug.Assert(indexTest(qDynamic) == uIndex, "indexTest() != index()");
#endif
#endif
      var found = default(T);
      var nPos = 0;
      for (; nPos < GroupLookupLength; nPos++) {
#if LotsForLittle
	var xp = Entries[uIndex][nPos];
#else
        var xp = Entries[nPos][uIndex];
#endif
#if TranspositionByValue || PreAllocated
        var bDefault = xp.EvalType == EvalType.Undefined;
#else
	var bDefault = xp == default(T);
#endif
        if (bDefault)
          break;

        if (xp.Hashcode == qHashcode) {
          found = xp;                   // For Rejuvenation
          pr = found.result(ref match);
          break;
        }
      }					// Group Search

      if (pr >= ProbeResult.Move) {     //[Temp]
        // Rejuvenate Useful Entries:
        var bNew = false;
        for (; nPos + 1 < GroupLookupLength; nPos++) {
#if LotsForLittle
	  var xp = Game.Entries[uIndex][nPos + 1];
#else
          var xp = Entries[nPos + 1][uIndex];
#endif
#if TranspositionByValue || PreAllocated
          if (xp.EvalType == EvalType.Undefined)
            break;
#else
	  if (xp == default(T))
	    break;
#endif
          Debug.Assert(xp.Hashcode != qHashcode, "get() duplicate");
#if LotsForLittle
	  Entries[uIndex][nPos] = xp;
#else
          Entries[nPos][uIndex] = xp;
#endif
          bNew = true;
        }				// Rejuvenate

        if (bNew) {
#if LotsForLittle
	  Entries[uIndex][nPos] = found;
#else
          Entries[nPos][uIndex] = found;
#endif
        }
      }

      return pr;
    }
    #endregion
  }
}
