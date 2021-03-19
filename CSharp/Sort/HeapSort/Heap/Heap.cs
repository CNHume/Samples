//
// Copyright (C) 2010-2021, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// 2017-03-19 CNHume  Added Intellisense
// 2017-03-18 CNHume  Renamed IsInverted to IsAscending; simplified control flow
// 2014-12-27 CNHume  Renamed Sift() and Add() methods to SiftDown() and SiftUp()
// 2012-03-31 CNHume  Created Heap Class to implement Priority Queues
//
// References:
//
// This object oriented implementation descends from an earlier C implementation
// written and refined over the course of 1988.  The approach was informed by an
// article on the Heap data structure written by Digital Equipment Corporation's
// John Sopka and appearing in the Newsletter of the Merrimack Valley Chapter of
// the ACM circa 1980.  C was not yet in wide use; and John's implementation was
// written in FORTRAN.
//
// Notes:
//
// A set of Entries and an Ordering Relation constitute a Heap when the Entries are in a
// Tree such that the Ordering Relation holds between each Entry and any of its Children.
// A Heap is a partially ordered Tree, where the relation does not hold between siblings.
//
// A Binary Tree is implemented over an array by adopting the convention that an Entry's
// Children are maintained at the sequential pair of Indicies formed by doubling the one-
// based Index of their Parent.  Only indicies less than the array Length are considered.
//
// Sample Usage:
//
//  var items = new[] { 101, 3, 44, 55, 8, 17, 6 };
//  var sorter = new Heap<Int32>(items);
//  sorter.Sort();
//
// Left to Right Unit Test:
//
//  var items = new[] { 101, 3, 44, 55, 8, 17, 6 };
//  var sorter = new Heap<Int32>(items, 0);
//  sorter.Invert();
//
//  for (var final = 0; final < items.Length; final++)
//    sorter.SiftUp(items[final]);
//
//  sorter.Sort();
//
// Conditionals
//
//#define ShowSort

namespace Sort {
  using SortTest;
  using SortTest.Exceptions;
  using SortTest.Extensions;

  using System;
  using System.Collections;        // For non-generic IEnumerable
  using System.Collections.Generic;

  public class Heap<T> : ICloneable, IEnumerable<T> where T : IComparable {
    #region Fields
    private T[] entries;
    protected Int32 counter;
    #endregion

    #region Properties
    public IMeter Meter { get; init; }

    /// <summary>Entries array</summary>
    public T[] Entries {
      get {
        return entries;
      }

      set {
        counter = 0;
        entries = value;
      }
    }

    /// <summary>Length of Entries array</summary>
    /// <value>Entries array Length</value>
    public Int32 Length {
      get {
        return entries is null ? 0 : entries.Length;
      }
    }

    /// <summary>Count of entries currently in use</summary>
    /// <remarks>Count assignment performs Heap operations appropriate to the change in value</remarks>
    /// <value># of entries currently in use</value>
    public Int32 Count {
      get {
        return counter;
      }

      set {
        if (value < 0 || value > Length)
          throw new IndexOutOfRangeException();

        if (value <= counter)
          counter = value;              // Truncate Heap
        else if (counter > 0) {
          while (counter < value)       // Add new Entries
            SiftUp(entries[counter]);
        }
        else {                          // counter == 0
          counter = value;              // Rebuild Heap
          Build();
        }
      }
    }

    /// <summary>Heap sense</summary>
    public Boolean IsAscending { get; protected set; }
    //public Boolean IsSorted { get; protected set; }
    #endregion

    #region Constructors
    /// <summary>Heap Constructor</summary>
    /// <param name="entries">Entries array</param>
    /// <param name="count"># of entries to use in Heap</param>
    /// <param name="ascending">Initial Heap sense</param>
    public Heap(IMeter meter, T[] entries, Int32 count, Boolean ascending = true) {
      //IsSorted = false;
      this.IsAscending = ascending;
      this.Meter = meter;
      this.Entries = entries;

      //[Warning]Side-effecting setter triggers a Build()
      this.Count = count;               //[ToDo]Use a SetCount() method here
    }

    /// <summary>Heap Constructor</summary>
    /// <param name="entries">Entries array</param>
    public Heap(IMeter meter, T[] entries)
      : this(meter, entries, entries is null ? 0 : entries.Length) {
    }

    /// <summary>Heap Constructor</summary>
    public Heap(IMeter meter = default)
      : this(meter, default) {
    }
    #endregion

    #region Interface Methods
    /// <summary>Deep Copy</summary>
    /// <param name="target">Target Heap</param>
    public void CopyTo(Heap<T> target) {
      if (Length > 0) {
        target.entries = new T[Length];
        Entries.CopyTo(target.entries, 0);
      }

      target.counter = Count;
      target.IsAscending = IsAscending;
    }

    /// <summary>Copy Constructor</summary>
    /// <param name="heap">Heap to copy</param>
    public Heap(Heap<T> heap) {
      heap.CopyTo(this);
    }

    /// <summary>Clone Heap</summary>
    /// <returns>Clone of current Heap</returns>
    public Object Clone() {
      return new Heap<T>(this);
    }
    #endregion

    #region Methods
    /// <summary>Index of parent</summary>
    /// <param name="child">Child index</param>
    protected Int32 Parent(Int32 child) {
      return (child - 1) / 2;
    }

    /// <summary>Index of left child</summary>
    /// <param name="parent">Parent index</param>
    /// <remarks>right = left + 1</remarks>
    protected Int32 Left(Int32 parent) {
      return parent * 2 + 1;
    }

    /// <summary>Used internally by Build() to add the entry at the Root Index.</summary>
    /// <remarks>O(n): Assumes both children are valid Heaps.</remarks>
    /// <param name="value">Value to add</param>
    /// <param name="root">Interim Root</param>
    protected void SiftDown(T value, Int32 root) {
      if (root < 0 || counter <= root)
        throw new IndexOutOfRangeException();

      //IsSorted = false;
      var left = Left(root);

      while (left < counter) {
        var right = left + 1;           // Select greater child:
        var bRight = false;
        if (right < counter) {
          Meter?.IncCompare();
          if (Extension.IsPredecessor(entries[left], entries[right], IsAscending))
            bRight = true;
        }

        var child = bRight ? right : left;
        Meter?.IncCompare();
        if (Extension.IsPredecessor(entries[child], value, IsAscending))
          break;
        //[Assert]entries[child] either precedes or is equal to value

        // Sift Down
        Meter?.IncMove();
        entries[root] = entries[child];
        root = child;                   // Continue with Child Heap
        left = Left(root);
      }

      Meter?.IncMove();
      entries[root] = value;
    }

    /// <summary>Rearrange Entries into a Heap.</summary>
    /// <remarks>O(n)</remarks>
    protected void Build() {            // aka, Heapify
      if (counter > 0) {
        //
        // Calling SiftDown() proceeds from right to left and reduces
        // the time complexity of this method from O(n log n) to O(n).
        //
        // Half of the nodes are leaves; and the expected number of
        // ordering operations depends on the height of the Heap.
        //
        for (var final = counter - 1; final >= 0; final--)
          SiftDown(entries[final], final);
#if DEBUG
        var valid = Validate();
#endif
      }
    }

    /// <summary>Invert Heap sense.</summary>
    /// <remarks>O(n)</remarks>
    public void Invert() {
      IsAscending = !IsAscending;
      Build();
    }

    /// <summary>Add new element to a Heap.</summary>
    /// <remarks>O(n): Not required for a HeapSort.</remarks>
    /// <param name="value">Value to add</param>
    public void SiftUp(T value) {
      if (Length <= counter)
        throw new HeapOverflowException();

      //IsSorted = false;
      var child = counter++;            // Post-Increment Count
      while (child > 0) {
        var parent = Parent(child);
        Meter?.IncCompare();
        if (Extension.IsPredecessor(value, entries[parent], IsAscending))
          break;
        //[Assert]entries[parent] either precedes or is equal to value

        // Sift Up:
        Meter?.IncMove();
        entries[child] = entries[parent];
        child = parent;                 // Continue with Parent Heap
      }

      Meter?.IncMove();
      entries[child] = value;
    }

    /// <summary>Remove root.</summary>
    /// <remarks>O(n)</remarks>
    /// <returns>Value of root entry</returns>
    public T Remove() {                 // Remove minimum entry from the root of the Heap
      if (counter > 0) {
        Meter?.IncMove();
        var value = entries[0];

        if (--counter > 0)              // Pre-Decrement Count
          SiftDown(entries[counter], 0);// ReverseSort() overwrites this final Entry

        return value;
      }

      throw new HeapUnderflowException();
    }
    #endregion

    #region Validation Methods
    private Boolean Validate() {
      if (counter > 0) {
        for (var final = counter - 1; final > 0; final--) {
          var parent = Parent(final);
          if (Extension.IsPredecessor(entries[parent], entries[final], IsAscending))
            return false;
          //[Assert]entries[final] either precedes or is equal to entries[parent]
        }
      }

      return true;
    }
    #endregion

    #region Sort Methods
    /// <summary>Perform HeapSort on the Entries array</summary>
    /// <remarks>O(n log n)</remarks>
    public void Sort() {
#if DEBUG
      var valid = Validate();
#endif
#if ShowSort
      Console.WriteLine($"IsAscending was {IsAscending}");
      var index = 0;
#endif
      foreach (var entry in this) {
        Meter?.IncMove();
#if ShowSort
        Console.WriteLine($"{index++}: {entry}");
#endif
      }
#if ShowSort
      Console.WriteLine($"IsAscending is {IsAscending}");
#endif
    }

    /// <summary>Perform Reverse HeapSort on the Entries array</summary>
    /// <remarks>O(n log n)</remarks>
    public void ReverseSort() {
      Sort();
      Reverse();
      IsAscending = !IsAscending;
    }
    #endregion

    #region Swap Methods
    /// <summary>Swap two entities of type T.</summary>
    public static void Swap(ref T e1, ref T e2) {
      var e = e1;
      e1 = e2;
      e2 = e;
    }

    /// <summary>Swap entries at the left and right indicies.</summary>
    /// <param name="entries"></param>
    /// <param name="left">Left index</param>
    /// <param name="right">Right index</param>
    protected void Swap(T[] entries, Int32 left, Int32 right) {
      Swap(ref entries[left], ref entries[right]);
      Meter?.IncMove(3);
    }

    /// <summary>Reverse Entries and Invert Heap sense</summary>
    /// <remarks>O(n): May be used after Sort() to restore Heap sense</remarks>
    protected void Reverse() {
      if (counter > 0) {
        for (Int32 left = 0, right = counter - 1; left < right; left++, right--)
          Swap(Entries, left, right);
      }
    }
    #endregion

    #region Enumerator
    /// <summary>Generic Heap enumerator</summary>
    /// <remarks>
    /// This non-standard Enumerator allows efficient removal of elements in priority
    /// order.  Entries are temporarily removed while the Enumeration is in progress.
    ///
    /// Once the final element has been removed, the count is reestablished and Heap Elements
    /// appear in Reverse Order.  This improves performance; but IsAscending must be inverted
    /// to restore the original Heap sense.
    /// </remarks>
    /// <returns>Generic enumerator</returns>
    public IEnumerator<T> GetEnumerator() {
      var count = counter;

      while (counter > 0) {             //[Note]LHS Index evaluates prior to RHS side-effects
        yield return entries[counter - 1] = Remove();
      }

      //IsSorted = true;
      IsAscending = !IsAscending;
      counter = count;                  // Avoid unnecessary Build()
    }

    /// <summary>Get non-generic enumerator</summary>
    /// <remarks>Explicit implementation</remarks>
    /// <returns>Non-generic enumerator</returns>
    IEnumerator IEnumerable.GetEnumerator() {
      return GetEnumerator();           // IEnumerable implementation casts IEnumerator<T> as IEnumerator
    }
    #endregion
  }
}
