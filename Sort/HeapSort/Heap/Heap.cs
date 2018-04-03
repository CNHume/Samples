﻿//
// Copyright (C) 2017, Christopher N. Hume.  All rights reserved.
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
namespace Sort {
  using Exceptions;
  using System;
  using System.Collections;		// For non-generic IEnumerable
  using System.Collections.Generic;

  public class Heap<T> : ICloneable, IEnumerable<T> where T : IComparable {
    #region Fields
    private T[] entries;
    protected Int32 counter;
    #endregion

    #region Constructors
    /// <summary>Heap Constructor</summary>
    /// <param name="entries">Entries array</param>
    /// <param name="count"># of entries to use in Heap</param>
    /// <param name="ascending">Initial Heap sense</param>
    public Heap(T[] entries, Int32 count, Boolean ascending = true) {
      //IsSorted = false;
      IsAscending = ascending;
      Entries = entries;
      Count = count;                    // This Count assignment triggers Build()
    }

    /// <summary>Heap Constructor</summary>
    /// <param name="entries">Entries array</param>
    public Heap(T[] entries)
      : this(entries, entries == null ? 0 : entries.Length) {
    }

    /// <summary>Heap Constructor</summary>
    public Heap()
      : this((T[])null) {
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
        var bRight = right < counter && entries[left].CompareTo(entries[right]) < 0 == IsAscending;
        var child = bRight ? right : left;

        if (entries[child].CompareTo(value) < 0 == IsAscending)
          break;

        // Sift Down
        entries[root] = entries[child];
        root = child;                 // Continue with Child Heap
        left = Left(root);
      }

      entries[root] = value;
    }

    /// <summary>Rearrange Entries into a Heap.</summary>
    /// <remarks>O(n)</remarks>
    protected void Build() {		// aka, Heapify
      if (counter < 2) return;

      //
      // Calling SiftDown() proceeds from right to left and reduces
      // the time complexity of this method from O(n log n) to O(n).
      //
      // Half of the nodes are leaves; and the expected number of
      // ordering operations depends on the height of the Heap.
      //
      for (var final = counter - 1; final >= 0; final--)
        SiftDown(entries[final], final);
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
        if (value.CompareTo(entries[parent]) < 0 == IsAscending)
          break;

        // Sift Up:
        entries[child] = entries[parent];
        child = parent;                 // Continue with Parent Heap
      }

      entries[child] = value;
    }

    /// <summary>Remove root.</summary>
    /// <remarks>O(n)</remarks>
    /// <returns>Value of root entry</returns>
    public T Remove() {	                // Remove minimum entry from the root of the Heap
      if (counter < 1)
        throw new HeapUnderflowException();

      var value = entries[0];

      if (--counter > 0)                // Pre-Decrement Count
        SiftDown(entries[counter], 0);  // ReverseSort() overwrites this final Entry

      return value;
    }
    #endregion

    #region Sort Methods
    /// <summary>Perform HeapSort on the Entries array</summary>
    /// <remarks>O(n log n)</remarks>
    public void Sort() {
      foreach (var entry in this) ;
    }

    /// <summary>Perform Reverse HeapSort on the Entries array</summary>
    /// <remarks>O(n log n)</remarks>
    public void ReverseSort() {
      Sort();
      Reverse();
    }

    /// <summary>Reverse Entries and Invert Heap sense</summary>
    /// <remarks>O(n): May be used after Sort() to restore Heap sense</remarks>
    protected void Reverse() {
      IsAscending = !IsAscending;
      if (counter < 2) return;
      for (Int32 left = 0, right = counter - 1; left < right; left++, right--)
        Swap(Entries, left, right);
    }

    /// <summary>Swap entries at the left and right indicies.</summary>
    /// <param name="entries"></param>
    /// <param name="left">Left index</param>
    /// <param name="right">Right index</param>
    protected static void Swap(T[] entries, int left, int right) {
      var entry = entries[left];
      entries[left] = entries[right];
      entries[right] = entry;
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
      var n = counter;

      while (counter > 0) {             // Left-Hand Operand First:
        yield return entries[counter - 1] = Remove();
      }

      //IsSorted = true;
      IsAscending = !IsAscending;
      counter = n;                      // Prevent unecessary Build()
    }

    /// <summary>Get non-generic enumerator</summary>
    /// <remarks>Explicit implementation</remarks>
    /// <returns>Non-generic enumerator</returns>
    IEnumerator IEnumerable.GetEnumerator() {
      return GetEnumerator();           // Cast generic implementation to IEnumerable
    }
    #endregion

    #region Properties
    /// <summary>Heap sense</summary>
    public Boolean IsAscending { get; protected set; }
    //public Boolean IsSorted { get; protected set; }

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
        return entries == null ? 0 : entries.Length;
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
    #endregion
  }
}
