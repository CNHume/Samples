﻿//
// (C) Copyright 2010-2021, Christopher N. Hume.  All rights reserved.
//
using Xunit;
using Xunit.Abstractions;

namespace HeapTests;

using Sorters;

using static System.String;

public class HeapTest {
  #region Properties
  public ITestOutputHelper OutputHelper { get; set; }
  #endregion

  #region Constructors
  public HeapTest(ITestOutputHelper outputHelper) {
    OutputHelper = outputHelper;
  }
  #endregion

  #region Methods
  [Theory]
  [InlineData(new[] { 101, 3, 44, 55, 8, 17, 6 })]
  public void TestEnumerator(Int32[] entries) {
#if DEBUG
    OutputHelper.WriteLine("TestEnumerator");
#endif
    Heap<Int32> heap = new(entries);
    //heap.Sort();
#if DEBUG
    OutputHelper.WriteLine(Join(" ", heap));
    OutputHelper.WriteLine(Join(" ", heap));
#endif
  }

  [Theory]
  [InlineData(new[] { 101, 3, 44, 55, 8, 17, 6 })]
  public void CloneTest(Int32[] entries) {
#if DEBUG
    OutputHelper.WriteLine("CloneTest");
#endif
    Heap<Int32> heap = new(entries);
    var heap2 = (Heap<Int32>)heap.Clone();
    //heap.Sort();
#if DEBUG
    OutputHelper.WriteLine(Join(" ", heap));
    OutputHelper.WriteLine(Join(" ", heap2));
#endif
  }
  #endregion
}
