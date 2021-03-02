//
// (C) Copyright 2010-2021, Christopher N. Hume.  All rights reserved.
//
namespace HeapTests {
  using HeapSort;

  using Xunit;
  using Xunit.Abstractions;

  using System;

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
    public void TestEnumerator(int[] entries) {
#if DEBUG
      OutputHelper.WriteLine("TestEnumerator");
#endif
      var heap = new Heap<Int32>(entries);
      //heap.Sort();
#if DEBUG
      OutputHelper.WriteLine(String.Join(" ", heap));
      OutputHelper.WriteLine(String.Join(" ", heap));
#endif
    }

    [Theory]
    [InlineData(new[] { 101, 3, 44, 55, 8, 17, 6 })]
    public void CloneTest(int[] entries) {
#if DEBUG
      OutputHelper.WriteLine("CloneTest");
#endif
      var heap = new Heap<Int32>(entries);
      var heap2 = (Heap<Int32>)heap.Clone();
      //heap.Sort();
#if DEBUG
      OutputHelper.WriteLine(String.Join(" ", heap));
      OutputHelper.WriteLine(String.Join(" ", heap2));
#endif
    }
    #endregion
  }
}
