//
// (C) Copyright 2010-2021, Christopher N. Hume.  All rights reserved.
//
using static System.String;

namespace HeapTests {
  using HeapSort;

  using Xunit;
  using Xunit.Abstractions;

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
      var heap = new Heap<Int32>(entries);
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
      var heap = new Heap<Int32>(entries);
      var heap2 = (Heap<Int32>)heap.Clone();
      //heap.Sort();
#if DEBUG
      OutputHelper.WriteLine(Join(" ", heap));
      OutputHelper.WriteLine(Join(" ", heap2));
#endif
    }
    #endregion
  }
}
