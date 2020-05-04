//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
namespace SortTest {
  using Sort;
  using NUnit.Framework;
  using System;

  [TestFixture]
  class HeapTest {
    [Test]
    public static void EnumeratorTest() {
      Console.WriteLine("EnumeratorTest");
      var foo = new[] { 101, 3, 44, 55, 8, 17, 6 };
      var heap = new Heap<Int32>(foo);
      //heap.Sort();
      Console.WriteLine(String.Join(" ", heap));
      Console.WriteLine(String.Join(" ", heap));
    }

    [Test]
    public static void CloneTest() {
      Console.WriteLine("CloneTest");
      var foo = new[] { 101, 3, 44, 55, 8, 17, 6 };
      var heap = new Heap<Int32>(foo);
      var heap2 = (Heap<Int32>)heap.Clone();
      //heap.Sort();
      Console.WriteLine(String.Join(" ", heap));
      Console.WriteLine(String.Join(" ", heap2));
    }
  }
}
