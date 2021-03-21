namespace HeapSort {
  using System;

  public interface IMeter {
    void IncCompare(UInt64 count = 1);
    void IncMove(UInt64 count = 1);
    void IncPart(UInt64 count = 1);
  }
}
