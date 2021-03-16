namespace SortTest {
  using System;

  public interface IMeter {
    public void IncCompare(UInt64 count = 1);
    public void IncMove(UInt64 count = 1);
    public void IncPart(UInt64 count = 1);
  }
}
