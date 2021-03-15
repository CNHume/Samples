namespace Sort {
  using System;

  public interface ICounter {
    public void IncCompare(UInt64 count = 1);
    public void IncMove(UInt64 count = 1);
    public void IncPart(UInt64 count = 1);
  }
}
