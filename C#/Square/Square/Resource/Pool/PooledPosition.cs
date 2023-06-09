//
// Copyright (C) 2010-2023, Christopher N. Hume.  All rights reserved.
//
//[2023-02-07 CNHume]Created Class from Pool.cs
//
// Conditionals:
//
//#define DebugPeak

using Engine;

namespace Resource;

using static Logging.Logger;

class PooledPosition {
  #region Constants
  const Int32 nDefaultAllocation = 12;
  #endregion

  #region Constructors
  public PooledPosition(GameState state, String? name = default) {
    State = state;
    DefaultElement = new Position(State);
    Name = name ?? typeof(Position).Name;
    Inactive = new Stack<Position>();
    Clear();
  }
  #endregion

  #region Methods
  public void Clear() {
    ActivePeak = ActiveCount = 0U;
  }

  private void Allocate(Int32 nAllocations) {
    for (var n = 0; n < nAllocations; n++)
      Inactive.Push(new Position(State));       // Lazy<T> would defer invocation of initParameters()
  }

  public Position Push() {
    if (Inactive == null)
      throw new ApplicationException("No Inactive Pool");

    if (Inactive.Count == 0)
      Allocate(nDefaultAllocation);

    var top = Inactive.Pop();
    IncActive();
    return top;
  }

  public void Pop(ref Position top) {
    Inactive.Push(top);
    DecActive();
    top = DefaultElement;
  }

  private void IncActive() {
    if (ActivePeak < ++ActiveCount) {
      ActivePeak = ActiveCount;
#if DebugPeak
      DisplayActive();
#endif
    }
  }

  private void DecActive() {
    ActiveCount--;
  }

  public void DisplayActive() {
    LogInfo(LogLevel.data, $"{Name} Count = {ActiveCount}, Peak = {ActivePeak}");
  }
  #endregion

  #region Fields
  private Stack<Position> Inactive;
  public UInt32 ActiveCount;
  public UInt32 ActivePeak;
  #endregion

  #region Properties
  public Position DefaultElement { get; init; }
  public GameState State { get; init; }
  public String Name { get; init; }
  #endregion
}
