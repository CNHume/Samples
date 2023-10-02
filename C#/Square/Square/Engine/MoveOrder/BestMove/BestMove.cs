namespace MoveOrder;

using Engine;

using static Engine.Board;

//
// Type Aliases:
//
using Eval = Int16;
using Hashcode = UInt64;

internal struct BestMove {
  #region Fields
  public Move Move;
  public String FEN;
  public Hashcode Hash;
  #endregion

  #region Constructor
  public BestMove(Move move, String sFEN, Hashcode qHash) {
    Move = move;
    FEN = sFEN;
    Hash = qHash;
  }
  #endregion
}
