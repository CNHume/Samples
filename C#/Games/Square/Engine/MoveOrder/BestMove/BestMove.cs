namespace MoveOrder;

using Engine;

using static Engine.Board;

//
// Type Aliases:
//
using Hashcode = UInt64;

#region Enumerations
public enum BestMoveEnum : byte {
  None,
  SearchDraw, SearchFinal, SearchProbe, SearchUpdate,
  QuietDraw, QuietFinal, QuietProbe, QuietUpdate
}
#endregion

internal struct BestMove {
  #region Fields
  public Move Move;
  public BestMoveEnum BME;
  public String? FEN;
  public Hashcode? Hash;
  public Boolean WTM;
  #endregion

  #region Constructor
  public BestMove(Move move, BestMoveEnum bme, Position position) {
    BME = bme;
    Move = move;

    FEN = position.ToString();
    Hash = position.Hash;
    WTM = position.WTM();
  }
  #endregion
}
