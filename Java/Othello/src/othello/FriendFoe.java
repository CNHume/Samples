/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
package othello;
import static othello.Board.nFiles;

public class FriendFoe {
  public final static int SIZE = nFiles;
  //[Forward]private final static int nStateHiBit = 1 << SIZE - 1;
  private final static int modulus = 3;

  // Because each square can only be friend, foe or empty
  // there are 3**8 = 6561 possible FriendFoeState values,
  // 10% of the product of possible friend and foe states.
  public int state;
  public int friend;
  public int foe;

  public FriendFoe(int friend, int foe) {
    encode(friend, foe);
  }

  public FriendFoe(int state) {
    decode(this.state);
  }

  public FriendFoe() {
    decode(0);
  }

  public final void encode(int friend, int foe) {
    this.friend = friend;
    this.foe = foe;
    this.state = 0;

    //[Forward]for (int index = 0; index < SIZE; index++, friend <<= 1, foe <<= 1) {
    //[Reverse]
    for (int index = 0; index < SIZE; index++, friend >>= 1, foe >>= 1) {
      state *= modulus;			// Base 3 encoding
//[Forward]
//      if ((nStateHiBit & friend) != 0) state += 1;
//      if ((nStateHiBit & foe) != 0) state += 2;
//[Reverse]
      if ((friend & 1) != 0) state += 1;
      if ((foe & 1) != 0) state += 2;
    }
  }

  public final void decode(int state) {
    this.state = state;
    friend = 0;
    foe = 0;
    for (int index = 0; index < SIZE; index++, state /= modulus) {
//[Forward]
//      friend >>= 1;
//      foe >>= 1;      
//      switch (state % modulus) {
//      case 1:
//	friend |= nStateHiBit;
//	break;
//      case 2:
//	foe |= nStateHiBit;
//	break;
//      }
//[Reverse]
      friend <<= 1;
      foe <<= 1;
      switch (state % modulus) {
      case 1:
	friend |= 1;
	break;
      case 2:
	foe |= 1;
	break;
      }
    }
  }
}
