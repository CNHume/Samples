/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 *
 * For background on the development of Reversi and Othello
 * see https://en.wikipedia.org/wiki/Reversi
 */
package othello;
import java.util.Stack;

class Pool<T> {
  final int nDefaultAllocation = 12;
  public int activeCount;
  public int activePeak;
  private Stack<T> inactive;

  public String name;
  private Factory<T> factory;

  public Pool(Factory<T> factory, String name) {
    this.factory = factory;
    this.name = name;
    inactive = new Stack<T>();
    Clear();
  }

  public void Clear() {
    activePeak = activeCount = 0;
  }

  private void Allocate(int nAllocations) {
    for (int n = 0; n < nAllocations; n++)
      inactive.push(factory.create());
  }

  public T push() {
    if (inactive.size() == 0)
      Allocate(nDefaultAllocation);

    T top = inactive.pop();
    assert top != null;			// Empty inactive Pool

    IncActive();
    return top;
  }

  public void pop(T top) throws NullPointerException {
    if (top == null)
      throw new NullPointerException("Pool.pop() called with null argument");

    inactive.push(top);
    DecActive();
  }

  private void IncActive() {
    if (activePeak < ++activeCount) {
      activePeak = activeCount;
      //[Debug Peak]DisplayActive();
    }
  }

  private void DecActive() {
    activeCount--;
  }

  public void DisplayActive() {
    Logger.logLevel(
      Logger.DEBUG,
      String.format("%s Count = %d, Peak = %d", name, activeCount, activePeak));
  }
}
