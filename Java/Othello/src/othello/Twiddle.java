/**
 * @author Copyright (C) 2018, Christopher N. Hume. All rights reserved.
 *
 * You should have received a copy of the MIT License along with this program.
 * If not, see https://opensource.org/licenses/MIT.
 */
package othello;

public class Twiddle {
  public long r;
  public long s;

  //
  // The lowest bit in a word is isolated by performing an AND with the value in the word
  // and its two's complement: v & -v.  The bit position n within the m-bit word can then
  // be obtained after multiplying an m-bit de Bruijn constant by this isolated bit value
  // and extracting the unique bit pattern from the high order p-bit byte, where p is the
  // PBL of m.  A lookup table corresponding to the de Bruijn constant is then indexed by
  // the unique p-bit value to obtain the bit position n.
  //
  // Note that m-bit multiplies will suffice.  It is also easy to implement half-multiply,
  // which is faster in the case of a 64-bits, because the isolated bit is a power of two.
  //
  // See "Using de Bruijn Sequences to Index a 1 in a Computer Word"
  // Charles E. Leiserson, Harald Prokop, Keith H. Randall, 1998-07-07, MIT LCS
  //
  public final static int nDeBruijn = 0x077CB531;
  // From the Paper: 0000 0111 0111 1100 1011 0101 0011 0001
  //protected static final byte[] deBruijnHalf
  // = { 0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  //	31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9 };

  protected static byte[] deBruijnHalf;
  protected final static int MASK5 = (1 << 5) - 1;

  public int removeLo(long input) {
    r = input;
    s = r & (~r + 1);			// s = r & -r to isolate the lowest/first bit
    r ^= s;				// r = r & (r - 1); clear from remaining bits

    int u = (int)s;			// Half de Bruijn: Avoiding 64-Bit Multiplication
    int n = 0;

    if (u == 0) {
      u = (int)(s >> 32);
      assert u != 0;			// No Bit Found
      if (u == 0) return -1;
      n = 32;
    }

    int p = (u * nDeBruijn >> 32 - 5) & MASK5;
    return deBruijnHalf[p] + n;
  }

  public static byte[] newDeBruijn(int nLog) {
    assert nLog <= 8;			// nLog too large
    int nLength = 1 << nLog;
    return new byte[nLength];
  }

  public final static void loadDeBruijn(byte[] deBruijnMap, int nLog, long qDeBruijnNumber) {
    int nLength = 1 << nLog;
    assert deBruijnMap.length == nLength;	// Inconsistent Length
    int nMask = nLength - 1;
    long m = qDeBruijnNumber;
    for (int n = 0; n < nLength; n++, m <<= 1) {
      int p = (int)(nMask & (m >> nLength - nLog));
      assert p < deBruijnMap.length;	// Index Out of Range
      deBruijnMap[p] = (byte)n;
    }

    //[Test]
//    StringBuilder sb = new StringBuilder();
//    String sDelim = "";
//    for (int n = 0; n < nLength; n++) {
//      sb.append(String.format("%s%2s", sDelim, deBruijnMap[n]));
//      sDelim = ", ";
//    }
//    Logger.LogLine(sb.toString());
  }
}
