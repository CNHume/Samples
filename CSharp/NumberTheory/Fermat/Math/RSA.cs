//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
// The following is based on the RSA Public-Key Encryption Algorithm described in
// "A Method for Obtaining Digital Signatures and Public-Key Cryptosystems"
// by R.L. Rivest, A. Shamir, and L. Adleman
// published February 1978, Communications of the ACM (Vol. 21, No. 2, pp. 120-126)
//
// See https://people.csail.mit.edu/rivest/Rsapaper.pdf
//
// The method is based on fundamental properties of Finite Cyclic Groups,
// which arise in modular arithmetc.
//
using System.Diagnostics;

using static Fermat.Math.Modular;

namespace Fermat.Math {
  public class RSA {
    #region Properties
    public decimal EncodePower { get; set; }
    public decimal Modulus { get; set; }
    #endregion

    #region Constructors
    public RSA(decimal encodePower, decimal modulus) {
      Modulus = modulus;
      EncodePower = encodePower;
    }
    #endregion

    #region Methods
    public decimal TestEncodeAndDecode(decimal input, decimal totient) {
      var encoded = Encode(input);
      var decoded = Decode(encoded, totient);
      Debug.Assert(decoded == input, $"decoded = {decoded} != input = {input}");
      return encoded;
    }

    public decimal Encode(decimal input) {
      return ModPower(input, EncodePower, Modulus);
    }

    //
    //[Note]Totient is kept secret but must be equal to totient(modulus).
    // Although modulus is public, totient(modulus) is hard to calculate.
    //
    public decimal Decode(decimal encoded, decimal totient) {
      //[Note]EncodePower must be relatively prime to totient
      var decodePower = ModInverse(EncodePower, totient);
      return ModPower(encoded, decodePower, Modulus);
    }
    #endregion
  }
}
