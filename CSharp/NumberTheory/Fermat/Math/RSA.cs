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
using System;
using System.Diagnostics;

using static Fermat.Math.Modular;

namespace Fermat.Math {
  public class RSA {
    #region Properties
    public decimal EncodePower { get; set; }
    public decimal Modulus { get; set; }
    public decimal Totient { get; set; }
    #endregion

    #region Constructors
    public RSA(decimal encodePower, decimal modulus, decimal totient) {
      EncodePower = encodePower;
      Modulus = modulus;
      Totient = totient;
    }
    #endregion

    #region Methods
    public decimal TestEncodeAndDecode(decimal input) {
      var encoded = Encode(input);
      var decoded = Decode(encoded);
      Debug.Assert(decoded == input, $"decoded = {decoded} != Input = {input}");
      return encoded;
    }

    //
    //[Note]Totient is kept secret but must be equal to totient(modulus).
    // Although modulus is public, totient(modulus) is hard to calculate.
    //
    public decimal Encode(decimal input) {
      var encoded = ModPower(input, EncodePower, Modulus);
      Console.WriteLine($"{encoded} = Encode(input = {input}, encodePower = {EncodePower}, modulus = {Modulus})");
      return encoded;
    }

    public decimal Decode(decimal encoded) {
      //[Note]EncodePower must be relatively prime to Totient
      var decodePower = ModInverse(EncodePower, Totient);
#if DEBUG
      Console.WriteLine($"decodePower = {decodePower} = ModInverse(encodePower = {EncodePower}, totient = {Totient})");

      var product = EncodePower * decodePower % Totient;
      Debug.Assert(product == 1, $"1 != {product} = encodePower = {EncodePower} * decodePower = {decodePower} % totient = {Totient}");
#endif
      var decoded = ModPower(encoded, decodePower, Modulus);
      Console.WriteLine($"{decoded} = Decode(encoded = {encoded}, decodePower = {decodePower}, modulus = {Modulus})");
      return decoded;
    }
    #endregion
  }
}
