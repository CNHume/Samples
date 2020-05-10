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
using Fermat.Parsers;
using Fermat.Settings;

using System;
using System.Diagnostics;

using static Fermat.Math.Modular;

namespace Fermat.Math {
  public class ModularTest {
    #region Properties
    public decimal Input { get; set; }
    public decimal Encoder { get; set; }
    public decimal Modulus { get; set; }
    public static TestSetting Settings { get; set; }
    #endregion

    #region Constructors
    public ModularTest(Command command, TestSetting settings) {
      Input = command.Input.Value;
      Encoder = command.Encoder.Value;
      Modulus = command.Modulus.Value;

      Settings = settings;
    }
    #endregion

    #region Methods
    public decimal TestModPower() {
      Console.WriteLine($"input = {Input}, encoder = {Encoder}, modulus = {Modulus}");

      //[Note]totient is kept secret; but must be equal to totient(modulus)
      // Although modulus is public, totient(modulus) is hard to calculate.
      var totient = Settings.Totient;

      //[Note]Encoder must be relatively prime to Totient
      var inverse = ModInverse(Encoder, totient);
#if DEBUG
      Console.WriteLine($"totient = {totient}");
      Console.WriteLine($"inverse = {inverse}");
#endif
      var product = Encoder * inverse % totient;
#if DEBUG
      Console.WriteLine($"encoder * inverse % totient = {product}");
#endif
      var encoded = ModPower(Input, Encoder, Modulus);
      var decoded = ModPower(encoded, inverse, Modulus);

      Console.WriteLine($"encoded = {encoded}");
      Console.WriteLine($"decoded = {decoded}");
      Debug.Assert(decoded == Input, $"decoded = {decoded} != Input = {Input}");

      return encoded;
    }
    #endregion
  }
}
