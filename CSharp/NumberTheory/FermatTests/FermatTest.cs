//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using Fermat.Parsers;
using Fermat.Settings;

using System;

using Xunit;
using Xunit.Abstractions;
using static Fermat.Math.Modular;
using static Fermat.Parsers.Parser;

namespace FermatTests {
  public class ModularTestFixture {
    public ModularTestFixture() {
    }
  }

  public class ModularTest : IClassFixture<ModularTestFixture> {
    #region Properties
    public decimal Input { get; set; }
    public decimal Encoder { get; set; }
    public decimal Modulus { get; set; }
    public decimal Totient { get; set; }
    public ITestOutputHelper OutputHelper { get; set; }
    #endregion

    #region Constructors
    //[InlineData(1001, 29, 9999999929, 9999999928)]
    public ModularTest(decimal input, decimal encoder, decimal modulus, decimal totient, ITestOutputHelper outputHelper) {
      Input = input;
      Encoder = encoder;
      Modulus = modulus;
      Totient = totient;
      OutputHelper = outputHelper;
    }
    #endregion

    #region Methods
    [Fact]
    public void TestModPower() {
      //[Note]Totient is kept secret; but must be equal to totient(modulus)
      // Although modulus is public, totient(modulus) is hard to calculate.
      OutputHelper.WriteLine($"input = {Input}, encoder = {Encoder}, modulus = {Modulus}, totient = {Totient}");

      //[Note]Encoder must be relatively prime to Totient
      var inverse = ModInverse(Encoder, Totient);
#if DEBUG
      OutputHelper.WriteLine($"totient = {Totient}");
      OutputHelper.WriteLine($"inverse = {inverse}");
#endif
      var product = Encoder * inverse % Totient;
#if DEBUG
      OutputHelper.WriteLine($"encoder * inverse % totient = {product}");
#endif
      var encoded = ModPower(Input, Encoder, Modulus);
      var decoded = ModPower(encoded, inverse, Modulus);
#if DEBUG
      OutputHelper.WriteLine($"encoded = {encoded}");
      OutputHelper.WriteLine($"decoded = {decoded}");
#endif
      Assert.True(decoded == Input, $"decoded = {decoded} != Input = {Input}");
    }
    #endregion
  }

  public class ParserTest {
    #region Properties
    public ITestOutputHelper OutputHelper { get; set; }
    #endregion

    #region Constructors
    public ParserTest(ITestOutputHelper outputHelper) {
      OutputHelper = outputHelper;
    }
    #endregion

    #region Methods
    [Theory]
    [InlineData(9876543210)]
    public void TestFormatAndParse(decimal id) {
      var formattedId = FormatNavigatorId(id);
      var telephoneId = ParseTelephoneID(formattedId);
      Assert.True(telephoneId == id, $"telephoneId = {telephoneId} != id = {id}");
#if DEBUG
      OutputHelper.WriteLine($"{telephoneId} = {formattedId}");
#endif
    }
    #endregion
  }
}
