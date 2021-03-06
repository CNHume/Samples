//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using Fermat.Math;

using Xunit;
using Xunit.Abstractions;

using static Fermat.Parsers.Parser;

namespace FermatTests {
  public class RSATest {
    #region Properties
    public ITestOutputHelper OutputHelper { get; set; }
    #endregion

    #region Constructors
    public RSATest(ITestOutputHelper outputHelper) {
      OutputHelper = outputHelper;
    }
    #endregion

    #region Methods
    [Theory]
    [InlineData(1001, 29, 9999999929, 9999999928)]
    [InlineData(1001, 29, 99999999999997, 99880810487836)]
    [InlineData(1001, 29, 99999999999989, 99999956900556)]
    public void TestRSA(decimal input, decimal encodePower, decimal modulus, decimal totient) {
      var rsa = new RSA(encodePower, modulus);
      var decodePower = Modular.ModInverse(rsa.EncodePower, totient);
#if DEBUG
      OutputHelper.WriteLine($"decodePower = {decodePower} = ModInverse(encodePower = {rsa.EncodePower}, totient = {totient})");
      var product = rsa.EncodePower * decodePower % totient;
      Assert.True(product == 1, $"1 != {product} = encodePower = {rsa.EncodePower} * decodePower = {decodePower} % totient = {totient}");
#endif
      var encoded = rsa.Encode(input);
#if DEBUG
      OutputHelper.WriteLine($"{encoded} = Encode(input = {input}, encodePower = {rsa.EncodePower}, modulus = {rsa.Modulus})");
#endif
      var decoded = rsa.Decode(encoded, totient);
#if DEBUG
      OutputHelper.WriteLine($"{decoded} = Decode(encoded = {encoded}, decodePower = {decodePower}, modulus = {rsa.Modulus})");
#endif
      Assert.True(decoded == input, $"decoded = {decoded} != input = {input}");
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
