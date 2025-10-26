//
// Copyright (C) 2019-2025, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using Xunit;
using Xunit.Abstractions;

namespace FermatTests;

using Fermat.Maths;

using static Fermat.Parsers.Parser;

public class RSATest {
  #region Properties
  public ITestOutputHelper OutputHelper { get; set; }
  #endregion                            // Properties

  #region Constructors
  public RSATest(ITestOutputHelper outputHelper) {
    OutputHelper = outputHelper;
  }
  #endregion                            // Constructors

  #region Methods
  [Theory]
  [InlineData("1001", "29", "9999999929", "9999999928")]
  [InlineData("1001", "29", "99999999999997", "99880810487836")]
  [InlineData("1001", "29", "99999999999989", "99999956900556")]
  public void TestRSA(
    string inputString, string encodePowerString, string modulusString, string totientString) {
    // The [InlineData] attribute does not support decimal
    var input = Convert.ToDecimal(inputString);
    var encodePower = Convert.ToDecimal(encodePowerString);
    var modulus = Convert.ToDecimal(modulusString);
    var totient = Convert.ToDecimal(totientString);

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
  #endregion                            // Methods
}

public class ParserTest {
  #region Properties
  public ITestOutputHelper OutputHelper { get; set; }
  #endregion                            // Properties

  #region Constructors
  public ParserTest(ITestOutputHelper outputHelper) {
    OutputHelper = outputHelper;
  }
  #endregion                            // Constructors

  #region Methods
  [Theory]
  [InlineData("9876543210")]
  public void TestFormatAndParse(string idString) {
    // The [InlineData] attribute does not support decimal
    var id = Convert.ToDecimal(idString);

    var formattedId = FormatNavigatorId(id);
    var telephoneId = ParseTelephoneID(formattedId);
    Assert.True(telephoneId == id, $"telephoneId = {telephoneId} != id = {id}");
#if DEBUG
    OutputHelper.WriteLine($"{telephoneId} = {formattedId}");
#endif
  }
  #endregion                            // Methods
}
