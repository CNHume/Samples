using Fermat.Parsing;
using Fermat.Settings;

using System;

using static Fermat.Math.Modular;

namespace Fermat.Math {
  public class ModularTest {
    #region Properties
    public decimal Input { get; set; }
    public decimal Totient { get; set; }
    public decimal Modulus { get; set; }
    public static TestSettings Settings { get; set; }
    #endregion

    #region Constructors
    public ModularTest(Command command, TestSettings settings) {
      Input = command.Input.Value;
      Totient = command.Totient.Value;
      Modulus = command.Modulus.Value;

      Settings = settings;
    }
    #endregion

    #region Methods
    public void TestModPower() {
      //[Note]totient is assumed equal to totient(modulus)
      //[Test]
      Console.WriteLine($"input = {Input}, totient = {Totient}, modulus = {Modulus}");

      var encoder = Settings.FermatEncoder;     // Must be relatively prime to totient
      var inverse = ModInverse(encoder, Totient);

      Console.WriteLine($"encoder = {encoder}");
      Console.WriteLine($"inverse = {inverse}");

      var product = encoder * inverse % Totient;
      Console.WriteLine($"encoder * inverse % totient = {product}");

      var encoded = ModPower(Input, encoder, Modulus);
      var decoded = ModPower(encoded, inverse, Modulus);

      Console.WriteLine($"encoded = {encoded}");
      Console.WriteLine($"decoded = {decoded}");

      var formattedId = Parser.FormatNavigatorId(encoded);
      var navigatorId = Parser.ParseNavigatorID(formattedId);

      Console.WriteLine($"{navigatorId} = {formattedId}");
    }
    #endregion
  }
}
