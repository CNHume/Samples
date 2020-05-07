//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Exceptions;

using System;

namespace Fermat {
  public static class Math {
    #region Properties
    public static Rule[] Rules { get; set; }
    #endregion

    #region Constructors
    static Math() {
      Rules = new Rule[] {
        new Rule(Token.NavigatorID, @"\d{3} \d{3} \d{4}"),
        new Rule(Token.NavigatorID, @"\d{3}-\d{3}-\d{4}"),
        new Rule(Token.NavigatorID, @"\d{10}"),
      };
    }
    #endregion

    #region Methods
    public static void TestModPower(Command cmd) {
      var input = cmd.Input.Value;
      var exp = cmd.Phi.Value;
      var mod = cmd.Modulus.Value;
      //[Test]Console.WriteLine($"input = {input}, exp = {exp}, mod = {mod}");

      var encode = 29m;
      var decode = exp - encode + 1;

      var encoded = ModPower(input, encode, mod);
      var inverse = ModPower(input, decode, mod);
      var decoded = (encoded * inverse) % mod;

      Console.WriteLine($"{decoded} = {encoded} * {inverse}");

      var text = Parser.FormatNavigatorId(encoded);
      Parser.ParseNavigatorID(text);
    }

    /// <summary>
    /// Calculates (input ^ exp) % mod
    /// </summary>
    /// <remarks>Based on Fermat's Little Theorem</remarks>
    /// <param name="input">the base</param>
    /// <param name="exp">the exponent</param>
    /// <param name="mod">the modulus</param>
    /// <returns></returns>
    public static decimal ModPower(decimal input, decimal exp, decimal mod) {
      validate(input, exp, mod);
      var output = 1m;
      var bit = 1m;
      var n = 0;                        // pbl(exp) the bit-width
      for (; bit <= exp; n++) bit *= 2;

      var mask = exp;
      for (; bit > 1;) {
        bit /= 2;
        output = (output * output) % mod;
        if (bit <= mask) {
          mask -= bit;
          output = (input * output) % mod;
        }
      }

      return output;
    }

    private static void validate(decimal input, decimal exp, decimal mod) {
      string message = null;
      if (hasFraction(input))
        message = $"whole input = {input}";
      else if (hasFraction(exp))
        message = $"natural exp = {exp}";
      else if (hasFraction(mod))
        message = $"whole mod = {mod}";
      else if (exp < 0)
        message = $"0 <= exp = {exp}";
      else if (mod <= 1)
        message = $"1 < mod = {mod}";
      else if (input <= 1)
        message = $"1 < input = {input}";
      else if (mod <= input)
        message = $"input = {input} < mod = {mod}";

      if (!string.IsNullOrEmpty(message))
        throw new ValidationException($"Invalid: {message}");
    }

    private static bool hasFraction(decimal number) {
      return number != decimal.Truncate(number);
    }
    #endregion
  }
}
