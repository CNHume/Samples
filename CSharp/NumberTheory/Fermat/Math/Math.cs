//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
using Fermat.Exceptions;

using System;
using System.Diagnostics;

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
    // The following is based on Fermat's Little Theorem
    public static void TestModPower(Command cmd) {
      var input = cmd.Input.Value;
      var phi = cmd.Phi.Value;
      var mod = cmd.Modulus.Value;
      //[Test]Console.WriteLine($"input = {input}, phi = {phi}, mod = {mod}");

      var encode = 29m;
      var decode = ModInverse(encode, phi);
      Console.WriteLine($"encode = {encode}");
      Console.WriteLine($"decode = {decode}");

      var product = (encode * decode) % phi;
      Console.WriteLine($"encode * decode % mod = {product}");

      var encoded = ModPower(input, encode, mod);
      var decoded = ModPower(encoded, decode, mod);

      Console.WriteLine($"encoded = {encoded}");
      Console.WriteLine($"decoded = {decoded}");

      var formattedId = Parser.FormatNavigatorId(encoded);
      var navigatorId = Parser.ParseNavigatorID(formattedId);

      Console.WriteLine($"{navigatorId} = {formattedId}");
    }

    public static decimal ModInverse(decimal input, decimal mod) {
      var output = 0m;
      var inverse = 1m;
      var numerator = mod;
      var denominator = input;

      while (denominator != 0) {
        var remainder = numerator % denominator;
        var quotient = (numerator - remainder) / denominator;
        Debug.Assert(!hasFraction(quotient), $"Non-integral quotient = {quotient}");

        var previous = output;
        output = inverse;
        inverse = modulo(previous - inverse * quotient, mod);

        numerator = denominator;
        denominator = remainder;
      }

      return output;
    }

    /// <summary>
    /// Returns n mod m
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    public static decimal modulo(decimal n, decimal m) {
      var remainder = n % m;
      return remainder < 0 ? remainder + m : remainder;
    }

    /// <summary>
    /// Returns n ^ exp mod m
    /// </summary>
    /// <remarks>Implelements modular exponentiation</remarks>
    /// <param name="n">the base</param>
    /// <param name="exp">the exponent</param>
    /// <param name="m">the modulus</param>
    /// <returns></returns>
    public static decimal ModPower(decimal n, decimal exp, decimal m) {
      validate(n, exp, m);
      var product = 1m;
      var bit = 1m;
      var pbl = 0;                      // bit-width of exp
      for (; bit <= exp; pbl++) bit *= 2;

      var mask = exp;
      for (; bit > 1;) {
        bit /= 2;
        product = (product * product) % m;
        if (bit <= mask) {
          mask -= bit;
          product = (n * product) % m;
        }
      }

      return product;
    }

    private static void validate(decimal n, decimal exp, decimal m) {
      string message = null;
      if (hasFraction(n))
        message = $"whole n = {n}";
      else if (hasFraction(exp))
        message = $"natural exp = {exp}";
      else if (hasFraction(m))
        message = $"whole m = {m}";
      else if (exp < 0)
        message = $"0 <= exp = {exp}";
      else if (m <= 1)
        message = $"1 < m = {m}";
      else if (n <= 1)
        message = $"1 < n = {n}";
      else if (m <= n)
        message = $"n = {n} < m = {m}";

      if (!string.IsNullOrEmpty(message))
        throw new ValidationException($"Invalid: {message}");
    }

    private static bool hasFraction(decimal number) {
      return number != decimal.Truncate(number);
    }
    #endregion
  }
}
