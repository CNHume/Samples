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
      var totient = cmd.Totient.Value;
      var modulus = cmd.Modulus.Value;

      //[Note]totient is assumed equal to totient(modulus)
      //[Test]Console.WriteLine($"input = {input}, totient = {totient}, modulus = {modulus}");

      var encoder = 29m;                // Must be relatively prime to totient
      var inverse = ModInverse(encoder, totient);

      Console.WriteLine($"encoder = {encoder}");
      Console.WriteLine($"inverse = {inverse}");

      var product = encoder * inverse % totient;
      Console.WriteLine($"encoder * inverse % totient = {product}");

      var encoded = ModPower(input, encoder, modulus);
      var decoded = ModPower(encoded, inverse, modulus);

      Console.WriteLine($"encoded = {encoded}");
      Console.WriteLine($"decoded = {decoded}");

      var formattedId = Parser.FormatNavigatorId(encoded);
      var navigatorId = Parser.ParseNavigatorID(formattedId);

      Console.WriteLine($"{navigatorId} = {formattedId}");
    }

    /// <summary>
    /// Returns the multiplicative inverse of n mod m
    /// </summary>
    /// <param name="n"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    public static decimal ModInverse(decimal n, decimal m) {
      var inverse = 0m;
      var next = 1m;
      var numerator = m;
      var denominator = n;

      while (denominator != 0) {
        var remainder = numerator % denominator;
        var quotient = (numerator - remainder) / denominator;
        Debug.Assert(!hasFraction(quotient), $"Non-integral quotient = {quotient}");

        var last = inverse;
        inverse = next;
        next = modulo(last - inverse * quotient, m);

        numerator = denominator;
        denominator = remainder;
      }

      return inverse;
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

    private static bool hasFraction(decimal n) {
      return n != decimal.Truncate(n);
    }
    #endregion
  }
}
