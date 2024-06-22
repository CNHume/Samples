//
// Copyright (C) 2020, Christopher N. Hume.  All rights reserved.
//
// You should have received a copy of the MIT License along with this program.
// If not, see https://opensource.org/licenses/MIT.
//
using Fermat.Exceptions;

using System.Diagnostics;

namespace Fermat.Maths {
  public static class Modular {
    #region Methods
    // The following is based on Fermat's Little Theorem
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
        Debug.Assert(IsInteger(quotient), $"Non-integral quotient = {quotient}");

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
    /// Returns n^exp mod m
    /// </summary>
    /// <remarks>Implelements modular exponentiation</remarks>
    /// <param name="n">the base</param>
    /// <param name="exp">the exponent</param>
    /// <param name="m">the modulus</param>
    /// <returns></returns>
    public static decimal ModPower(decimal n, decimal exp, decimal m) {
      validate(n, exp, m);
      n %= m;                           //[Safe]Reduce chance of overflow

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
      string? message = default;
      //
      //[Note]The modulus m must not be greater than 2^48 = 281,474,976,710,656 = 2.8e14
      // because overflow will occur if the square of any n mod m exceeds
      // decimal.MaxValue = 2^96 - 1 = 79,228,162,514,264,337,593,543,950,335 = 7.9e28
      //
      var limit = 281474976710656m;
      //[Test]var overflow = limit * limit;

      if (!IsInteger(n))
        message = $"whole n = {n}";
      else if (!IsInteger(exp))
        message = $"natural exp = {exp}";
      else if (!IsInteger(m))
        message = $"whole m = {m}";
      else if (exp < 0)
        message = $"0 <= exp = {exp}";
      else if (m <= 1)
        message = $"1 < m = {m}";
      else if (n <= 1)
        message = $"1 < n = {n}";
      else if (m <= n)
        message = $"n = {n} < m = {m}";
      else if (limit < m)
        message = $"limit = {limit} < m = {m}";

      if (!string.IsNullOrEmpty(message))
        throw new ValidationException($"Invalid: {message}");
    }

    public static bool IsInteger(decimal n) {
      return n == decimal.Truncate(n);
    }
    #endregion
  }
}
