# (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2017-07-27  CNHume  Converted Lisp version of 1988-06-14 to Python
import functools
from fractions import Fraction
from decimal import *


class ContinuedFraction:
  """Continued Fraction Class"""

  def __init__(self, fraction: Fraction):
    self.fraction = fraction
    self.tolerance = 0

  @classmethod
  def from_string(cls, fraction_as_string):
    fraction = Fraction(fraction_as_string)
    cf = cls(fraction)
    return cf

  @classmethod
  def from_decimal(cls, fraction_as_decimal: Decimal):
    fraction = Fraction(fraction_as_decimal)
    cf = cls(fraction)
    return cf

  @classmethod
  def from_float(cls, fraction_as_float: float):
    fraction = Fraction(fraction_as_float)
    cf = cls(fraction)
    return cf

  def __str__(self):
    terms = self.continued()
    strIT = map(str, terms)
    return ' '.join(strIT)

  def dump(self):
    print('fraction: {}\nterms: {}'.format(self.fraction, self))

  def coerce_ratio(self):
    """Represent any real valued input as a ratio"""
    return discontinued(self.continued())

  def continued(self):
    """Convert rational number to a continued fraction"""
    terms = []
    threshhold = abs(self.fraction * self.tolerance / 100)
    numerator = self.fraction.numerator
    denominator = self.fraction.denominator
    while True:
      quotient, remainder = divmod(numerator, denominator)
      terms.append(quotient)
      # Preclude division by zero
      if remainder == 0:
        break
      ratio = discontinued(terms)
      error = abs(self.fraction - ratio)
      if error <= threshhold:
        break
      numerator, denominator = denominator, remainder
    return terms


def discontinued(terms):
  """Convert continued fraction to a rational number"""
  return functools.reduce(reducer, reversed(terms), Fraction(0))


def reducer(reduction: Fraction, term) -> Fraction:
  fraction = Fraction(term)
  return fraction if reduction == 0 else fraction + 1 / reduction
