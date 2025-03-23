#!/usr/bin/env python
# (C) Copyright 2018, Christopher N. Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
import sys
import math
from fractions import Fraction
from decimal import *

from continued_fraction import *


def main():
  """Continued Fraction Tests"""
  if len(sys.argv) > 0:
    inputs = sys.argv
    verb = inputs.pop(0)

    #phi = (1 + math.sqrt(5.0)) / 2
    #cf = ContinuedFraction.from_float(phi)

    #fraction = Fraction.from_float(3.14159)
    # fraction = Fraction('3.14159')
    # cf = ContinuedFraction(fraction)
    # terms = cf.continued()
    # fraction = discontinued(terms)

    if len(inputs) > 0:
      #pi = '3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679'
      input = inputs[0]

      cf = ContinuedFraction.from_string(input)
      cf.tolerance = 1e-14
      cf.dump()

      ratio = cf.coerce_ratio()
      print('ratio: {}'.format(ratio))


if __name__ == '__main__':
  main()
  pass
