#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Continued Fraction Tests"""

import sys
import math
from fractions import Fraction
from decimal import *

from ContinuedFraction import ContinuedFraction

if len(sys.argv) > 0:
  inputs = sys.argv
  verb = inputs.pop(0)

  #phi = (1 + math.sqrt(5.0)) / 2
  #cf = ContinuedFraction.from_float(phi)

  #fraction = Fraction.from_float(3.14159)
  #fraction = Fraction("3.14159")
  #cf = ContinuedFraction(fraction)

  if len(inputs) > 0:
    #pi = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
    input = inputs[0]

    cf = ContinuedFraction.from_string(input)
    cf.tolerance = 1e-14
    cf.dump()
  
    ratio = cf.coerce_ratio()
    print("ratio: {0}".format(ratio))
