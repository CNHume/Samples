# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-10-19  CNHume  Created File
from fractions import Fraction
from Perform import Perform

class Poly:
  # The following method is based on Omri Barel's answer to "How to efficiently calculate a row in pascal's triangle?"
  # See https://stackoverflow.com/questions/15580291/how-to-efficiently-calculate-a-row-in-pascals-triangle
  @staticmethod
  def pascal(n):
    c = 1
    yield c
    #[Note]The kth coefficient is used to generate the k + 1st
    for k in range(n):
      c = c * (n - k) // (k + 1)
      yield c

  @staticmethod
  def isPrime(n):
    m = n // 2
    c = 1
    # print c
    for k in range(m):
      num = n - k
      den = k + 1
      c = c * num // den
      # print c
      if c % n:
        return False
    return True
  
  @staticmethod
  def isPrimeTest(n):
    return Perform.testFun(Poly.isPrime, n)
