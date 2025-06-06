# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-10-19  CNHume  Created File
# from fractions import Fraction
from perform import *

# The following method is based on Omri Barel's answer to "How to efficiently calculate a row in pascal's triangle?"
# See https://stackoverflow.com/questions/15580291/how-to-efficiently-calculate-a-row-in-pascals-triangle
def pascal(n):
  """Generate coefficients for the nth row of Pascal's Triangle"""
  c = 1
  yield c
  #[Note]The kth coefficient is used to generate the k + 1st
  for k in range(n):
    num = n - k
    den = k + 1
    c = c * num // den
    yield c

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

def pascalTest(n):
  return testList(lambda m: list(pascal(m)), n)

def isPrimeTest(n):
  return testFun(isPrime, n)
