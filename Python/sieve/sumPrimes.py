# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2015-05-04  CNHume  Created File
import sys
from Sieve import Sieve
from Polynomial import Poly
from Perform import Perform

def main():
  # 411.5 sec using integers
  n = 999983
  n = 113

  isPrime = Poly.isPrimeTest(n)
  print('isPrime({}) = {}'.format(n, isPrime))
  # return

  if len(sys.argv) > 1:
    limit = int(sys.argv[1])
    if limit <= 0:
      raise ValueError('limit must be positive')

    sieve = Sieve()
    # sieve.ntest(9)
    # return

    # 1 prime summing to 2
    # sieve.test(3)
    # 4 primes summing to 17
    sieve.test(11)
    # 9 primes summing to 100
    sieve.test(25)
    # 25 primes summing to 1,060
    # sieve.test(100)
    # 303 primes summing to 277,050
    sieve.test(2000)
    # 78,498 primes summing to 37,550,402,023
    sieve.test(78498)
    # 148,933 primes summing to 142,913,828,922
    # sieve.test(2000000)

    # sieve.test(10000000)
    # 664,579 summing to 3,203,324,994,356

    # 5,761,455 summing to 279,209,790,387,276
    # sieve.test(100000000)

    # rate can be 2 MHz
    sieve.test(limit)

main()
pass
