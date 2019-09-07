# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2015-05-04  CNHume  Created File
from Sieve import Sieve
import sys

def main():
  if len(sys.argv) > 1:
    limit = int(sys.argv[1])
    if limit <= 0:
      raise ValueError('limit must be positive')

    sieve = Sieve()
    # sieve.ntest(78498)

    # 1 prime summing to 2
    # sieve.test(3)
    # 4 primes summing to 17
    sieve.test(11)
    # 9 primes summing to 100
    sieve.test(25)
    # 25 primes summing to 1,060
    sieve.test(100)
    # 303 primes summing to 277,050
    sieve.test(2000)
    # 78498 primes summing to 37,550,402,023
    # sieve.test(1000000)
    # 148933 primes summing to 142,913,828,922
    # sieve.test(2000000)

    # rate can be 1.66 MHz
    sieve.test(limit)

main()
pass
