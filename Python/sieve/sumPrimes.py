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

    sieve = Sieve(True)

    # sieve.test(3)
    sieve.test(11)
    # 9 primes summing to 100
    sieve.test(25)
    # 25 primes summing to 1060
    sieve.test(100)
    # 303 primes summing to 277050
    sieve.test(2000)

    # sieve.test(limit)

main()
pass
