# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2015-05-04  CNHume  Created File
from Sieve import Sieve
import sys

def printIt(it):
  for index, element in enumerate(it):
    print('P[{0}] = {1}'.format(index, element))

def main():
  if len(sys.argv) > 1:
    limit = int(sys.argv[1])
    if limit <= 0:
      raise ValueError('limit must be positive')

    sieve = Sieve()
    printIt(sieve.primes(3))
    print('sum = {}'.format(sum(sieve.primes(3))))
    printIt(sieve.primes(11))
    print('sum = {}'.format(sum(sieve.primes(11))))
    printIt(sieve.primes(26))
    print('sum = {}'.format(sum(sieve.primes(26))))

    # 24 primes summing to 1060
    printIt(sieve.primes(100))
    print('sum = {}'.format(sum(sieve.primes(100))))
    # 302 primes summing to 277050
    printIt(sieve.primes(limit))
    print('sum = {}'.format(sum(sieve.primes(limit))))

main()
pass
