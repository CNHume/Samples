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

    sieve = Sieve(limit)
    printIt(sieve.primes())
    print('sum = {}'.format(sum(sieve.primes())))

main()
pass
