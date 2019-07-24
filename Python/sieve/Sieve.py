# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2015-05-04  CNHume  Created File
class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self, limit):
    '''Obtain indexes for the odd composites such that n = 2 * index + 1 < limit'''
    self.limit = limit
    self.limit2 = self.limit // 2
    self.sieve = set()

    if limit > 1:
      oddIndex = 0
      odd = 1
      delta = 0
      squareIndex = 0
      # Note: square receives the value of odd squares: 1, 9, 25, 49, 81...
      # The difference between the nth odd square and its successor is 8*n
      # because odd x increase by 2 and (x + 2)**2 - x**2 = 4*x + 4
      square = 1

      while square < limit:
        oddIndex += 1
        odd += 2
        delta += 4
        delta2 = delta + delta

        squareIndex += delta
        square += delta2
        # Test whether odd is Prime
        if oddIndex not in self.sieve:
          # Sift odd multiples of odd
          for oddIndex2 in range(squareIndex, self.limit2, odd):
            self.sieve.add(oddIndex2)

  def primes(self):
    '''Generates Primes less than limit'''
    if self.limit > 1:
      for oddIndex in range(0, self.limit2):
        if oddIndex not in self.sieve:
          # Repurpose the index corresponding to 1 to represent 2 instead,
	        # replacing the multiplicative identity with the sole even Prime
          yield 2 * oddIndex + 1 if oddIndex > 0 else 2
