# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2015-05-04  CNHume  Created File
class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self):
    '''Obtain indexes for the odd composites such that n = 2 * index + 1 < limit'''
    self.limit = 1
    self.oddIndex = 0
    self.odd = 1
    self.delta = 0
    self.squareIndex = 0
    self.square = 1

    self.sieve = set()
    self.siftPrimes = []
    self.listPrimes = []
  
  def sift(self):
    '''Sift Composites less than limit'''
    self.oddIndex += 1
    self.odd += 2
    self.delta += 4
    self.squareIndex += self.delta

    # Note: square receives the value of odd squares: 1, 9, 25, 49, 81...
    # The difference between the nth odd square and its successor is 8*n
    # because odd x increase by 2 and (x + 2)**2 - x**2 = 4*x + 4
    self.square += self.delta + self.delta

    # Test whether odd is Prime
    if self.oddIndex not in self.sieve:
      p = 2 * self.oddIndex + 1
      print('Appending {}'.format(p))
      self.siftPrimes.append(p)
      # Sift odd multiples of odd
      for oddIndex2 in range(self.squareIndex, self.limit // 2, self.odd):
        self.sieve.add(oddIndex2)

  def rangeStart(self, p):
    limit = self.limit
    pp = p * p
    if pp < limit:
      p2 = p + p
      delta = limit - pp
      r = delta % p2
      start = limit + p2 - r
    else:
       start = pp
    return p, start

  def raiseLimit(self, limit):
    pairs = map(self.rangeStart, self.siftPrimes)
    for p, start in pairs:
      for oddIndex2 in range(start // 2, limit // 2, p):
        self.sieve.add(oddIndex2)
    self.limit = limit

  def genPrimes(self, limit):
    '''Generate Primes less than limit'''
    if self.limit < limit:
      self.listPrimes = []
      self.raiseLimit(limit)

    while self.square < self.limit:
      self.sift()

    for oddIndex in range(self.limit // 2):
      if oddIndex not in self.sieve:
        # Repurpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        yield 2 * oddIndex + 1 if oddIndex > 0 else 2

  def primes(self, limit):
    '''Return Primes less than limit'''
    if self.limit < limit:
      self.listPrimes = list(self.genPrimes(limit))
    return self.listPrimes
