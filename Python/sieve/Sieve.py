# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-04  CNHume  Completed Incremental Generation
# 2015-05-04  CNHume  Created Prime Number Generator
from functools import partial
# from datetime import datetime
import time

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self):
    '''Incremental Prime Number Generator'''
    # Odd Composite Indexes such that n = 2 * index + 1 < limit
    self.sieveIndexes = set()           

    self.limitPrimes = []
    self.sievePrimes = []
    self.primeLimit = 1
    self.sieveLimit = 1

    self.oddIndex = 0
    self.odd = 1
    self.delta = 0
    self.squareIndex = 0
    self.square = 1
  
  def sift(self):
    '''Sift Composites less than limit'''
    # Test whether odd is Prime
    if self.odd > 1 and self.oddIndex not in self.sieveIndexes:
      #[Test]print('Appending {}'.format(self.odd))
      self.sievePrimes.append(self.odd)
      # Sift odd multiples of odd
      for oddIndex2 in range(self.squareIndex, self.sieveLimit // 2, self.odd):
        self.sieveIndexes.add(oddIndex2)

    self.oddIndex += 1
    self.odd += 2
    self.delta += 4
    self.squareIndex += self.delta

    # Note: square receives the value of odd squares: 1, 9, 25, 49, 81...
    # The difference between the nth odd square and its successor is 8*n
    # because odd x increase by 2 and (x + 2)**2 - x**2 = 4*x + 4
    self.square += self.delta + self.delta

  @staticmethod
  def nextMuliple(limit, p):
    '''Return next odd multiple of p greater than or equal to limit'''
    # Skip even multiples of p
    m = p + p
    # The next multiple is congruent delta mod m
    delta = p * p - limit
    next = limit + delta % m
    return p, next

  def raiseLimit(self, limit):
    boundNext = partial(self.nextMuliple, self.sieveLimit)
    self.sieveLimit = limit
    pairs = map(boundNext, self.sievePrimes)
    for p, next in pairs:
      for oddIndex2 in range(next // 2, limit // 2, p):
        self.sieveIndexes.add(oddIndex2)
    while self.square < limit:
      self.sift()
  
  def genPrimes(self, start, limit):
    '''Generate Primes less than limit'''
    if self.sieveLimit < limit:
      self.raiseLimit(limit)

    for oddIndex in range(start // 2, limit // 2):
      if oddIndex not in self.sieveIndexes:
        # Repurpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        yield 2 * oddIndex + 1 if oddIndex > 0 else 2

  def primes(self, limit):
    '''Return Primes less than limit'''
    if limit < self.primeLimit:
      return [p for p in self.limitPrimes if p < limit]
    
    if self.primeLimit < limit:
      for p in self.genPrimes(self.primeLimit, limit):
        self.limitPrimes.append(p)
      self.primeLimit = limit

    return self.limitPrimes

  def test(self, limit):
    '''Perform test case'''
    # start time
    elapsed_t0 = time.time()
    primes = self.primes(limit)
    # end time
    elapsed_t1 = time.time()
    elapsed_delta = elapsed_t1 - elapsed_t0
    print(u'{0:.3f} sec elapsed'.format(round(elapsed_delta, 3)))
    print('limit = {}'.format(limit))
    count = len(primes)
    if elapsed_delta > 0:
      rate = count / elapsed_delta
      scale = 1e3
      print(u'count = {0} at {1:.3f} KHz'.format(count, round(rate / scale, 3)))
    else:
      print(u'count = {0}'.format(count))
    # self.printList(primes)
    # print('total = {}'.format(sum(primes)))
    print('final = {}'.format(primes[-1]))
    print

  @staticmethod
  def printList(elements):
    for index, element in enumerate(elements):
      print('P[{0}] = {1}'.format(index, element))
