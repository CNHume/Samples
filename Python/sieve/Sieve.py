# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-10  CNHume  Completed test() method
# 2019-08-04  CNHume  Completed Incremental Generation
# 2015-05-04  CNHume  Created Prime Number Generator
import time

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self):
    self.sieveIndexes = set()           

    # sievePrimes are used in raiseLimit() to find nextMuliple()
    self.sievePrimes = []
    self.limitPrimes = []
    self.sieveLimit = 1

    self.oddIndex = 0
    self.odd = 1
    self.odd2 = 1
    self.delta = 0
    self.squareIndex = 0
    self.square = 1
  
  def sift(self):
    '''Sift Odd Composite Indexes where n = 2 * index + 1 < limit'''
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

  def nextMuliple(self, p):
    '''Return next odd multiple of p greater than or equal to limit'''
    limit = self.sieveLimit
    # Skip even multiples of p
    m = p + p
    # The next multiple is congruent delta mod m
    delta = p * p - limit
    next = limit + delta % m
    return p, next

  def raiseLimit(self, limit):
    #[Note]nextMuliple() uses prior value of sieveLimit
    pairs = map(self.nextMuliple, self.sievePrimes)
    for p, next in pairs:
      for oddIndex2 in range(next // 2, limit // 2, p):
        self.sieveIndexes.add(oddIndex2)
    self.sieveLimit = limit
  
  def siftPrimes(self, limit, lastLimit):
    '''Sift Primes less than the limit'''
    for oddIndex in range(lastLimit // 2, limit // 2):
      self.odd2 = 2 * oddIndex + 1
      if oddIndex not in self.sieveIndexes:
        # Re-purpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        p = self.odd2 if oddIndex > 0 else 2
        yield p

  def genPrimes(self):
    '''Generate Primes, incrementally'''
    if self.square <= self.odd2 + 2:
      self.lastLimit = self.square
      self.sift()
      #[Test]
      print('Raising limit from {} to {}'.format(self.lastLimit, self.square))
      self.raiseLimit(self.square)

    return self.siftPrimes(self.square, self.lastLimit)

  def primes(self, limit):
    '''Return Primes less than limit'''
    while self.sieveLimit < limit:
      for p in self.genPrimes():
        self.limitPrimes.append(p)
    return [p for p in self.limitPrimes if p < limit] if limit < self.sieveLimit else self.limitPrimes

  def test(self, limit):
    '''Perform test case'''
    # start time
    elapsed_t0 = time.time()
    primes = self.primes(limit)
    # gen = self.genPrimes(limit)
    # result = gen.next()
    # # end time
    elapsed_t1 = time.time()
    elapsed_delta = elapsed_t1 - elapsed_t0
    print('{0:.3f} sec elapsed'.format(round(elapsed_delta, 3)))
    if elapsed_delta > 0:
      rate = limit / elapsed_delta
      rounded = round(rate / 1e3, 3)
      print('limit = {0} at {1:.3f} KHz'.format(limit, rounded))
    else:
      print('limit = {0}'.format(limit))
    count = len(primes)
    print('count = {}'.format(count))
    print('final = {}'.format(primes[-1]))
    self.printList(primes)
    print('total = {}'.format(sum(primes)))
    print

  @staticmethod
  def printList(elements):
    for index, element in enumerate(elements):
      print('P[{0}] = {1}'.format(index, element))
