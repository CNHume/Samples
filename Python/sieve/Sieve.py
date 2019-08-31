# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-30  CNHume  Renamed global variables, refactoring their use.
# 2019-08-25  CNHume  Added siftPrimes().  Refactored nextLimit(), nextPrimes() and primes()
# 2019-08-10  CNHume  Completed test() method
# 2019-08-04  CNHume  Completed Incremental Generation
# 2015-05-04  CNHume  Created Prime Number Generator
import time

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self, debug=False):
    self.debug = debug
    self.sieveIndexes = set()           

    # sievePrimes are used in addComposites() to find nextMuliple()
    self.sievePrimes = []
    self.squarePrimes = []
    self.lastSquare = None

    self.oddIndex = 0
    self.odd = 1
    self.oddSifted = 1
    self.delta = 0
    self.squareIndex = 0
    self.square = 1
  
  def nextMuliple(self, p):
    '''Return next odd multiple of p greater than or equal to lastSquare'''
    # Skip even multiples of p
    m = p + p
    # The next multiple is congruent delta mod m
    delta = p * p - self.lastSquare
    multiple = self.lastSquare + delta % m
    return p, multiple

  def addComposites(self):
    '''Add indexes for odd composites | lastSquare < n = 2 * index + 1 < square'''
    pairs = map(self.nextMuliple, self.sievePrimes)
    for p, multiple in pairs:
      for index in range(multiple // 2, self.square // 2, p):
        self.sieveIndexes.add(index)
    
  def nextSquare(self):
    # Test whether odd is Prime
    if self.odd > 1 and self.oddIndex not in self.sieveIndexes:
      if self.debug:
        print('sievePrimes.append({})'.format(self.odd))
      self.sievePrimes.append(self.odd)

    self.oddIndex += 1
    self.odd += 2
    self.delta += 4
    self.squareIndex += self.delta

    # Note: square receives the value of odd squares: 1, 9, 25, 49, 81...
    # The difference between the nth odd square and its successor is 8*n
    # because odd n increase by 2 and (n + 2)**2 - n**2 = 4*n + 4
    self.lastSquare = self.square
    self.square += self.delta + self.delta
    if self.debug:
      print('square = {}'.format(self.square))

    self.addComposites()

  def siftPrimes(self, lastLimit, nextLimit):
    '''Sift next set of Primes'''
    for oddIndex in range(lastLimit // 2, nextLimit // 2):
      self.oddSifted = 2 * oddIndex + 1
      if oddIndex not in self.sieveIndexes:
        # Re-purpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        p = self.oddSifted if oddIndex > 0 else 2
        yield p

  def nextPrime(self, limit):
    '''Generate Primes, incrementally'''
    while self.square < limit:
      oddNext = self.oddSifted + 2
      if not oddNext < self.square:
        self.nextSquare()
      for p in self.siftPrimes(self.lastSquare, self.square):
        yield p

  def primes(self, limit):
    '''Return Primes less than limit'''
    for p in self.nextPrime(limit):
      self.squarePrimes.append(p)
    return [p for p in self.squarePrimes if p < limit] if limit < self.square else self.squarePrimes

  def test(self, limit):
    '''Perform test case'''
    # start time
    elapsed_t0 = time.time()
    primes = self.primes(limit)
    # end time
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
    if primes:
      print('final = {}'.format(primes[-1]))
    # self.printList(primes)
    print('total = {}'.format(sum(primes)))
    print

  @staticmethod
  def printList(elements):
    for index, element in enumerate(elements):
      print('P[{0}] = {1}'.format(index, element))
