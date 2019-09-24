# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-10  CNHume  Completed test() method
# 2015-05-04  CNHume  Created Prime Number Generator
from functools import partial
import time

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self, debug=False):
    self.debug = debug
    self.sieveIndexes = set()
    # sievePrimes are used by extend() to find nextMuliple()
    self.sievePrimes = []
    self.siftedPrimes = []
    self.count = 0
    self.oddIndex = 0
    self.odd = 1
    self.delta = 0
    self.square = 1
    self.lastLimit = 1

  def nextSquare(self):
    '''Advance to next square'''
    self.oddIndex += 1
    self.odd += 2
    self.delta += 4
    # Note: square receives the value of odd squares: 1, 9, 25, 49, 81...
    # The difference between the nth odd square and its successor is 8*n
    # because odd n increase by 2 and (n + 2)**2 - n**2 = 4*n + 4
    self.square += self.delta + self.delta
    if self.debug:
      print('square = {}'.format(self.square))

  def expand(self, lastLimit, nextLimit, p):
    '''Expand Sieve of Composites | lastLimit < n = 2 * index + 1 < nextLimit'''
    for index in range(lastLimit // 2, nextLimit // 2, p):
      self.sieveIndexes.add(index)
  
  def leastOddMuliple(self, limit, p):
    '''Least odd multiple of p greater than or equal to limit'''
    # Skip even multiples of p
    m = p + p
    # The next multiple is congruent delta mod m
    delta = p * p - limit
    lub = limit + delta % m
    return p, lub

  def extend(self, limit):
    '''Extend Sieve of Composites using sievePrimes'''
    if self.lastLimit < limit:
      lom = partial(self.leastOddMuliple, self.lastLimit)
      self.lastLimit = limit
      pairs = map(lom, self.sievePrimes)
      for p, lub in pairs:
        self.expand(lub, limit, p)

  def sievePrime(self, p):
    if self.debug:
      print('sievePrimes.append({})'.format(p))
    self.sievePrimes.append(p)

  def sifted(self, lastLimit, nextLimit):
    '''Generate sifted Primes'''
    for oddIndex in range(lastLimit // 2, nextLimit // 2):
      if oddIndex in self.sieveIndexes:
        #[Note]Freeing memory may reduce speed by 15%
        # self.sieveIndexes.remove(oddIndex)
        pass
      else:
        self.count += 1
        # Re-purpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        p = 2 * oddIndex + 1 if oddIndex > 0 else 2
        yield p

  def testOddAndExpand(self, nextSquare):
    '''Test whether odd is Prime and expand the Sieve'''
    if self.odd > 1 and self.oddIndex not in self.sieveIndexes:
      self.sievePrime(self.odd)
      self.expand(self.square, nextSquare, self.odd)

  def loopLimit(self, limit):
    '''Test whether odd is Prime and expand while square < limit'''
    while self.square < limit:
      self.testOddAndExpand(limit)
      self.nextSquare()

  def primes(self, limit):
    '''Return Primes less than limit'''
    if limit == 2:
      limit = 1
    lastLimit = self.lastLimit
    self.extend(limit)
    self.loopLimit(limit)
    for p in self.sifted(lastLimit, limit):
      self.siftedPrimes.append(p)
    return [p for p in self.siftedPrimes if p < limit] if limit < lastLimit else self.siftedPrimes

  def testOdd(self):
    '''Test whether odd is Prime'''
    if self.oddIndex not in self.sieveIndexes:
      self.sievePrime(self.odd)

  def nextPrime(self):
    '''Generate next Prime'''
    while True:
      self.nextSquare()
      lastLimit = self.lastLimit
      self.extend(self.square)
      self.testOdd()
      for p in self.sifted(lastLimit, self.square):
        yield p

  def genPrimes(self, n):
    '''Generate the first n Primes'''
    primes = self.nextPrime()
    while self.count < n:
      yield primes.next()

  def nprimes(self, n):
    '''List of the first n Primes'''
    return list(self.genPrimes(n))

  @staticmethod
  def printList(elements):
    '''Print enumeration of elements'''
    for index, element in enumerate(elements):
      print('P[{0}] = {1}'.format(index, element))

  def testFun(self, fun, n):
    '''Perform test case'''
    # start time
    elapsed_t0 = time.time()
    primes = fun(n)
    # end time
    elapsed_t1 = time.time()
    elapsed_delta = elapsed_t1 - elapsed_t0
    print('{0:.3f} sec elapsed'.format(round(elapsed_delta, 3)))
    if elapsed_delta > 0:
      rate = n / elapsed_delta
      rounded = round(rate / 1e3, 3)
      print('limit = {0} at {1:.3f} KHz'.format(n, rounded))
    else:
      print('limit = {0}'.format(n))
    count = len(primes)
    print('count = {}'.format(count))
    if primes:
      print('final = {}'.format(primes[-1]))
    # self.printList(primes)
    print('total = {}'.format(sum(primes)))
    print

  def test(self, n):
    self.testFun(self.primes, n)
  
  def ntest(self, n):
    self.testFun(self.nprimes, n)
  