# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-10  CNHume  Completed test() method
# 2015-05-04  CNHume  Created Prime Number Generator
from functools import partial
from Perform import Perform

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self, debug=False):
    self.debug = debug
    self.sieveIndexes = set()
    # sievePrimes are used by extend() to find leastOddMuliple()
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

  def sift(self, lastLimit, nextLimit, p):
    '''Sift multiples of p'''
    for index in range(lastLimit // 2, nextLimit // 2, p):
      self.sieveIndexes.add(index)
  
  def leastOddMuliple(self, limit, p):
    '''Least odd multiple of p greater than or equal to limit'''
    # Skip even multiples of p
    m, square = p + p, p * p
    # The next multiple is congruent delta mod m
    delta = square - limit
    lub = limit + delta % m
    return p, lub

  def extend(self, limit):
    '''Extend Sieve of Composites using sievePrimes'''
    if self.lastLimit < limit:
      lom = partial(self.leastOddMuliple, self.lastLimit)
      self.lastLimit = limit
      pairs = map(lom, self.sievePrimes)
      for p, lub in pairs:
        self.sift(lub, limit, p)

  def sievePrime(self, p):
    '''Add new sievePrime'''
    if self.debug:
      print('sievePrimes.append({})'.format(p))
    self.sievePrimes.append(p)

  def testOdd(self):
    '''Test whether odd is Prime'''
    if self.oddIndex not in self.sieveIndexes:
      self.sievePrime(self.odd)

  def testOddAndSift(self, limit):
    '''Test whether odd is Prime and sift, if so'''
    if self.odd > 1 and self.oddIndex not in self.sieveIndexes:
      self.sievePrime(self.odd)
      self.sift(self.square, limit, self.odd)

  def expand(self, limit):
    '''Test whether odd is Prime and sift, while square < limit'''
    while self.square < limit:
      self.testOddAndSift(limit)
      self.nextSquare()

  def sifted(self, lastLimit, nextLimit):
    '''Generate sifted Primes'''
    for oddIndex in range(lastLimit // 2, nextLimit // 2):
      if oddIndex not in self.sieveIndexes:
        self.count += 1
        # Re-purpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime.
        p = 2 * oddIndex + 1 if oddIndex > 0 else 2
        yield p

  def primes(self, limit):
    '''Return Primes less than limit'''
    # Though the first odd number 1, is not a Prime,
    # it is used to represent the sole even prime 2.
    if limit == 2:
      limit = 1
    lastLimit = self.lastLimit
    self.extend(limit)
    self.expand(limit)
    for p in self.sifted(lastLimit, limit):
      self.siftedPrimes.append(p)
    return [p for p in self.siftedPrimes if p < limit] if limit < lastLimit else self.siftedPrimes

  # rate can be 150 KHz
  def nPrimes2(self, n):
    '''Return the first n Primes'''
    if n <= self.count:
      return self.siftedPrimes[:n]

    while True:
      limit, lastLimit = 4 * self.lastLimit, self.lastLimit
      self.extend(limit)
      self.expand(limit)
      for p in self.sifted(lastLimit, limit):
        self.siftedPrimes.append(p)
        if n <= self.count:
          return self.siftedPrimes

  # rate can be 120 KHz
  def nPrimes(self, n):
    '''Return the first n Primes'''
    if n <= self.count:
      return self.siftedPrimes[:n]

    while True:
      self.nextSquare()
      limit, lastLimit = self.square, self.lastLimit
      self.extend(limit)
      self.testOdd()
      for p in self.sifted(lastLimit, limit):
        self.siftedPrimes.append(p)
        if n <= self.count:
          return self.siftedPrimes

  def nextPrime(self, clear=False):
    '''Generate next Prime'''
    while True:
      self.nextSquare()
      limit, lastLimit = self.square, self.lastLimit
      self.extend(limit)
      self.testOdd()
      for p in self.sifted(lastLimit, limit):
        yield p
      #[Note]Freeing memory may reduce speed by 37.5%
      if clear:
        self.sieveIndexes.clear()

  def nprimes(self, n):
    '''List of the first n Primes'''
    return list(self.nPrimes(n))
  
  def ntest(self, n):
    Perform.testFun(self.nprimes, n)

  def test(self, n):
    Perform.testFun(self.primes, n)
