# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-08-10  CNHume  Completed test() method
# 2015-05-04  CNHume  Created Prime Number Generator
from Perform import Perform

class Sieve:
  '''Sieve of Eratosthenes'''
  def __init__(self, debug=False):
    self.debug = debug
    self.sieveIndexes = set()
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
    # The difference between the square of the nth odd and its successor is
    # 8*(n + 1) where odd = 2*n + 1 because (odd + 2)**2 - odd**2 = 4*odd + 4
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
    return lub

  def extendNext(self, limit):
    '''Extend Sieve of Composites using sievePrimes'''
    if self.lastLimit < limit:
      for p in self.sievePrimes:
        lub = self.leastOddMuliple(self.lastLimit, p)
        self.sift(lub, limit, p)
      self.lastLimit = limit

  def extend(self, limit):
    '''Extend Sieve of Composites using siftedPrimes, while p < odd'''
    if self.lastLimit < limit:
      for p in self.siftedPrimes:
        if p > 2:
          if not p < self.odd:
            break
          lub = self.leastOddMuliple(self.lastLimit, p)
          self.sift(lub, limit, p)
      self.lastLimit = limit

  def expandNext(self, limit):
    '''Test whether odd is Prime and sift, appending sievePrimes'''
    if self.oddIndex not in self.sieveIndexes:
      self.sift(self.square, limit, self.odd)
      self.sievePrimes.append(self.odd)

  def expand(self, limit):
    '''Test whether odd is Prime and sift, while square < limit'''
    while self.square < limit:
      if self.oddIndex not in self.sieveIndexes and self.odd > 1:
        self.sift(self.square, limit, self.odd)
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

  # Rate can be 2.9 MHz
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

  # rate can be 280 KHz
  def nPrimes(self, n):
    '''Return the first n Primes'''
    if not self.count < n:
      return self.siftedPrimes[:n]

    while True:
      self.nextSquare()
      lastLimit, limit = self.lastLimit, self.square
      self.extend(limit)
      # self.expand(limit)
      for p in self.sifted(lastLimit, limit):
        self.siftedPrimes.append(p)
        if not self.count < n:
          return self.siftedPrimes

  # rate can be 252 KHz
  def nextPrime(self, clear=False):
    '''Generate next Prime'''
    while True:
      self.nextSquare()
      lastLimit, limit = self.lastLimit, self.square
      self.extendNext(limit)
      self.expandNext(limit)
      for p in self.sifted(lastLimit, limit):
        yield p
      if clear:
        self.sieveIndexes.clear()

  def genPrimes(self, n, clear=False):
    '''Generate the first n Primes'''
    primes = self.nextPrime(clear)
    while self.count < n:
      yield primes.next()

  def nprimes(self, n):
    '''List of the first n Primes'''
    return list(self.nPrimes(n))
  
  def ntest(self, n):
    return Perform.testList(self.nprimes, n)

  def test(self, n):
    return Perform.testList(self.primes, n)
