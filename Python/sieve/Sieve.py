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
    self.squarePrimes = []
    self.count = 0
    self.oddIndex = 0
    self.odd = 1
    self.delta = 0
    self.square = 1

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
  
  def leastMuliple(self, limit, p):
    '''Least odd multiple of p greater than or equal to limit'''
    # Skip even multiples of p
    m = p + p
    # The next multiple is congruent delta mod m
    delta = p * p - limit
    next = limit + delta % m
    return p, next

  def extend(self, lastLimit, nextLimit):
    '''Sift odd composites | lastLimit < n = 2 * index + 1 < nextLimit'''
    if lastLimit < nextLimit:
      multiple = partial(self.leastMuliple, lastLimit)
      pairs = map(multiple, self.sievePrimes)
      for p, next in pairs:
        for index in range(next // 2, nextLimit // 2, p):
          self.sieveIndexes.add(index)
    
  def expand(self, limit):
    '''Expand sieve to next limit'''
    # Test whether odd is Prime
    if self.odd > 1 and self.oddIndex not in self.sieveIndexes:
      if self.debug:
        print('sievePrimes.append({})'.format(self.odd))
      self.sievePrimes.append(self.odd)
      for index in range(self.square // 2, limit // 2, self.odd):
        self.sieveIndexes.add(index)

  def sifted(self, lastLimit, nextLimit):
    '''Generate sifted Primes'''
    for oddIndex in range(lastLimit // 2, nextLimit // 2):
      if oddIndex not in self.sieveIndexes:
        self.count += 1
        # Re-purpose the index corresponding to 1 to represent 2 instead,
        # replacing the multiplicative identity with the sole even Prime
        p = 2 * oddIndex + 1 if oddIndex > 0 else 2
        yield p

  def primes(self, limit):
    '''Return Primes less than limit'''
    lastSquare = self.square
    nextSquare = Sieve.lubSquare(limit)
    self.extend(lastSquare, nextSquare)
    while self.square < nextSquare:
      self.expand(nextSquare)
      self.nextSquare()
    for p in self.sifted(lastSquare, nextSquare):
      self.squarePrimes.append(p)
    return [p for p in self.squarePrimes if p < limit] if limit < nextSquare else self.squarePrimes

  def nextPrime(self):
    '''Generate next Prime'''
    while True:
      lastSquare = self.square
      self.nextSquare()
      nextSquare = self.square
      self.extend(lastSquare, nextSquare)
      self.expand(nextSquare)
      for p in self.sifted(lastSquare, nextSquare):
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
  def lubSquare(n):
    '''Least square that is an upper bound for n'''
    nextRoot = Sieve.isqrt(n - 1) + 1
    nextOdd = nextRoot | 1
    return nextOdd * nextOdd

  @staticmethod
  def isqrt(n):
    '''Integer square root, using Newton's method'''
    x = n
    y = (x + 1) // 2
    while y < x:
      x = y
      y = (x + n // x) // 2
    return x

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
  