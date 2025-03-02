# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-09-21  CNHume  Created class

class Numeric:    
  @staticmethod
  def lubSquare(n):
    '''Least square that is an upper bound for n'''
    nextRoot = Numeric.isqrt(n - 1) + 1
    nextOdd = nextRoot | 1
    return nextOdd * nextOdd

  @staticmethod
  def isqrt(n):
    '''Integer square root, using Newton's method'''
    if n < 0:
      raise ValueError('n must not be negative')
    x = n
    y = (x + 1) // 2
    while y < x:
      x = y
      y = (x + n // x) // 2
    return x

  @staticmethod
  def sqrt(n):
    '''Square root, using Newton's method'''
    if n < 0:
      raise ValueError('n must not be negative')
    elif 0 < n and n < 1:
      return 1 / Numeric.sqrt1(1 / n)
    else:
      return Numeric.sqrt1(n)

  @staticmethod
  def sqrt1(n):
    x = n
    y = x / 2
    while y < x:
      x = y
      y = (x + n / x) / 2
    return x
