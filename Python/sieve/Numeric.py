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
    x = n
    y = (x + 1) // 2
    while y < x:
      x = y
      y = (x + n // x) // 2
    return x
