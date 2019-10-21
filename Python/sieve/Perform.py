
# -*- coding: utf-8 -*-
# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-09-24  CNHume  Created class
import time

class Perform:
  @staticmethod
  def printList(elements):
    '''Print enumeration of elements'''
    for index, element in enumerate(elements):
      print('P[{0}] = {1}'.format(index, element))

  @staticmethod
  def testFun(fun, n):
    '''Test and return scalar result'''
    # start time
    elapsed_t0 = time.time()
    result = fun(n)
    # end time
    elapsed_t1 = time.time()
    elapsed_delta = elapsed_t1 - elapsed_t0
    print
    print('{0:.3f} sec elapsed'.format(round(elapsed_delta, 3)))
    if elapsed_delta > 0:
      rate = n / elapsed_delta
      rounded = round(rate / 1e3, 3)
      print('limit = {0} at {1:.3f} KHz'.format(n, rounded))
    else:
      print('limit = {0}'.format(n))
    return result

  @staticmethod
  def testList(fun, n):
    '''Test and summarize list result'''
    list = Perform.testFun(fun, n)
    count = len(list)
    print('count = {}'.format(count))
    if list:
      print('final = {}'.format(list[-1]))
    # Perform.printList(list)
    print('total = {}'.format(sum(list)))
    return list
