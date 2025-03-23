# Copyright (C) 2019, Christopher Hume.  All rights reserved.
# 2019-09-24  CNHume  Created class
import time

def printList(elements):
  """Print enumeration of elements"""
  for index, element in enumerate(elements):
    print('P[{0}] = {1}'.format(index, element))

def testFun(fun, n):
  """Test and return scalar result"""
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

def testList(fun, n):
  """Test and summarize list result"""
  results = testFun(fun, n)
  count = len(results)
  print('count = {}'.format(count))
  if results:
    print('final = {}'.format(results[-1]))
  # printList(results)
  print('total = {}'.format(sum(results)))
  return results

def testLast(fun, n, m):
  """Test and summarize list result"""
  results = testFun(fun, n)
  count = len(results)
  print('count = {}'.format(count))
  if results:
    last = results[-m:]
    printList(last[::-1])
  return results
