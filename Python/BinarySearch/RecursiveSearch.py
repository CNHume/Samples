# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
# 2018-03-21  CNHume  Created File

class RecursiveBinarySearch:
  '''Recursive Binary Search Class'''
  @staticmethod
  def glb(entries, value):
    return RecursiveBinarySearch.glbSlice(entries, value, 0, len(entries) - 1)

  @staticmethod
  def glbSlice(entries, value, left, right):
    if (left <= right):
      middle = (left + right) // 2
      if (entries[middle] < value):
        return RecursiveBinarySearch.glbSlice(entries, value, middle + 1, right)
      else:
        return RecursiveBinarySearch.glbSlice(entries, value, left, middle - 1)

    #[Assert]left == right + 1
    # glb: entries[right] < value and value <= entries[right + 1]
    return right

  @staticmethod
  def lub(entries, value):
    return RecursiveBinarySearch.lubSlice(entries, value, 0, len(entries) - 1)

  @staticmethod
  def lubSlice(entries, value, left, right):
    if (left <= right):
      middle = (left + right) // 2
      if (entries[middle] <= value):
        return RecursiveBinarySearch.lubSlice(entries, value, middle + 1, right)
      else:
        return RecursiveBinarySearch.lubSlice(entries, value, left, middle - 1)

    #[Assert]left == right + 1
    # lub: entries[left - 1] <= value and value < entries[left]
    return left
