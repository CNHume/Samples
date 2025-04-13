# Copyright (C) 2018, Christopher Hume.  All rights reserved.
# 2018-03-21  CNHume  Created File
"""Recursive Binary Search Class"""


def glb(entries, value):
  return glbSlice(entries, value, 0, len(entries) - 1)


def glbSlice(entries, value, left, right):
  if (left <= right):
    middle = (left + right) // 2
    if (entries[middle] < value):
      return glbSlice(entries, value, middle + 1, right)
    else:
      return glbSlice(entries, value, left, middle - 1)

  #[Assert]left == right + 1
  # glb: entries[right] < value and value <= entries[right + 1]
  return right


def lub(entries, value):
  return lubSlice(entries, value, 0, len(entries) - 1)


def lubSlice(entries, value, left, right):
  if (left <= right):
    middle = (left + right) // 2
    if (entries[middle] <= value):
      return lubSlice(entries, value, middle + 1, right)
    else:
      return lubSlice(entries, value, left, middle - 1)

  #[Assert]left == right + 1
  # lub: entries[left - 1] <= value and value < entries[left]
  return left
