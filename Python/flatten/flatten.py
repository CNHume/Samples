# -*- coding: utf-8 -*-
# 2018-10-24 CNHume Created File

def main():
  print flatten([])
  print flatten([[0, 1, [4, 5]], [2, 3]])
  print flatten([0])
  print flatten([0, 1])
  print flatten([[0, 1]])
  print flatten([0, [0, 1]])
  print flatten([[0, 1], [], 2])
  print flatten([[0, 1], [2, 3]])
  print flatten([[0, 1, [4, 5], 6], [2, 3]])
  print flatten([[0, 1, [4, 5], 6], [[0, 1], 2, 3]])
  pass

def flatten(elements):
  return flatten2(elements)

def flatten1(elements):
  return [item for element in elements for item in flat_list(element)]

def flat_list(element):
  return flatten1(element) if isinstance(element, list) else [element]

def flatten2(elements):
  result = []
  for element in elements:
    if isinstance(element, list):
      result.extend(flatten2(element))
    else:
      result.append(element)
  return result

if __name__ == '__main__':
  main()
  pass
