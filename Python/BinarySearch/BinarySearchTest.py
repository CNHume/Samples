# Copyright (C) 2018, Christopher Hume.  All rights reserved.
# 2018-03-21  CNHume  Created File

import sys
import IterativeSearch
import RecursiveSearch


def main():
  if len(sys.argv) < 2:
    return

  value = int(sys.argv[1])
  print('value = {0}'.format(value))

  tests = [
      [],
      [2],
      [2, 2],
      [2, 2, 2, 2],
      [3, 3, 4, 4],
      [0, 1, 3, 3, 4, 4],
      [0, 1, 2, 2, 2, 3, 3, 4, 4],
      [0, 1, 1, 2, 2, 2, 3, 3, 4, 4],
      [0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4],
      [0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4],
      [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4],
  ]

  delimiter = ', '
  index = 0
  for test in tests:
    join = delimiter.join(str(n) for n in test)
    print('test[{0}]: {1}'.format(index, join))

    # glb = RecursiveSearch.glb(test, value)
    # lub = RecursiveSearch.lub(test, value)

    glb = IterativeSearch.glb(test, value)
    lub = IterativeSearch.lub(test, value)

    print('glb = {0}'.format(glb))
    print('lub = {0}'.format(lub))
    index += 1


if __name__ == '__main__':
  main()
  pass
