# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher N. Hume
#
# Code Sample to solve a Sorting Propblem posed by Polysign, Inc.
#
# 2018-04-03  CNHume  Created File
#
from operator import itemgetter

def customSort(input):
    counts = {}
    for value in input:
        counts[value] = counts[value] + 1 if value in counts else 1

    #
    # Each pair is (k: input value, v: count)
    #
    pairs = [(k, v) for k, v in counts.items()]
    output = [pair[0] for pair in sorted(pairs, key=itemgetter(1, 0))]
    return output

def main():
    input = [1, 2, 3, 2, 3, 4, 5, 4, 4]
    output = customSort(input)
    print(output)

main()
