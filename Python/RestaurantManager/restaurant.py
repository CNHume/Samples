# -*- coding: utf-8 -*-
# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-21  CNHume  Renamed program to restaurant.  Moved classes into separate modules.
# 2019-01-07  CNHume  Allocate tables to seat groups of varying size

from RestaurantManager import RestaurantManager

def runTestCase(size_count_pairs, group_sizes):
    rm = RestaurantManager(size_count_pairs)
    # rm.showPermutations()
    rm.test(group_sizes)

def main():
    size_count_pairs = [ (2, 4), (4, 2), (6, 1) ]
    # size_count_pairs = [ (2, 2), (4, 4), (6, 2), (10, 1) ]

    group_sizes = [5, 6, 7]
    # group_sizes = [6, 7, 9, 4, 5]
    runTestCase(size_count_pairs, group_sizes)

main()
