# -*- coding: utf-8 -*-
# (C) Copyright 2019, Christopher N. Hume.  All rights reserved.
# 2019-01-21  CNHume  Created BestCandidate module

from __future__ import division
from TableSizeCounter import TableSizeCounter

class BestCandidate:
    """
    Identify the best seating when presented a series of candidates.  Each candidate is a list of size_count_pairs.
    """
    def __init__(self, group_size, table_count, seat_count):
        self.group_size = group_size
        self.table_count = table_count
        self.seat_count = seat_count
        self.clear()

    def clear(self):
        self.candidate = None
        self.loss = 0

    def orderByTablesThenSeats(self, table_sum, seat_sum):
        """Order by table_sum then seat_sum"""
        return table_sum * self.seat_count + seat_sum

    def orderBySeatsThenTables(self, table_sum, seat_sum):
        """Order by seat_sum then table_sum"""
        return seat_sum * self.table_count + table_sum

    def orderByDistance(self, table_sum, seat_sum):
        """Order by normalized Euclidean distance of (table_sum, seat_sum) from the Origin"""
        return (table_sum / self.table_count)**2 + (seat_sum / self.seat_count)**2

    def update(self, candidate):
        seat_sum = TableSizeCounter.seatSum(candidate)
        if seat_sum < self.group_size:
            return

        table_sum = TableSizeCounter.tableSum(candidate)
        loss = self.orderByDistance(table_sum, seat_sum)

        # Does candidate improve the optimal seating?
        update = not self.candidate or loss < self.loss
        if update:
            self.candidate = candidate
            self.loss = loss

    def best(self, candidates):
        """Find optimal candidate for group_size"""
        self.clear()
        for candidate in candidates:
            self.update(candidate)
        return self.candidate
