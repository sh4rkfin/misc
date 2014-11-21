#! /usr/bin/env python

def find_min_values(values):
    min1, min2 = values[0], values[1]
    if values[0] < values[1]:
        min1, min2 = min2, min1
    for v in values[2:]:
        if v < min1:
            min1, min2 = v, min1
        elif v < min2:
            min2 = v



