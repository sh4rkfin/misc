#!/usr/bin/env python
import unittest

import re

class ModelTestCase(unittest.TestCase):
    def test_2d_or_3d(self):
        regex = "x\[(\d+),(\d+)(,(\d+))?\]\s*(\*)?\s*(\d+)"
        matcher = re.compile(regex)
        m = matcher.search("x[1,2,3] 4")
        print m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        self.assertEquals(m.group(1), '1')
        self.assertEquals(m.group(2), '2')
        self.assertEquals(m.group(4), '3')
        self.assertEquals(m.group(5), None)
        self.assertEquals(m.group(6), '4')
        m = matcher.search("x[1,2] 4")
        print m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        self.assertEquals(m.group(1), '1')
        self.assertEquals(m.group(2), '2')
        self.assertEquals(m.group(4), None)
        self.assertEquals(m.group(5), None)
        self.assertEquals(m.group(6), '4')

    def test_all(self):
        regex = "x\[(\d+)(,(\d+)(,(\d+))?)?\]\s*(\*)?\s*(\d+)"
        matcher = re.compile(regex)
        m = matcher.search("x[1,2,3] 4")
        print m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        self.assertEquals(m.group(1), '1')
        self.assertEquals(m.group(3), '2')
        self.assertEquals(m.group(5), '3')
        self.assertEquals(m.group(6), None)
        self.assertEquals(m.group(7), '4')
        m = matcher.search("x[1,2] 4")
        print m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        self.assertEquals(m.group(1), '1')
        self.assertEquals(m.group(3), '2')
        self.assertEquals(m.group(5), None)
        self.assertEquals(m.group(6), None)
        self.assertEquals(m.group(7), '4')
        m = matcher.search("x[1] 4")
        print m.group(1), m.group(2), m.group(3), m.group(4), m.group(5), m.group(6)
        self.assertEquals(m.group(1), '1')
        self.assertEquals(m.group(3), None)
        self.assertEquals(m.group(5), None)
        self.assertEquals(m.group(6), None)
        self.assertEquals(m.group(7), '4')

    def test_callback(self):
        foo = 'foo'
        results = []

        def callback(stuff):
            foo = stuff
            results.append(stuff)

        callback('first')
        callback('second')

        print "results:", results


if __name__ == '__main__':
    unittest.main()
