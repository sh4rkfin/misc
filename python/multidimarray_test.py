#!/usr/bin/env python
import unittest

from multidimarray import MultiDimArray


class MultiDimArrayTestCase(unittest.TestCase):
    def test_basic(self):
        array = MultiDimArray({}, 4, 3, 2)
        self.assertEqual(array[1][1][1], 0)
        self.assertEqual(array[0][0][0], 0)
        self.assertEqual(array[5][5][5], 0)

    def test_non_zero(self):
        val = {
            1: {
                2: {
                    1: 10
                }
            }
        }
        array = MultiDimArray(val, 4, 3, 2)
        self.assertEqual(array[1][1][1], 0)
        self.assertEqual(array[0][0][0], 0)
        self.assertEqual(array[5][5][5], 0)
        self.assertEqual(array[1][2][1], 10)

    def test_len_and_iter(self):
        array = MultiDimArray({}, 4, 3, 2)
        self.assertEqual(len(array), 4, "len should be 4")
        for x in array:
            self.assertEqual(len(x), 3)
            for y in x:
                self.assertEqual(len(y), 2)
                for z in y:
                    self.assertEqual(z, 0)

    def test_caching_empty(self):
        val = {1: {2: {1: 10}}}
        array = MultiDimArray(val, 4, 3, 2)
        sub = array[2]
        sub2 = array[2]
        print sub
        print sub2
        self.assertTrue(sub == sub2)

    def test_enumerate(self):
        val = {1: {2: {1: 10}}}
        array = MultiDimArray(val, 4, 3, 2)
        count = 0
        for i, v in enumerate(array):
            for j, w in enumerate(v):
                for k, x in enumerate(w):
                    # print "array[{0},{1},{2}]: {3}".format(i, j, k, x)
                    count += 1
                    if i == 1 and j == 2 and k == 1:
                        self.assertEqual(x, 10)
                    else:
                        self.assertEqual(x, 0)
        self.assertEqual(count, 24)

if __name__ == '__main__':
    unittest.main()
