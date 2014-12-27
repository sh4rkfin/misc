#!/usr/bin/env python
import unittest

import util


class UtilTestCase(unittest.TestCase):
    def test_tuple_len(self):
        tuple = ('one', 2, 3.0)
        self.assertEqual(len(tuple), 3)

    def test_tuple_access(self):
        tuple = ('one', 2, 3.0)
        self.assertEqual(tuple[0], 'one')
        self.assertEqual(tuple[1], 2)

    def test_tuple_comparison(self):
        t1 = ('one', 1)
        t2 = ('two', 2)
        t3 = ('three', 3)
        t4 = ('four', 4)
        ts = [t2, t1, t3, t4]
        ts.sort()
        self.assertEqual(ts[0], t4)
        self.assertEqual(ts[1], t1)
        self.assertEqual(ts[2], t3)
        self.assertEqual(ts[3], t2)
        #print ts

    def test_break_continue(self):
        count = 0
        for i in range(3):
            if i == 0:
                continue
            count += 1
            if i == 1:
                break
        self.assertEqual(count, 1)

    def test_method_dict(self):

        class A:
            def __init__(self):
                self.foo = 'a'

        a = A()
        for x in a.__dict__:
            print a.__dict__[x]
        a.__dict__['bar'] = 'baz'
        self.assertEqual(a.bar, 'baz')
        a.__dict__['2d'] = 'hit'
        # invalid syntax: a.2d

    def test_basic_collapse(self):
        l = [1, 2, 3]
        result = util.accumulate(l, lambda x, y: x+y, 0)
        print "sum:", result
        self.assertEqual(6, result)

    def test_add_matrices(self):
        m1 = [[1 for _ in range(4)] for _ in range(3)]
        m2 = [[2 for _ in range(4)] for _ in range(5)]
        util.add_to(m1, m2)
        print m1
        for a in m1:
            for b in a:
                self.assertEqual(b, 3)

    def test_make_zero_matrix(self):
        m1 = [[1 for _ in range(4)] for _ in range(3)]
        m2 = util.make_zero_matrix_of_same_dimension(m1)
        self.assertEqual(len(m2), 3)
        for a in m2:
            self.assertEqual(len(a), 4)
            for b in a:
                self.assertEqual(b, 0)

    def test_accumulate_matrices(self):
        m1 = [[1 for _ in range(4)] for _ in range(3)]
        m2 = [[2 for _ in range(4)] for _ in range(5)]
        m = [m1, m2]

        def add_to(first, second):
            if first is None:
                first = util.make_zero_matrix_of_same_dimension(second)
            util.add_to(first, second)
            return first

        result = util.accumulate(m, util.add_to)
        self.assertEqual(len(result), 3)
        for a in result:
            self.assertEqual(len(a), 4)
            for b in a:
                self.assertEqual(b, 3)

    def test_dimension_count(self):
        m1 = [[1 for _ in range(4)] for _ in range(3)]
        m2 = [[2 for _ in range(4)] for _ in range(5)]
        self.assertEqual(util.dimension_count(m1), 2)
        self.assertEqual(util.dimension_count(m2), 2)
        m = [m1, m2]
        self.assertEqual(util.dimension_count(m), 3)
        self.assertEqual(util.dimension_count([]), 1)
        self.assertEqual(util.dimension_count([[]]), 2)
        self.assertEqual(util.dimension_count([[[]]]), 3)

    def test_arg_max(self):
        t1 = ('one', 1)
        t2 = ('two', 2)
        t3 = ('three', 3)
        t4 = ('four', 4)
        ts = [t2, t1, t3, t4]
        arg_max = util.arg_max(ts, lambda x: x[0])
        print "arg max:", arg_max
        self.assertEqual(arg_max, t2)

if __name__ == '__main__':
    unittest.main()
