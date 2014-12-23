#!/usr/bin/env python
import unittest


class UtilTestCase(unittest.TestCase):
    def test_tuple_len(self):
        tuple = ('one', 2, 3.0)
        self.assertEqual(len(tuple), 3)

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

if __name__ == '__main__':
    unittest.main()
