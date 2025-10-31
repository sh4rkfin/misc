#!/usr/bin/env python3

import unittest
from enum import Enum


class Op(Enum):
    ADD = 1
    MUL = 2

    class Invalid(Exception):
        def __init__(self):
            super().__init__('not a valid operation')

    class MissingArguments(Exception):
        def __init__(self, op):
            super().__init__(f'operation {op} requires non-zero arguments')

    def perform(self, first, second):
        if self == Op.ADD:
            return first + second
        elif self == Op.MUL:
            return first * second
        raise Op.Invalid()

    def perform_many(self, *args):
        if not args:
            raise Op.MissingArguments(self)
        result = args[0]
        for arg in args[1:]:
            result = self.perform(result, arg)
        return result


class Value:

    class Invalid(Exception):
        def __init__(self, message):
            super().__init__(message)

    @staticmethod
    def maybe_wrap(value):
        if not isinstance(value, Value):
            return Value(value)
        return value

    def __init__(self, value,
                 children: tuple = (),
                 op: Op = None):
        """
        :param value: the value that this instance should wrap
        :param children: the dependent values
        """
        self.value = value
        self.children = children
        self.op = op
        self.grad = 0.0
        if self.children and not self.op or not self.children and self.op:
            raise Value.Invalid('children and op must be both '
                                'provided or not provided')

    def is_leaf(self):
        return not self.children

    def collect_values(self, collector: list = None):
        if collector is None:
            collector = []
        collector.append(self)
        for child in self.children:
            child.collect_values(collector)
        return collector

    def backward_inner(self, parent_gradient):
        if self.op == Op.ADD:
            self.grad += parent_gradient
            for c in self.children:
                c.backward_inner(self.grad)
        elif self.op == Op.MUL:
            self.grad += parent_gradient
            for idx, ci in enumerate(self.children):
                other_value = 1.0
                for jdx, cj in enumerate(self.children):
                    if idx != jdx:
                        other_value = self.op.perform(other_value, cj.value)
                ci.backward_inner(self.grad * other_value)
        else:
            self.grad += parent_gradient

    def backward(self):
        self.backward_inner(1.0)

    def __call__(self):
        results = [c() for c in self.children]
        if self.op:
            self.value = self.op.perform_many(*results)
        return self.value

    def __add__(self, other):
        other = Value.maybe_wrap(other)
        return Value(self.value + other.value,
                     children=(self, other),
                     op=Op.ADD)

    def __radd__(self, other):
        other = Value.maybe_wrap(other)
        return other.__add__(self)

    def __mul__(self, other):
        other = Value.maybe_wrap(other)
        return Value(self.value * other.value,
                     children=(self, other),
                     op=Op.MUL)

    def __rmul__(self, other):
        other = Value.maybe_wrap(other)
        return other.__mul__(self)

    def __repr__(self):
        result = f'Value[{self.value}, grad={self.grad}'
        if self.op:
            result += f',op={self.op}'
        if self.children:
            result += f',children={self.children}'
        return result + ']'

    def __str__(self):
        values = self.collect_values()
        result = ''
        for v in values:
            result += f' value={v.value} grad={v.grad}'
            if v.op:
                result += f' op={v.op}'
            if v.children:
                result += ': '
                for c in v.children:
                    result += f'c.value={c.value}, '
            result += '\n'
        return result


class UtilTestCase(unittest.TestCase):

    def test_add(self) -> None:
        v1 = Value(1)
        v2 = Value(3)
        new_val = 2 + v1 + 4 + v2
        # print(new_val)
        self.assertEqual(new_val.value, 10)

    def test_mul(self) -> None:
        v1 = Value(1)
        v2 = Value(3)
        new_val = 2 * v1 * 4 * v2
        # print(new_val)
        self.assertEqual(new_val.value, 24)

    def test_call(self) -> None:
        v1 = Value(1)
        v2 = Value(3)
        new_val = 2 * v1 + 4 * v2
        self.assertEqual(new_val.value, 14)
        result = new_val()
        self.assertEqual(result, 14)
        self.assertEqual(result, new_val.value)

    def test_op_perform_many(self):
        op = Op.ADD
        result = op.perform_many(1, 2, 3, 4)
        self.assertEqual(result, 10)
        result = op.perform_many(*[1, 2, 3, 4])
        self.assertEqual(result, 10)

    def test_collect_values(self) -> None:
        v1 = Value(1)
        v2 = Value(3)
        val = 2 * v1 + 4 * v2
        values = val.collect_values()
        # print(f'{str(val)}')
        self.assertIn(v1, values)
        self.assertIn(v2, values)
        self.assertIn(val, values)

    def test_basic_rate_of_change(self):
        v1 = Value(1)
        v2 = Value(3)
        val = 2 * v1 + 4 * v2
        e1 = val()
        inc = 0.001
        v2.value += inc
        del_v2 = (val() - e1) / inc
        self.assertAlmostEqual(4, del_v2)

    def test_val_by_val(self):
        v1 = Value(1)
        v2 = Value(3)
        val = v2 * v2 + 4 * v1
        # print(val)
        self.assertEqual(val.value, 13)
        inc = 0.00001
        v2.value += inc
        del_v2 = (val() - 13) / inc
        self.assertAlmostEqual(del_v2, 2 * (v2.value - inc), places=4)

    def test_more_than_binary_operations(self):
        v1 = Value(1)
        v2 = Value(2)
        v3 = Value(3)
        val = Value(None, (v1, v2, v3), Op.MUL)
        val()
        self.assertEqual(val(), 6)

    def test_basic_grad(self):
        v1 = Value(1)
        v2 = Value(3)
        val = v2 * v2 + 4 * v1
        val.backward()
        print(repr(v2))
        print(val)
        self.assertEqual(6, v2.grad)
        self.assertEqual(4, v1.grad)


if __name__ == '__main__':
    unittest.main()
