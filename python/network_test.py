#!/usr/bin/env python
import unittest

from network import Arc, Node, Network

class NetworkTestCase(unittest.TestCase):
    def test_basic(self):
        node = Node('one')
        nw = Network([node])
        self.assertEqual(nw.find_node('one'), node)
        node = Node(('two', 2))
        nw.add_node(node)
        self.assertEqual(nw.find_node(('two', 2)), node)
        three = Node('three')
        arc = Arc(three, 1)
        nw.find_node('one').add_arc(arc)
        self.assertEqual(nw.find_node('three'), three)

    def test_dont_lose_nodes(self):
        n1 = Node(1)
        n2 = Node(2)
        n1.add_arc(Arc(n2))
        nw = Network([n1])
        self.assertEqual(nw.find_node(2), n2)
        n3 = Node(3)
        n2.add_arc(Arc(n3))
        self.assertEqual(nw.find_node(3), n3)

    def test_has_arc(self):
        n1 = Node(1)
        n2 = Node(2)
        a = Arc(n2)
        n1.add_arc(a)
        self.assertTrue(n1.has_arc(a))

    def test_to_node(self):
        n2 = Node(2)
        a = Arc(n2)
        self.assertTrue(a._to_node == n2)


if __name__ == '__main__':
    unittest.main()
