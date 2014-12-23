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

if __name__ == '__main__':
    unittest.main()
