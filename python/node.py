#! /usr/bin/env python

class Node:
    depth_invocations = 0
    def __init__(self, children):
        self.children = children

    def depth(self):
        Node.depth_invocations += 1
        if not self.children:
            return 0
        max([c.depth() for c in self.children]) + 1
        #max_depth = 0
        #for c in self.children:
        #    max_depth = max(c.depth(), max_depth)
        #return max_depth + 1

a = Node([])
b = Node([a])
c = Node([b,a])
d = Node([a,b,c])
e = Node([a,b,c,d])

print "node depth: {0}, depth_invocations: {1}".format(e.depth(), Node.depth_invocations)

