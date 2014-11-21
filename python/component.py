#! /usr/bin/env python

class Component:
    PROCESSING = 0
    PROCESSED = 1

    def __init__(self, name, dependents):
        self.name = name
        self.dependents = dependents

    def build_dependency_list(self, list=None, processing_states=None):
        if list == None: list = [] # because of the mutable default argument gotcha
        if processing_states == None: processing_states = {}
        has_loop = False
        processing_state = processing_states.get(self)
        if processing_state == None:
            processing_states[self] = Component.PROCESSING
            for d in self.dependents:
                (x, child_has_loop) = d.build_dependency_list(list, processing_states)
                if child_has_loop:
                    has_loop = True
            # add the component to the end of the list
            list.append(self.name)
            processing_states[self] = Component.PROCESSED
        elif processing_state == Component.PROCESSING:
            # loop case, bail
            has_loop = True
        return (list, has_loop)

    def add(self, dependent):
        if dependent not in self.dependents:
            self.dependents.append(dependent)


a = Component("a", [])
b = Component("b", [a])
c = Component("c", [b,a])
d = Component("d", [a,b,c])
e = Component("e", [a,b,c,d])

list, has_loop = e.build_dependency_list()
print "order: {0}, has_loop: {1}".format(list, has_loop)

assert not has_loop, "has loop should be false"
assert list == ["a", "b", "c", "d", "e"], "list should be: a b c d e"

a.add(c)

list, has_loop = e.build_dependency_list()
print "order: {0}, has_loop: {1}".format(list, has_loop)

assert has_loop, "has loop should be true"
assert list == ["b", "c", "a", "d", "e"]
