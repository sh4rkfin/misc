#! /usr/bin/env python

class Component2:

    def __init__(self, name, dependents):
        self.name = name
        self.dependents = dependents

    def build_dependency_list_slowly(self, list=None):
        if list == None: list = [] # because of the mutable default argument gotcha
        for d in self.dependents:
            d.build_dependency_list_slowly(list)
        if self.name not in list:
            list.append(self.name)
        return list

    # Accepts and if necessary updates the supplied rank map.
    # The rank map maps component to integer rank where rank represents
    # the build order of the component.
    def build_dependency_list(self, rank=None):
        if rank == None: rank = {} # because of the mutable default argument gotcha
        if self not in rank:
            for d in self.dependents:
                d.build_dependency_list(rank)
            rank[self] = len(rank)
        return rank

    def add(self, dependent):
        if dependent not in self.dependents:
            self.dependents.append(dependent)

    def name(self):
        return self.name

a = Component2("a", [])
b = Component2("b", [a])
c = Component2("c", [b,a])
d = Component2("d", [a,b,c])
e = Component2("e", [a,b,c,d])

component_map = e.build_dependency_list()
list = sorted(component_map, key=component_map.get)
#names = map(lambda elem: elem.name, list)
names = map(Component2.name, list)
print "order: {0}".format(names)

assert names == ["a", "b", "c", "d", "e"], "list should be: a b c d e"

