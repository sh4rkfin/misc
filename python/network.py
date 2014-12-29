import util

class Arc:
    def __init__(self, to_node, flow=0):
        self.to_node = to_node
        self.flow = flow

    def get_flow(self):
        return self.flow

    def __repr__(self):
        return "->{0}".format(self.to_node.key)

class Node:
    def __init__(self, key, source=0, arcs=None):
        self.key = key
        self.source = source
        if arcs is None:
            arcs = []
        self.arcs = arcs
        self.network = None

    def add_arc(self, arc):
        self.arcs.append(arc)
        if self.network:
            self.network.add_node(arc.to_node)

    def has_arc(self, arc):
        return arc in self.arcs

    def set_network(self, network):
        self.network = network

    def change_source_flow(self, value):
        self.source += value

    def __str__(self):
        result = '{0} {1} -> '.format(self.key, self.source)
        for a in self.arcs:
            result += str(a.to_node.key) + ' ' + str(a.flow) + ' '
        return result

    def __repr__(self):
        return '{0}'.format(self.key)

    def set_source(self, source):
        self.source = source

    def get_arc(self, to_node):
        for a in self.arcs:
            if a.to_node == to_node:
                return a
        return None

    def get_max_arc(self, key_function):
        return util.arg_max(self.arcs, key_function)

    def gather_nodes(self, collector):
        if self not in collector:
            collector[self] = self
            for a in self.arcs:
                a.to_node.gather_nodes(collector)

    @staticmethod
    def compare(n1, n2):
        if n1.source == n2.source:
            return -1 if n1.key < n2. key else 1 if n1.key > n2.key else 0
        return n1.source - n2.source


class Path:
    def __init__(self, node, arcs=None):
        if arcs is None:
            arcs = []
        self._node = node
        self._arcs = []
        for a in arcs:
            self.add_arc(a)

    def from_node(self):
        return self._node

    def to_node(self):
        l = len(self._arcs)
        return self._node if l == 0 else self._arcs[l - 1].to_node

    def add_arc(self, arc):
        if not self.to_node().has_arc(arc):
            raise ValueError("Cant add non-connected arc to path; "
                             "to_node() = {0}, arc = {1}, self: {2}".format(self.to_node(), arc, self))
        self._arcs.append(arc)

    def nodes(self):
        result = [self._node]
        for a in self._arcs:
            result.append(a.to_node)
        return result

    def __getitem__(self, idx):
        return self._arcs[idx]

    def get_min_flow(self):
        result = -1
        for a in self._arcs:
            if result == -1 or result > a.flow:
                result = a.flow
        return result

    def __len__(self):
        return len(self._arcs)

    def __str__(self):
        return '{0}{1}'.format(self._node.key, self._arcs)

    def is_cycle(self):
        return len(self) > 0 and self.to_node() == self._node

    def change_flow(self, value):
        if len(self) == 0:
            return
        for a in self._arcs:
            a.flow += value
        if not self.is_cycle():
            self._node.change_source_flow(value)
            self.to_node().change_source_flow(-value)


class Network:

    def __init__(self, nodes=None):
        self.node_map = {}
        self.tol = 1e-3
        if nodes is not None:
            for n in nodes:
                self.add_node(n)

    def add_node(self, node):
        # print "*** try add node: ", hex(id(node)), node,
        found = self.node_map.get(node.key)
        if found is not None:
            if found == node:
                # print "... node exists: "
                return
            if found != node:
                raise ValueError("can't add two nodes with same key to network; "
                                 "node: {0}, existing: {1}".format(node, found))
        # print "... node added: ", node
        nodes = {}
        node.gather_nodes(nodes)
        for n in nodes:
            if n.key not in self.node_map:
                self.node_map[n.key] = n
                n.set_network(self)

    def get_node_map(self):
        return self.node_map

    def find_node(self, key):
        return self.node_map.get(key)

    def find_or_create_node(self, key):
        node = self.node_map.get(key)
        if node is None:
            node = Node(key)
            self.add_node(node)
        return node

    def draw(self, comparator=None):
        node_map = self.get_node_map()
        nodes = node_map.values()
        nodes.sort(comparator)
        for n in nodes:
            print hex(id(n)), str(n)

    def get_max_source_node(self):
        max_source, max_node = 0, None
        for n in self.node_map.values():
            if n.source > max_source or max_node is None:
                max_node = n
                max_source = n.source
        return max_node

    def break_into_flows(self):
        result = {}
        while True:
            # find max source
            max_node = self.get_max_source_node()
            if util.le(max_node.source, 0, self.tol):
                # no more source nodes, return
                break
            memo = {max_node: {'parent': None}}
            path = Network.resolve_flow(max_node, memo)
            min_flow = path.get_min_flow()
            path.change_flow(-min_flow)
            result[path] = min_flow
        return result

    @staticmethod
    def resolve_flow(node, memo, tol=1e-3):
        if util.ge(node.source, 0, tol):
            # not a a sink node, keep looking
            arc = node.get_max_arc(Arc.get_flow)
            memo[arc.to_node] = {'parent': node, 'arc': arc}
            return Network.resolve_flow(arc.to_node, memo)

        # we are at a sink node, compute the path
        current, path = node, []
        parent = memo[current]['parent']
        while parent is not None:
            arc = memo[current]['arc']
            path.append(arc)
            current = parent
            parent = memo[current]['parent']
        path.reverse()
        return Path(current, path)






