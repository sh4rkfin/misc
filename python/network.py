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

    def set_network(self, network):
        self.network = network

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


class Network:
    def __init__(self, nodes=None):
        self.node_map = {}
        if nodes is not None:
            for n in nodes:
                self.add_node(n)

    def add_node(self, node):
        #print "*** try add node: ", hex(id(node)), node,
        found = self.node_map.get(node.key)
        if found is not None:
            if found == node:
                #print "... node exists: "
                return
            if found != node:
                raise ValueError("can't add two nodes with same key to network; "
                                 "node: {0}, existing: {1}".format(node, found))
        #print "... node added: ", node
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
        # find max source
        while True:
            max_node = self.get_max_source_node()
            if max_node.source <= 0:
                break
            memo = {max_node: {'parent': None}}
            path = Network.resolve_flow(max_node, memo)
            path.reverse()
            if path is None:
                self.draw()
            min_flow = -1
            for p in path:
                if min_flow == -1 or min_flow > p.flow:
                    min_flow = p.flow
            for p in path:
                p.flow -= min_flow
            max_node.source -= min_flow
            path[len(path) - 1].to_node.source += min_flow
            print "flow: ", min_flow, max_node.key, path
        self.draw()

    @staticmethod
    def resolve_flow(node, memo):
        if node.source < 0:
            current, path = node, []
            parent = memo[current]['parent']
            while parent is not None:
                path.append(memo[current]['arc'])
                current = parent
                parent = memo[current]['parent'] if current is not None else None
            return path

        arc = node.get_max_arc(Arc.get_flow)
        if arc is None:
            print "*** bad node: ", node
        if arc.flow == 0:
            # done
            return

        memo[arc.to_node] = {'parent': node, 'arc': arc}
        return Network.resolve_flow(arc.to_node, memo)






