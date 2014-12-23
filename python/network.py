class Arc:
    def __init__(self, to_node, flow=0):
        self.to_node = to_node
        self.flow = flow


class Node:
    def __init__(self, key, arcs=None):
        self.key = key
        if arcs is None:
            arcs = []
        self.arcs = arcs
        self.network = None

    def add_arc(self, arc):
        self.arcs.append(arc)
        if self.network:
            self.network.invalidate_caches()

    def set_network(self, network):
        self.network = network


class Network:
    def __init__(self, nodes=None):
        if nodes is None:
            nodes = []
        self.nodes = []
        self.node_map = None
        for n in nodes:
            self.add_node(n)

    def invalidate_caches(self):
        self.node_map = None

    def add_node(self, node):
        node_map = self.get_node_map()
        found = node_map.get(node.key)
        if found is not None:
            if found == node:
                return
            if found != node:
                raise ValueError("can't add two nodes with same key to network; "
                                 "node: {0}, existing: {1}".format(node, found))
        node_map[node.key] = node
        self.nodes.append(node)
        node.set_network(self)
        for arc in node.arcs:
            self.add_node(arc.to_node)

    def get_node_map(self):
        if self.node_map is None:
            node_map = {}
            for node in self.nodes:
                node_map[node.key] = node
                for arc in node.arcs:
                    node_map[arc.to_node.key] = arc.to_node
            self.node_map = node_map
        return self.node_map

    def find_node(self, key):
        return self.get_node_map()[key]
