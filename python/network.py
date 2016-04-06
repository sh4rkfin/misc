import util

class Arc:
    def __init__(self, to_node, flow=0, base_arc=None):
        self._to_node = to_node
        self._flow = flow
        self._alternative_color_flow = 0
        self._cost = 0
        self._capacity = float("inf")
        self._base_arc = base_arc

    def is_residual(self):
        return self._base_arc is not None

    def total_flow(self):
        return self._flow + self._alternative_color_flow

    def to_node(self):
        return self._to_node

    def calculate_cost(self):
        return self._flow * self._cost

    def set_cost(self, cost):
        self._cost = cost

    def set_capacity(self, capacity):
        self._capacity = capacity

    def cost(self):
        return self._cost

    def alternative_color_flow(self):
        return self._alternative_color_flow

    def set_alternative_color_flow(self, value):
        self._alternative_color_flow = value

    def capacity(self):
        return self._capacity

    def residual_capacity(self):
        return self._capacity - self._flow - self._alternative_color_flow

    def adjust_capacity(self, value):
        self._capacity += value

    def change_flow(self, value):
        self._flow += value

    def augment_flow(self, flow):
        self._flow += flow

    def has_residual_capacity(self):
        return self.residual_capacity() > 0

    def is_residual_arc_for(self, base_arc):
        return self._base_arc == base_arc

    def __repr__(self):
        return "->{0}".format(self._to_node.key)


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
            self.network.add_node(arc.to_node())

    def has_arc(self, arc):
        return arc in self.arcs

    def set_network(self, network):
        self.network = network

    def change_source_flow(self, value):
        self.source += value

    def __str__(self):
        return self.to_string()

    def calculate_cost(self):
        result = 0
        for a in self.arcs:
            result += a.calculate_cost()
        return result

    @staticmethod
    def key_to_string(key):
        if isinstance(key, tuple):
            return ':'.join([str(k) for k in list(key)])
        return str(key)

    def to_string(self, key_stringer=None, arcs_on_separate_lines=False):
        if key_stringer is None:
            key_stringer = Node.key_to_string
        result = '{0} {1} -> '.format(key_stringer(self.key), self.source)
        count = len(self.arcs)
        if arcs_on_separate_lines and count > 0:
            result += "\n     -> "
        for i, a in enumerate(self.arcs):
            result += "{0},f={1},c={2}".format(key_stringer(a.to_node().key), a.total_flow(), a.cost())
            if a.capacity() != float('inf'):
                result += ",u={0} ".format(a.capacity())
            else:
                result += " "
            if arcs_on_separate_lines and i < count - 1:
                result += "\n"
                result += "     -> "
        return result

    def __repr__(self):
        return '{0}'.format(self.key)

    def set_source(self, source):
        self.source = source

    def get_arc(self, to_node):
        for a in self.arcs:
            if a.to_node() == to_node:
                return a
        return None

    def get_max_arc(self, key_function):
        return util.arg_max(self.arcs, key_function)

    def gather_nodes(self, collector):
        if self not in collector:
            collector[self] = self
            for a in self.arcs:
                a.to_node().gather_nodes(collector)

    def find_residual_arc(self, base_arc):
        for a in self.arcs:
            if a.is_residual_arc_for(base_arc):
                return a

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
        return self._node if l == 0 else self._arcs[l - 1].to_node()

    def add_arc(self, arc):
        if not self.to_node().has_arc(arc):
            raise ValueError("Cant add non-connected arc to path; "
                             "to_node() = {0}, arc = {1}, self: {2}".format(self.to_node(), arc, self))
        self._arcs.append(arc)

    def nodes(self):
        result = [self._node]
        for a in self._arcs:
            result.append(a.to_node())
        return result

    def __getitem__(self, idx):
        return self._arcs[idx]

    def get_min_value(self, value_getter):
        return util.minimize(self._arcs, value_getter)

    def find_max_augmenting_flow(self):
        result = self.get_min_value(Arc.residual_capacity)
        result = min(result, self._node.source)
        result = min(result, -self.to_node().source)
        result = max(result, 0)
        return result

    def get_min_flow(self):
        return self.get_min_value(Arc.total_flow)

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
            a.change_flow(value)
        if not self.is_cycle():
            self._node.change_source_flow(value)
            self.to_node().change_source_flow(-value)

    def sum_costs(self):
        result = 0
        for a in self._arcs:
            result += a.cost()
        return result

    def consume_flow(self, value):
        if len(self) == 0:
            return
        from_node = self._node
        for a in self._arcs:
            # augment the flow and reduce the residual capacity
            a.augment_flow(value)
            # create / increase the residual back arc
            residual_arc = a.to_node().find_residual_arc(a)
            if residual_arc is None:
                residual_arc = Arc(from_node, 0, a)
                residual_arc.set_capacity(0)
                a.to_node().add_arc(residual_arc)
            residual_arc.adjust_capacity(value)
            # update the from_node
            from_node = a.to_node()

        if not self.is_cycle():
            self._node.change_source_flow(-value)
            self.to_node().change_source_flow(+value)


class Network:

    def __init__(self, nodes=None):
        self._node_map = {}
        self._tol = 1e-3
        self._default_node_comparator = None
        if nodes is not None:
            for n in nodes:
                self.add_node(n)

    def set_default_node_comparator(self, default_node_comparator):
        self._default_node_comparator = default_node_comparator

    def add_node(self, node):
        # print "*** try add node: ", hex(id(node)), node,
        found = self._node_map.get(node.key)
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
            if n.key not in self._node_map:
                self._node_map[n.key] = n
                n.set_network(self)

    def get_node_map(self):
        return self._node_map

    def find_node(self, key):
        return self._node_map.get(key)

    def find_node_satisfying(self, predicate):
        for n in self._node_map.values():
            if predicate(n):
                return n

    def find_nodes_satisfying(self, predicate):
        result = []
        for n in self._node_map.values():
            if predicate(n):
                result.append(n)
        return result

    def calculate_cost(self):
        result = 0
        for n in self._node_map.values():
            result += n.calculate_cost()
        return result

    def find_or_create_node(self, key):
        node = self._node_map.get(key)
        if node is None:
            node = Node(key)
            self.add_node(node)
        return node

    def draw(self, comparator=None, key_stringer=None, arcs_on_separate_lines=False):
        node_map = self.get_node_map()
        nodes = node_map.values()
        if comparator is None:
            comparator = self._default_node_comparator
        nodes.sort(comparator)
        for n in nodes:
            print n.to_string(key_stringer, arcs_on_separate_lines)

    def get_max_source_node(self):
        max_source, max_node = 0, None
        for n in self._node_map.values():
            if n.source > max_source or max_node is None:
                max_node = n
                max_source = n.source
        return max_node

    def break_into_flows(self):
        result = {}
        while True:
            # find max source
            max_node = self.get_max_source_node()
            if util.le(max_node.source, 0, self._tol):
                # no more source nodes, return
                break
            memo = {max_node: {'parent': None}}
            path = Network.resolve_flow(max_node, memo)
            min_flow = path.get_min_flow()
            path.change_flow(-min_flow)
            result[path] = min_flow
        return result

    @staticmethod
    def create_shortest_path_tree(source_node):
        """
        Creates and returns a shortest path tree rooted at source_node. Should be Bellman-Ford.
        :param source_node: root of the shortest path tree
        :return: the shortest path tree
        """
        memo = {}
        current = source_node
        memo[current] = {'state': 'unvisited', 'dist': 0, 'parent': None, 'arc': None}
        while True:
            current_dist = memo[current]['dist']
            for a in current.arcs:
                if not a.has_residual_capacity():
                    continue
                node = a.to_node()
                node_memo = memo.get(node)
                if node_memo is None:
                    memo[node] = {'state': 'unvisited', 'dist': float('inf')}
                    node_memo = memo[node]
                node_dist = node_memo['dist']
                tentative_dist = current_dist + a.cost()
                if tentative_dist < node_dist:
                    node_memo['state'] = 'unvisited'
                    # update tentative distance
                    if tentative_dist < node_dist:
                        node_memo['dist'] = tentative_dist
                        node_memo['parent'] = current
                        node_memo['arc'] = a
            min_dist, arg_min = None, None
            memo[current]['state'] = 'visited'
            for n, m in memo.items():
                if m['state'] == 'unvisited':
                    if min_dist is None or min_dist > m['dist']:
                        min_dist = m['dist']
                        arg_min = n
            if min_dist is None:
                break
            current = arg_min
        return memo

    @staticmethod
    def resolve_flow(node, memo, tol=1e-3):
        if util.ge(node.source, 0, tol):
            # not a a sink node, keep looking
            arc = node.get_max_arc(Arc.total_flow)
            memo[arc.to_node()] = {'parent': node, 'arc': arc}
            return Network.resolve_flow(arc.to_node(), memo)

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