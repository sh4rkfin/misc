import util
import heapq

class Arc:
    """
    Represents a connection to a specified node, the to_node, potentially
    with flow that may be of different colors along this connection.

    This arc may also be a "residual arc"; that is, a notional connection
    that exists to track the idea that flow may be reduced along an
    arc that runs in the opposite direction.

    Residual arcs may have a specific color. That is, only flow of a given
    color may be pushed along the arc.
    """
    def __init__(self, to_node, flow=0, base_arc=None, color=None):
        self._to_node = to_node
        self._cost = 0
        self._capacity = float("inf")
        if base_arc is None and color is not None:
            raise ValueError("only residual arcs may be color specific")
        self._base_arc = base_arc
        self._single_color = color
        self._total = None
        self._flow = {}
        if flow != 0:
            self.set_flow(flow)

    def is_residual(self):
        return self._base_arc is not None

    def base_arc(self):
        return self._base_arc

    def get_single_color(self):
        return self._single_color

    def to_node(self):
        return self._to_node

    def calculate_cost(self):
        return self.total_flow() * self._cost

    def set_cost(self, cost):
        self._cost = cost

    def set_capacity(self, capacity):
        self._capacity = capacity

    def cost(self):
        return self._cost

    def capacity(self):
        return self._capacity

    def flow(self, color=None):
        value = self._flow.get(color)
        return value if value else 0

    def residual_capacity(self, color=None):
        if not self.is_color_supported(color):
            return 0
        return self._capacity - self.total_flow()

    def adjust_capacity(self, value):
        self._capacity += value

    def flow_map(self):
        return self._flow

    def total_flow(self):
        if self._total is None:
            self._total = sum(self._flow.viewvalues())
        return self._total

    def is_color_supported(self, color):
        return self._single_color is None or color == self._single_color

    def assers_is_supported_color(self, color):
        if not self.is_color_supported(color):
            raise ValueError("color is not supported")

    def set_flow(self, value, color=None):
        self.assers_is_supported_color(color)
        self._flow[color] = value
        self._total = None

    def change_flow(self, value, color=None):
        self.assers_is_supported_color(color)
        flow = self._flow.get(color)
        if flow is None:
            self._flow[color] = value
        else:
            self._flow[color] += value
        self._total = None

    def augment_flow(self, flow, color=None):
        self.change_flow(flow, color)

    def has_residual_capacity(self, color=None):
        return self.residual_capacity(color) > 0

    def is_residual_arc_for(self, base_arc, color=None):
        return self._base_arc == base_arc and self.is_color_supported(color)

    def __repr__(self):
        return "->{0}".format(self._to_node.key())


class Node:
    def __init__(self, key, source=0, arcs=None):
        self._key = key
        self._source = {}
        if source:
            self._source[None] = source
        if arcs is None:
            arcs = []
        self._arcs = arcs
        self._network = None
        self._total = None

    def key(self):
        return self._key

    def arcs(self):
        return self._arcs

    def add_arc(self, arc):
        self._arcs.append(arc)
        if self._network:
            self._network.add_node(arc.to_node())

    def has_arc(self, arc):
        return arc in self._arcs

    def set_network(self, network):
        self._network = network

    def change_source_flow(self, value, color=None):
        source = self._source.get(color)
        if source is None:
            self._source[color] = value
        else:
            self._source[color] += value
        self._total = None

    def source(self, color=None):
        value = self._source.get(color)
        return value if value else 0

    def set_source(self, source, color=None):
        self._source = {color: source}
        self._total = None

    def total_source(self):
        if self._total is None:
            self._total = sum(self._source.viewvalues())
        return self._total

    def get_max_source_color(self):
        item = util.arg_max(self._source.items(), lambda x: x[1])
        return None if item is None else item[0]

    def __str__(self):
        return self.to_string()

    def calculate_cost(self):
        result = 0
        for a in self._arcs:
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
        result = '{0} {1} -> '.format(key_stringer(self._key), self._source if self._source else "")
        count = len(self._arcs)
        if arcs_on_separate_lines and count > 0:
            result += "\n     -> "
        for i, a in enumerate(self._arcs):
            flow = a.flow_map()
            result += "{0},f={1},c={2}".format(key_stringer(a.to_node().key()),
                                               flow if flow else 0, a.cost())
            if a.capacity() != float('inf'):
                result += ",u={0} ".format(a.capacity())
            else:
                result += " "
            if arcs_on_separate_lines and i < count - 1:
                result += "\n"
                result += "     -> "
        return result

    def __repr__(self):
        return '{0}'.format(self._key)

    def get_arc(self, to_node):
        for a in self._arcs:
            if a.to_node() == to_node:
                return a
        return None

    def get_max_arc(self, value_function):
        return util.arg_max(self._arcs, value_function)

    def gather_nodes(self, collector):
        if self not in collector:
            collector[self] = self
            for a in self._arcs:
                a.to_node().gather_nodes(collector)

    def find_residual_arc(self, base_arc, color=None):
        for a in self._arcs:
            if a.is_residual_arc_for(base_arc, color):
                return a

    def get_non_zero_source_colors(self):
        result = []
        for k, v in self._source.items():
            if v:
                result.append(k)
        return result

    @staticmethod
    def compare(n1, n2):
        n1_source, n2_source = n1.total_source(), n2.total_source()
        if n1_source == n2_source:
            return -1 if n1.key < n2. key else 1 if n1.key > n2.key else 0
        return n1_source - n2_source


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

    def find_max_augmenting_flow(self, color=None):
        result = self.get_min_value(lambda x: x.residual_capacity(color))
        result = min(result, self._node.source(color))
        result = min(result, -self.to_node().source(color))
        result = max(result, 0)
        return result

    def get_min_flow(self):
        return self.get_min_value(Arc.total_flow)

    def __len__(self):
        return len(self._arcs)

    def __str__(self):
        return '{0}{1}'.format(self._node.key(), self._arcs)

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

    def consume_flow(self, value, color=None):
        if len(self) == 0:
            return
        from_node = self._node
        for a in self._arcs:
            # augment the flow and reduce the residual capacity
            base_arc = a.base_arc()
            change = value
            if base_arc is None:
                base_arc = a
                residual_arc = a.to_node().find_residual_arc(a, color)
                if residual_arc is None:
                    residual_arc = Arc(from_node, 0, a, color)
                    residual_arc.set_cost(-base_arc.cost())
                    residual_arc.set_capacity(0)
                    a.to_node().add_arc(residual_arc)
            else:
                residual_arc = a
                change = -value

            base_arc.augment_flow(change, color)
            # create / increase the residual back arc
            residual_arc.adjust_capacity(change)
            # update the from_node
            from_node = a.to_node()

        if not self.is_cycle():
            self._node.change_source_flow(-value, color)
            self.to_node().change_source_flow(+value, color)


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

    def get_non_zero_source_colors(self):
        result = {}
        for n in self._node_map.viewvalues():
            colors = n.get_non_zero_source_colors()
            for c in colors:
                result[c] = True
        return result.keys()

    def add_node(self, node):
        # print "*** try add node: ", hex(id(node)), node,
        found = self._node_map.get(node.key())
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
                self._node_map[n.key()] = n
                n.set_network(self)

    def get_node_map(self):
        return self._node_map

    def find_node(self, key):
        return self._node_map.get(key)

    def find_node_satisfying(self, predicate):
        for n in self._node_map.viewvalues():
            if predicate(n):
                return n

    def find_nodes_satisfying(self, predicate):
        result = []
        for n in self._node_map.viewvalues():
            if predicate(n):
                result.append(n)
        return result

    def calculate_cost(self):
        result = 0
        for n in self._node_map.viewvalues():
            result += n.calculate_cost()
        return result

    def find_node(self, key):
        return self._node_map.get(key)

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

    def get_total_positive_source(self):
        result = 0
        for n in self._node_map.viewvalues():
            src = n.total_source()
            if src > 0:
                result += src
        return result

    def push_min_cost_flow(self, source_node, color=None, cost_threshold=None):
        """
        Pushes as much flow as possible of the specified color from source_node to some
        destination along the minimum cost path, as long as the cost is less than the
        cost_threshold, if specified.

        This function assumes that the source value of a node is not balanced by
        net outbound flow. It tries to find a reachable destination node with
        destination.source(color) < 0 and the push as much flow as possible along the
        min cost path connecting these nodes.

        :param source_node: the source node; source_node.source(color) is assumed > 0
        :param color: the color of the flow to push
        :param cost_threshold: the cost threshold
        :return: the Path and amount of flow that was pushed (and the source in
                    source_node reduced by); None and 0 if no path was found
        """
        sp = self.create_shortest_path_tree(source_node, color)
        ns = self.find_nodes_satisfying(lambda x: sp.get(x) is not None and x.source(color) < 0)
        dest_node = util.arg_min(ns, lambda x: sp[x]['dist'])
        if cost_threshold is None or sp[dest_node]['dist'] <= cost_threshold:
            parent = sp[dest_node]['parent']
            path = []
            while parent is not None:
                path.append(sp[dest_node]['arc'])
                dest_node = parent
                parent = sp[parent]['parent']
            path.reverse()
            p = Path(dest_node, path)
            cap = p.find_max_augmenting_flow(color)
            p.consume_flow(cap, color)
            return p, cap
        return None, 0

    def get_reduced_costs(self, potentials):
        result = {}
        for n in self._node_map.viewvalues():
            dist = potentials.get(n)
            dfrom = float('inf') if dist is None else dist['dist']
            for a in n.arcs():
                if a.has_residual_capacity():
                    dist = potentials.get(a.to_node())
                    dto = float('inf') if dist is None else dist['dist']
                    result[(n, a)] = a.cost() + dfrom - dto
        return result

    @staticmethod
    def create_shortest_path_tree(source_node, color=None):
        """
        Creates and returns a shortest path tree rooted at source_node.
        :param source_node: root of the shortest path tree
        :return: the shortest path tree
        """
        memo = {}
        current = source_node
        memo[current] = {'state': 'unvisited', 'dist': 0, 'parent': None, 'arc': None}
        while True:
            current_dist = memo[current]['dist']
            for a in current.arcs():
                if not a.has_residual_capacity(color):
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
    def create_shortest_path_tree3(source_node, color=None):
        """
        Creates and returns a shortest path tree rooted at source_node.
        :param source_node: root of the shortest path tree
        :return: the shortest path tree
        """
        memo = {}
        unvisited = {source_node: True}
        memo[source_node] = {'dist': 0, 'parent': None, 'arc': None}
        current = source_node
        while True:
            del unvisited[current]
            current_dist = memo[current]['dist']
            for a in current.arcs():
                if not a.has_residual_capacity(color):
                    continue
                node = a.to_node()
                node_memo = memo.get(node)
                if node_memo is None:
                    unvisited[node] = True
                    memo[node] = {'dist': float('inf')}
                    node_memo = memo[node]
                node_dist = node_memo['dist']
                tentative_dist = current_dist + a.cost()
                if tentative_dist < node_dist:
                    unvisited[node] = True
                    node_memo['dist'] = tentative_dist
                    node_memo['parent'] = current
                    node_memo['arc'] = a
            min_dist, arg_min = None, None
            for n in unvisited:
                m = memo[n]
                if min_dist is None or min_dist > m['dist']:
                    min_dist = m['dist']
                    arg_min = n
            if min_dist is None:
                break
            current = arg_min
        return memo

    @staticmethod
    def create_shortest_path_tree2(source_node, color=None):
        """
        Creates and returns a shortest path tree rooted at source_node. Should be Dijkstra.
        :param source_node: root of the shortest path tree
        :return: the shortest path tree
        """
        memo = {}
        unvisited = []
        heapq.heappush(unvisited, (0, source_node))
        memo[source_node] = {'dist': 0, 'parent': None, 'arc': None}
        while unvisited:
            heap_elem = heapq.heappop(unvisited)
            current = heap_elem[1]
            current_dist = memo[current]['dist']
            for a in current.arcs():
                if not a.has_residual_capacity(color):
                    continue
                node = a.to_node()
                node_memo = memo.get(node)
                is_new_node = False
                if node_memo is None:
                    is_new_node = True
                    memo[node] = {'dist': float('inf')}
                    node_memo = memo[node]
                node_dist = node_memo['dist']
                tentative_dist = current_dist + a.cost()
                if tentative_dist < node_dist:
                    node_memo['dist'] = tentative_dist
                    node_memo['parent'] = current
                    node_memo['arc'] = a
                    idx = -1
                    if not is_new_node:
                        try:
                            idx = unvisited.index((node_dist, node))
                        except ValueError as ve:
                            pass
                    if idx < 0:
                        heapq.heappush(unvisited, (tentative_dist, node))
                    else:
                        unvisited[idx] = (tentative_dist, node)
                        heapq.heapify(unvisited)
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
