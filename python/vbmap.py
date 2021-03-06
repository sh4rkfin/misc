import util
import model
from multidimarray import MultiDimArray
from network import Arc, Node, Network, Path
import copy
import math
import json


def get_replica_gen_model():
    """
    Returns a ModelInstance (a glpsolvable structure in GMPL format) for the
    problem of identifying the "replica networks" associated with a given data
    partitioning problem. A replica network for the i-th replica is the set of arcs
    that may have i-th replica flow.

    :return: a model instance for the replica networks generator model
    """
    model_file_name = "models/replica-map-gen-with-prev.mod"
    data_string = "data;\n" \
                  "param n := $n;\n" \
                  "param r := $r;\n" \
                  "param s := $s;\n" \
                  "param pretty := 1;\n" \
                  "param prev :=\n" \
                  "$prev_connections;\n" \
                  "end;\n"
    result_file_name_template = "result-n$n-r$r-replicagen.txt"
    m = model.Model(model_file_name, data_string)
    data_file_name_template = "cluster-n$n-r$r-replicagen.data"
    return model.ModelInstance(m, None, data_file_name_template, result_file_name_template, None)


def get_vbmap_gen_model():
    model_file_name = "models/vbmap-gen.mod"
    data_string = "data;\n" \
                  "param n := $n;\n" \
                  "param r := $r;\n" \
                  "param v := 1024;\n" \
                  "param tol := 2;\n" \
                  "param conn :=\n" \
                  "$prev_connections;\n" \
                  "end;"
    data_file_name_template = "cluster-n$n-r$r-vbmap.data"
    result_file_name_template = "result-n$n-r$r-vbmap.txt"
    m = model.Model(model_file_name, data_string)
    return model.ModelInstance(m, None, data_file_name_template, result_file_name_template, None)


def get_vbmap_gen_with_prev_model():
    model_file_name = "models/vbmap-gen-with-prev.mod"
    data_string = "data; " \
                  "param n := $n; " \
                  "param r := $r; " \
                  "param v := 1024; " \
                  "param tol := 2; " \
                  "param conn :=\n" \
                  "$prev_connections;\n" \
                  "param prev_avb := $prev_avb;\n" \
                  "param prev_rvb := $prev_rvb;\n" \
                  "param prev_x := $prev_x;\n" \
                  "end;\n"
    data_file_name_template = "cluster-n$n-r$r-vbmap.data"
    result_file_name_template = "result-n$n-r$r-vbmap.txt"
    m = model.Model(model_file_name, data_string)
    return model.ModelInstance(m, None, data_file_name_template, result_file_name_template, None)


def get_vbmap_gen_with_colors_model():
    model_file_name = "models/vbmap-color-gen-with-prev.mod"
    data_string = "data; " \
                  "param n := $n; " \
                  "param c := $c; " \
                  "param v := 1024; " \
                  "param tol := 2; " \
                  "param prev_avb :\n" \
                  "$prev_avb;\n" \
                  "param prev_rvb :\n" \
                  "$prev_rvb;\n" \
                  "end;\n"
    data_file_name_template = "cluster-n$n-colorvbmap.data"
    result_file_name_template = "result-n$n-colorvbmap.txt"
    m = model.Model(model_file_name, data_string)
    return model.ModelInstance(m, None, data_file_name_template, result_file_name_template, None)


def get_vbmap_gen_with_colors_connections_model():
    model_file_name = "models/vbmap-color-gen-with-conn.mod"
    data_string = "data;\n" \
                  "param n := $n;\n" \
                  "param c := $c;\n" \
                  "param v := 1024;\n" \
                  "param tol := 2;\n" \
                  "param prev_avb :\n" \
                  "$prev_avb;\n" \
                  "param prev_rvb :\n" \
                  "$prev_rvb;\n" \
                  "param conn :\n" \
                  "$connections;\n" \
                  "end;\n"
    data_file_name_template = "cluster-n$n-colorvbmap.data"
    result_file_name_template = "result-n$n-colorvbmap.txt"
    m = model.Model(model_file_name, data_string)
    return model.ModelInstance(m, None, data_file_name_template, result_file_name_template, None)


def read_bucket_config(filename):
    with open(filename, "r") as f:
        result = json.load(f)
        return result['vBucketServerMap']

def do_bucket_config(start_bucket_config):
    data = read_bucket_config(start_bucket_config)
    replicas = data['numReplicas']
    vb_map = data['vBucketMap']
    nodes = data['serverList']
    reps = [[0 for _ in xrange(len(nodes))] for _ in xrange(len(nodes))]
    avb = [0 for _ in xrange(len(nodes))]
    rvb = [0 for _ in xrange(len(nodes))]
    for chain in vb_map:
        ni = chain[0]
        avb[ni] += 1
        for j in xrange(replicas):
            nj = chain[1 + j]
            rvb[nj] += 1
            reps[ni][nj] += 1
    print "active vbuckets: ", avb
    print "replica vbuckets:", rvb
    print "replication flows:"
    print twod_array_to_string(reps, with_indices=True, delimiter='\t')
    return data

def calculate_cost(begin_bucket_config, end_bucket_config):
    # Note that this function only works for straight adds with 1 replica
    start = do_bucket_config(begin_bucket_config)
    end = do_bucket_config(end_bucket_config)
    node_count = max(len(start['serverList']), len(end['serverList']))
    active_moves = [[0 for _ in xrange(node_count)] for _ in xrange(node_count)]
    replica_moves = [[0 for _ in xrange(node_count)] for _ in xrange(node_count)]
    end_vb_map = end['vBucketMap']
    begin_vb_map = start['vBucketMap']
    i = 0
    for end_chain in end_vb_map:
        begin_chain = begin_vb_map[i]
        active_moves[begin_chain[0]][end_chain[0]] += 1
        replica_moves[begin_chain[1]][end_chain[1]] += 1
        i += 1
    print "active moves:"
    print twod_array_to_string(active_moves, with_indices=True, delimiter='\t', total=True)
    print "replica moves:"
    print twod_array_to_string(replica_moves, with_indices=True, delimiter='\t', total=True)
    print "active move cost: ", util.sum_off_diagonal(active_moves)
    print "replica move cost: ", util.sum_off_diagonal(replica_moves)
    print "total cost: ", util.sum_off_diagonal(active_moves) + util.sum_off_diagonal(replica_moves)


def node_compare(n1, n2):
    """ Is a comparator for nodes created by VbMapProblem.create_network that compares the nodes
        in the following order:
            prev_avb, avb, rvb, prev_rvb
        Ties are broken by comparing the keys of the nodes
    :param n1: first node to compare
    :param n2: second node to compare
    :return:
    """
    rank = {'prev_avb': 0, 'avb': 1, 'avbp': 2, 'rvbp': 3, 'rvb': 4, 'prev_rvb': 5}
    n1_key, n2_key = n1.key(), n2.key()
    r1, r2 = rank[n1_key[0]], rank[n2_key[0]]
    if r1 == r2:
        return -1 if n1_key < n2_key else 1 if n1_key > n2_key else 0
    return r1 - r2


def build_replica_networks(node_count,
                           replica_count,
                           slave_factor,
                           previous=None,
                           working_dir='.',
                           use_existing_solution=False):
    """
    Returns a 3-d structure (map of map of maps) describing the replica networks for
    a cluster of node_count size with replica_count replicas.
    :param node_count: the number of nodes
    :param replica_count: the number of replicas
    :param slave_factor: the max slaves of each node
    :param previous: the previous replica connections
    :param working_dir: the working directory
    :param use_existing_solution: whether to use an existing solution if found
    :return:
    """
    if working_dir is None:
        working_dir = '.'
    if previous is None:
        previous = [[[0 for _ in range(node_count)]
                     for _ in range(node_count)]
                    for _ in range(replica_count)]
    prev_connections_string = make_prev_connections_string(previous)
    params = dict(n=node_count,
                  r=replica_count,
                  s=slave_factor,
                  prev_connections=prev_connections_string)
    m = get_replica_gen_model()
    m.working_dir = working_dir
    m.set_use_existing_solution(use_existing_solution)
    result = m.solve(params)
    if result != 0:
        print "No solution to problem: return code is: ", result
        exit(result)
    result_file = m.get_result_file()

    # next read replication matrix
    rep = {}
    regex = "x\[(\d+),(\d+)\,(\d+)]\s*\*\s*(\d+)"

    def my_processor(m):
        val = int(m.group(4))
        if val == 0:
            return
        rk = int(m.group(3))
        if rk not in rep:
            rep[rk] = {}
        ni = int(m.group(1))
        if ni not in rep[rk]:
            rep[rk][ni] = {}
        nj = int(m.group(2))
        rep[rk][ni][nj] = val
    util.parse(result_file, regex, my_processor)
    return rep


def twod_array_to_string(array, with_indices=False, end_of_index_line="", delimiter=' ', total=False):
    result = ""
    row_count = len(array)
    if not row_count:
        return result
    col_count = len(array[0])
    result += delimiter
    if with_indices:
        for i in range(col_count):
            result += "{0}{1}".format(i, delimiter)
        if total:
            result += "total"
        result += " {0}\n".format(end_of_index_line)
    for i in range(row_count):
        if with_indices:
            result += " {0} ".format(i)
        result += delimiter
        if total:
            row_total = 0
        for val in array[i]:
            result += "{0}{1}".format(val, delimiter)
            if total:
                row_total += val
        if total:
            result += "{0}".format(row_total)
        result += '\n'
    if total:
        result += " total{0}".format(delimiter)
        for j in range(col_count):
            col_total = 0
            for i in range(row_count):
                col_total += array[i][j]
            result += "{0}{1}".format(col_total, delimiter)
        result += '\n'
    return result


def make_3d_param_string(param_3d):
    result = ""
    for k in range(len(param_3d)):
        result += "[*,*,{0}]: ".format(k)
        result += twod_array_to_string(array=param_3d[k],
                                       with_indices=True,
                                       end_of_index_line=":=")
    return result


def make_prev_connections_string(prev_connections):
    result = ""
    for k in range(len(prev_connections)):
        result += "[*,*,{0}]: ".format(k)
        result += twod_array_to_string(array=prev_connections[k],
                                       with_indices=True,
                                       end_of_index_line=":=")
    return result


def make_1d_string(array):
    result = ""
    size = len(array)
    for i in range(size):
        result += " {0} {1}".format(i, array[i])
        if i < size - 1:
            result += ","
    return result


def generate_vbmap(node_count,
                   replica_count,
                   replica_networks,
                   working_dir='.',
                   use_existing_solution=False):
    if working_dir is None:
        working_dir = '.'
    model = get_vbmap_gen_model()
    params = dict(n=node_count,
                  r=replica_count,
                  prev_connections=make_3d_param_string(replica_networks))
    model.working_dir = working_dir
    model.set_use_existing_solution(use_existing_solution)
    model.solve(params)
    return model


def generate_vbmap_with_prev(node_count,
                             replica_count,
                             replica_networks,
                             prev_avb,
                             prev_rvb,
                             prev_x,
                             working_dir='.',
                             use_existing_solution=False):
    if working_dir is None:
        working_dir = '.'
    m = get_vbmap_gen_model()
    prev_x = MultiDimArray(prev_x.values, node_count, node_count)
    params = dict(n=node_count,
                  r=replica_count,
                  prev_connections=make_3d_param_string(replica_networks),
                  prev_avb=make_1d_string(prev_avb),
                  prev_rvb=make_1d_string(prev_rvb),
                  prev_x=make_1d_string(prev_x))
    m.working_dir = working_dir
    m.set_use_existing_solution(use_existing_solution)
    m.solve(params)
    return m


class VbMapProblem:
    def __init__(self, node_count, replica_count, slave_factor, working_dir, previous=None):
        self.name = "n{0}-r{1}".format(node_count, replica_count)
        self.node_count = node_count
        self.working_dir = working_dir
        self.replica_count = replica_count
        self.slave_factor = slave_factor
        self._replica_networks = None
        self.replication_map = None
        self.previous = previous
        self.xi = None
        self.xd = None
        self.color_count = -1
        self.avb_with_colors = None
        self.rvb_with_colors = None
        self.vbmap_model = None
        self._use_exising_solution = False

    def get_replica_networks(self):
        return self._replica_networks

    def set_use_existing_solution(self, value):
        self._use_exising_solution = value

    def generate_replica_networks(self):
        """
        Generates the "replica networks" for this.
        :return:
        """
        actuals = None
        if self.previous:
            self.previous.generate_vbmap()
            actuals = self.previous.get_actual_replica_networks()
            for k in range(len(actuals)):
                util.ensure_has_capacity(actuals[k], self.node_count, lambda: [])
                for i in range(len(actuals[k])):
                    util.ensure_has_capacity(actuals[k][i], self.node_count)
        map = build_replica_networks(self.node_count,
                                     self.replica_count,
                                     self.slave_factor,
                                     actuals,
                                     self.working_dir,
                                     self._use_exising_solution)
        self._replica_networks = MultiDimArray(map, self.replica_count, self.node_count, self.node_count)

    def generate_vbmap(self):
        if not self._replica_networks:
            self.generate_replica_networks()
        if self.previous:
            avb = list(self.previous.get_active_vbuckets())
            util.ensure_has_capacity(avb, self.node_count)
            rvb = list(self.previous.get_replica_vbuckets())
            util.ensure_has_capacity(rvb, self.node_count)
            rep_map = self.previous.get_replication_map()
            self.vbmap_model = generate_vbmap_with_prev(self.node_count,
                                                        self.replica_count,
                                                        self._replica_networks,
                                                        avb,
                                                        rvb,
                                                        rep_map,
                                                        self.working_dir,
                                                        self._use_exising_solution)
        else:
            self.vbmap_model = generate_vbmap(self.node_count,
                                              self.replica_count,
                                              self._replica_networks,
                                              self.working_dir,
                                              self._use_exising_solution)

    def break_active_vbuckets_into_colors(self):
        """
        Returns a 2-d array representing the different colors of active
        vbuckets in this problem.
        E.g. if the solution the problem has the active vbuckets as
                [ 3, 0, 5 ]
        this method would return 2 arrays of:
                [ 3, 0, 0 ]
                [ 0, 0, 5 ]
        where the first array represents the active vbuckets of color 0
        and the second array the active vbuckets of color 1.

        This method is related to the similarly named method for replica
        vbuckets. To extend the previous example, let's imagine that
        2 of the 3 active vbuckets of color 1 are replicated to node 1
        and the remainder to node 2. Breaking apart the replica vbuckets
        into colors would look like:
                [ 0, 2, 1 ]
                [ 3, 2, 0 ]
        assuming a reasonable divide of the replicas for the active
        vbuckets on node 2.
        """
        if not self.avb_with_colors:
            avb = self.get_active_vbuckets()
            map = self.get_replication_map()
            a = []
            r = []
            count = 0
            for i in range(len(avb)):
                if avb[i] > 0:
                    util.ensure_has_capacity(a, count + 1, lambda: [])
                    util.ensure_has_capacity(r, count + 1, lambda: [])
                    a[count] = [0 for _ in range(self.node_count)]
                    r[count] = [0 for _ in range(self.node_count)]
                    a[count][i] = avb[i]
                    for j in range(self.node_count):
                        r[count][j] = map[i][j]
                    count += 1
                    continue
            self.color_count = count
            self.avb_with_colors = a
            self.rvb_with_colors = r
        return self.avb_with_colors

    def break_replica_vbuckets_into_colors(self):
        if not self.rvb_with_colors:
            self.break_active_vbuckets_into_colors()
        return self.rvb_with_colors

    def prev_avb(self):
        prev_avb = copy.deepcopy(self.previous.break_active_vbuckets_into_colors())
        for val in prev_avb:
            util.ensure_has_capacity(val, self.node_count)
        return prev_avb

    def prev_rvb(self):
        prev_rvb = copy.deepcopy(self.previous.break_replica_vbuckets_into_colors())
        for val in prev_rvb:
            util.ensure_has_capacity(val, self.node_count)
        return prev_rvb

    def generate_vbmap_with_colors(self):
        if self.replica_count != 1:
            raise ValueError("color vbmap generation only supported for replica count of 1")
        if not self.previous:
            self.generate_vbmap()
            return
        replica_nw = self._replica_networks[0]
        prev_avb = self.prev_avb()
        prev_rvb = self.prev_rvb()
        print "prev_avb: ", prev_avb
        params = dict(n=self.node_count,
                      c=self.previous.color_count,
                      prev_avb=twod_array_to_string(prev_avb, True, ":="),
                      prev_rvb=twod_array_to_string(prev_rvb, True, ":="),
                      connections=twod_array_to_string(replica_nw, True, ":="))
        m = get_vbmap_gen_with_colors_connections_model()
        m.working_dir = self.working_dir
        m.set_use_existing_solution(self._use_exising_solution)
        m.solve(params, ['--nomip'])
        self.vbmap_model = m

    def get_replication_map(self):
        if not self.replication_map:
            map = self.vbmap_model.get_2d_variable_as_map("x")
            self.replication_map = MultiDimArray(map, self.node_count, self.node_count)
        return self.replication_map

    def get_colored_replication_map(self):
        return self.vbmap_model.get_variable("x")

    def get_flow_increases(self):
        if not self.xi:
            map = self.vbmap_model.get_2d_variable_as_map("xi")
            self.xi = MultiDimArray(map, self.node_count, self.node_count)
        return self.xi

    def get_flow_decreases(self):
        if not self.xd:
            map = self.vbmap_model.get_2d_variable_as_map("xd")
            self.xd = MultiDimArray(map, self.node_count, self.node_count)
        return self.xd

    def get_active_vbuckets(self):
        result = self.vbmap_model.get_variable("avb")
        if util.dimension_count(result) == 2:
            result = util.accumulate(result, util.add_to)
        return result

    def get_colored_avb(self):
        if not self.is_colored_vbmap_problem():
            raise ValueError("not a colored vbmap problem")
        return self.vbmap_model.get_variable("avb")

    def get_colored_rvb(self):
        if not self.is_colored_vbmap_problem():
            raise ValueError("not a colored vbmap problem")
        return self.vbmap_model.get_variable("rvb")

    def get_replica_vbuckets(self):
        result = self.vbmap_model.get_variable("rvb")
        if util.dimension_count(result) == 2:
            result = util.accumulate(result, util.add_to)
        return result

    def is_colored_vbmap_problem(self):
        return self.vbmap_model.get_model_name().find("color") >= 0

    def get_active_vbucket_moves(self):
        return self.vbmap_model.get_variable("za")

    def get_total_active_vbucket_moves(self):
        moves = self.get_active_vbucket_moves()
        if self.is_colored_vbmap_problem():
            result = 0
            for x in moves:
                result += util.sum_off_diagonal(x)
        else:
            result = util.sum_off_diagonal(moves)
        return result

    def get_total_replica_vbucket_moves(self):
        moves = self.get_replica_vbucket_moves()
        if self.is_colored_vbmap_problem():
            result = 0
            for x in moves:
                result += util.sum_off_diagonal(x)
        else:
            result = util.sum_off_diagonal(moves)
        return result

    def create_network(self, color):
        """
        Creates a partition rebalance network for flows of a given color.

        The network looks like this:


        +-+
        |1|                                         prev_avb
        +++_____________________________
         |               |              \                 za
         v               v              v
        +-+             +-+            +-+
        |1|             |2|            |3|               avb
        +-+             +-+            +-+
                 x               x
        +-+             +-+            +-+
        |1|             |2|            |3|               rvb
        +-+             +-+            +-+
         |       x       |       x      |                 zr
         v               v              v
        +-+             +-+            +-+
        |1|             |2|            |3|          prev_rvb
        +-+             +-+            +-+

        prev_avb[i]: is the previous (or existing) active vbuckets of this color on node i
        avb[i]: will be the active vbuckets of this color on node i after the move
        rvb[i]: replica vbuckets of this color on node i
        prev_rvb[i]: the previous replica vbuckets of this color on node i
        za[i,j]: the number of active vbuckets of this color that move from node i to node j
                    as part of the solution
        zr[i,]: the number of replica vbuckets of this color that move from node i to node j

        prev_avb and prev_rvb are known and we're solving for avb, rvb, za and zr.

        No self-replication is captured by the fact that there are no links between nodes
        avb[i] and rvb[i].

        Costs of the za arcs are: za[i,j] = /  0    if i == j
                                            \  1    otherwise

        And similarly for the zr arcs. This captures the idea that we charge 1 to move one
        active vbucket to another node and 0 to leave it where it is.

        :param color:
        :param current_flow:
        :return:
        """
        if self.replica_count != 1:
            raise ValueError("can't create network with replica_count != 1")
        if not self._replica_networks:
            self.generate_replica_networks()
        network = Network()
        network.set_default_node_comparator(node_compare)
        self.add_to_network(color, network)
        return network

    def add_to_network(self, color, network):
        prev_avb = self.prev_avb()[color]
        for i, a in enumerate(prev_avb):
            if a > 0:
                n1 = network.find_or_create_node(('prev_avb', i, color))
                n1.set_source(a, color)
                for j in range(self.node_count):
                    n2 = network.find_or_create_node(('avb', j))
                    a = Arc(n2)
                    n1.add_arc(a)
                    a.set_cost(1 if i != j else 0)
        avbs = network.find_nodes_satisfying(lambda x: x.key()[0] == 'avb')
        for n1 in avbs:
            key = ('avbp', n1.key()[1])
            n2 = network.find_node(key)
            if n2 is None:
                n2 = Node(key)
                network.add_node(n2)
                arc = Arc(n2)
                arc.set_capacity(math.floor(1024.0 / len(avbs)))
                arc.set_cost(-100000)
                n1.add_arc(arc)
                n1.add_arc(Arc(n2))

        replica_network = self._replica_networks[0]
        for i, connections in enumerate(replica_network):
            for j, connection in enumerate(connections):
                if connection != 0:
                    n1 = network.find_or_create_node(('avbp', i))
                    n2 = network.find_or_create_node(('rvbp', j))
                    a = n1.get_arc(n2)
                    if not a:
                        a = Arc(n2)
                        n1.add_arc(a)
                        capacity = math.ceil(1024.0 / (self.node_count * self.slave_factor))
                        print "cap: ", capacity
                        a.set_capacity(capacity)

        rvbps = network.find_nodes_satisfying(lambda x: x.key()[0] == 'rvbp')
        for n1 in rvbps:
            key = ('rvb', n1.key()[1])
            n2 = network.find_node(key)
            if n2 is None:
                n2 = Node(key)
                network.add_node(n2)
                arc = Arc(n2)
                arc.set_capacity(math.floor(1024.0 / len(avbs)))
                arc.set_cost(-100000)
                n1.add_arc(arc)
                n1.add_arc(Arc(n2))

        prev_rvb = self.prev_rvb()[color]
        for i, r in enumerate(prev_rvb):
            if r > 0:
                n1 = network.find_or_create_node(('prev_rvb', i, color))
                for j in range(self.node_count):
                    n2 = network.find_or_create_node(('rvb', j))
                    a = Arc(n1)
                    n2.add_arc(a)
                    a.set_cost(1 if i != j else 0)
                    n1.set_source(-r, color)

    @staticmethod
    def push_toward_feasibility(network, cost_threshold=None):
        print "Network before augmentation"
        network.draw(arcs_on_separate_lines=True)
        flows = []
        result = 'feasible'
        while True:
            source_node = network.find_node_satisfying(lambda x: x.total_source() > 0)
            if source_node is None:
                ns = network.find_nodes_satisfying(lambda x: x.total_source() < 0)
                result = 'infeasible' if ns else 'feasible'
                break
            colors = network.get_non_zero_source_colors()
            path_found = False
            for color in colors:
                source_node = network.find_node_satisfying(lambda x: x.source(color) > 0)
                ns = network.find_nodes_satisfying(lambda x: x.source(color) < 0)
                if not ns:
                    result = 'infeasible'
                    break
                path, flow = network.push_min_cost_flow(source_node, color, cost_threshold)
                if path:
                    path_found = True
                    flows.append((path, flow))
            if not path_found:
                break

        print "Network after augmentation"
        print "total cost: ", network.calculate_cost()
        for p, f in flows:
            print f, ": ", p
        network.draw(arcs_on_separate_lines=True)
        return {
            'result': result,
            'flows': flows,
        }

    @staticmethod
    def augment_flows(network):
        solution = {'flows': []}
        for threshold in [-1, 0, 1, None]:
            result = VbMapProblem.push_toward_feasibility(network, threshold)
            print "total cost: ", network.calculate_cost()
            solution['result'] = result['result']
            solution['flows'] += result['flows']
        return solution

    def solve_min_cost_flow(self):
        if not self._replica_networks:
            self.generate_replica_networks()
        self.previous.break_active_vbuckets_into_colors()
        network = None
        for c in range(self.previous.color_count):
            if network is None:
                network = self.create_network(c)
            else:
                self.add_to_network(c, network)
        solution = VbMapProblem.augment_flows(network)
        self.print_solution(network, solution)

    @staticmethod
    def get_unique_nodes(network):
        nodes = network.find_nodes_satisfying(
            lambda x: x.key()[0] == 'prev_avb' or x.key()[0] == 'avb')
        return set([n.key()[1] for n in nodes])

    @staticmethod
    def find_flow_between_nodes_in_network(network, from_node_name, to_node_name, color_determiner):
        result = {}
        prev_avbs = network.find_nodes_satisfying(lambda x: x.key()[0] == from_node_name)
        unique_nodes = VbMapProblem.get_unique_nodes(network)
        count = len(unique_nodes)
        for node in prev_avbs:
            node_id = node.key()[1]
            for arc in node.arcs():
                to_node = arc.to_node()
                if to_node.key()[0] == to_node_name:
                    color = color_determiner(node, to_node)
                    zac = result.get(color)
                    if not zac:
                        result[color] = zac = [[0 for _ in xrange(count)] for _ in xrange(count)]
                    to_id = to_node.key()[1]
                    flow = arc.flow(color) if color is None else arc.total_flow()
                    zac[node_id][to_id] += flow
        return result

    def print_solution(self, network, solution):
        total_cost = network.calculate_cost()
        adjustment = 0
        avbs = network.find_nodes_satisfying(lambda x: x.key()[0] == 'avb' or x.key()[0] == 'rvbp')
        for n in avbs:
            for a in n.arcs():
                if a.cost() < 0:
                    adjustment -= a.cost() * a.total_flow()

        za = VbMapProblem.find_flow_between_nodes_in_network(network,
                                                             'prev_avb', 'avb',
                                                             lambda n1, n2: n1.key()[2])
        for color, zac in za.items():
            print "active moves for color:", color
            print twod_array_to_string(array=zac, with_indices=True, delimiter='\t', total=True)

        zr = VbMapProblem.find_flow_between_nodes_in_network(network,
                                                             'rvb', 'prev_rvb',
                                                             lambda n1, n2: n2.key()[2])
        for color, zrc in zr.items():
            print "replica moves for color:", color
            print twod_array_to_string(array=zrc, with_indices=True, delimiter='\t', total=True)

        za_total = util.accumulate(za.viewvalues(), util.add_to)
        print "total active vbucket moves:"
        print twod_array_to_string(array=za_total, with_indices=True, delimiter='\t', total=True)
        zr_total = util.accumulate(zr.viewvalues(), util.add_to)
        print "total replica vbucket moves:"
        print twod_array_to_string(array=zr_total, with_indices=True, delimiter='\t', total=True)

        x = [[0 for _ in range(self.node_count)] for _ in range(self.node_count)]
        for p, f in solution['flows']:
            from_node = p[0].to_node().key()[1]
            to_node = p[1].to_node().key()[1]
            x[from_node][to_node] += f
        print "solution result:", solution['result']
        print "total cost:", total_cost + adjustment
        print twod_array_to_string(array=x, with_indices=True, delimiter='\t')
        for p, f in solution['flows']:
            print p.sum_costs(), ",", f, ": ", p


    def create_non_zero_flow_network(self, color):
        if not self.is_colored_vbmap_problem():
            raise ValueError("VbMapProblem instance is not a colored map problem")
        network = Network()
        za = self.get_active_vbucket_moves()[color]
        for i, a in enumerate(self.prev_avb()[color]):
            if a > 0:
                node = Node(("prev_avb", i), a)
                network.add_node(node)
                for j, x in enumerate(za[i]):
                    if x > 0:
                        to_node = network.find_or_create_node(('avb', j))
                        node.add_arc(Arc(to_node, x))
        avb = self.get_colored_avb()[color]
        # print "avb: ", avb
        x = self.get_colored_replication_map()[color]
        # for i in range(len(x)):
        #     print "x[{0}]: ".format(i), x[i]
        for i, a in enumerate(avb):
            if a > 0:
                node = network.find_or_create_node(("avb", i))
                for j, y in enumerate(x[i]):
                    if y > 0:
                        to_node = network.find_or_create_node(('rvb', j))
                        node.add_arc(Arc(to_node, y))
        zr = self.get_replica_vbucket_moves()[color]
        for i, a in enumerate(self.prev_rvb()[color]):
            if a > 0:
                node = network.find_or_create_node(("prev_rvb", i))
                node.set_source(-a)
                for j in range(len(zr)):
                    y = zr[j][i]
                    if y > 0:
                        from_node = network.find_node(('rvb', j))
                        from_node.add_arc(Arc(node, y))
        return network

    def interpret(self, path, flow):
        result = []
        idx = []
        for n in path.nodes():
            idx.append(n.key[1])
        size = len(idx)
        avb_changed = idx[0] != idx[1]
        rvb_changed = idx[size - 1] != idx[size - 2]
        if not avb_changed and not rvb_changed:
            return result
        result.append("select {0} vbuckets from those currently being replicated"
                      " from node {1} to node {2}".format(flow, idx[0], idx[size - 1]))
        if avb_changed:
            result.append(" -> move the active vbuckets "
                          "from node {1} to node {2}".format(flow, idx[0], idx[1]))
        if rvb_changed:
            result.append(" -> move the replica vbuckets "
                          "from node {1} to node {2}".format(flow, idx[size - 1], idx[size - 2]))
        return result

    def make_plan_for_color(self, color):
        n = self.create_non_zero_flow_network(color)
        flows = n.break_into_flows()
        result = []
        for p, f in flows.items():
            print "flow: ", f, p
        for p, f in flows.items():
            result.extend(self.interpret(p, f))
        return result

    def make_plan(self):
        result = []
        for i in range(self.previous.color_count):
            result.extend(self.make_plan_for_color(i))
        return result

    def get_replica_vbucket_moves(self):
        return self.vbmap_model.get_variable("zr")

    def get_actual_replica_networks(self):
        rep_map = self.get_replication_map()
        result = []
        for k in range(self.replica_count):
            network = self._replica_networks[k]
            util.ensure_has_capacity(result, k + 1, lambda: [])
            for i in range(self.node_count):
                util.ensure_has_capacity(result[k], i + 1, lambda: [])
                for j in range(self.node_count):
                    util.ensure_has_capacity(result[k][i], j + 1)
                    if network[i][j] > 0 and rep_map[i][j] > 0:
                        result[k][i][j] = 1
        return result

    def print_result(self):
        rep_map = self.get_replication_map()
        for x in rep_map:
            for y in x:
                print " {0}".format(y),
            print
        for k in range(len(self._replica_networks)):
            print "replica n/w {0}".format(k)
            print twod_array_to_string(self._replica_networks[k])
        avb = self.get_active_vbuckets()
        rvb = self.get_active_vbuckets()
        for i in range(len(avb)):
            print "avb[{0}]:\t{1}".format(i, avb[i])
        for i in range(len(rvb)):
            print "avb[{0}]:\t{1}".format(i, rvb[i])
        actuals = self.get_actual_replica_networks()
        for k in range(len(actuals)):
            print "actual replica n/w {0}".format(k)
            print twod_array_to_string(actuals[k])
        za = self.get_active_vbucket_moves()
        print "za:\n{0}".format(twod_array_to_string(za))
        zr = self.get_replica_vbucket_moves()
        print "zr:\n{0}".format(twod_array_to_string(zr))
        xi = self.get_flow_increases()
        print "xi:\n{0}".format(twod_array_to_string(xi))
        xd = self.get_flow_increases()
        print "xd:\n{0}".format(twod_array_to_string(xd))