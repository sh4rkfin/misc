#!/usr/bin/env python
#
# TODO: good header comment
# TODO: convert all these nasty multi-dimensional array to maps (maybe)
# TODO: handle rack / zone awareness
# TODO: need to figure out how to deal with vbucket identity -
#       may need to cost the difference in the flows in addition to / aswell as
#       the difference in the number of vbuckets

import argparse
from multidimarray import MultiDimArray
import os
import sys
import util
import model
import copy
from network import Arc, Node, Network
from copy import deepcopy

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
parser.add_argument("-w", "--working", dest="working", type=str, help="working directory", default="./working")
args = parser.parse_args()

def get_replica_gen_model():
    model_file_name = "models/replica-map-gen-with-prev.mod"
    data_string = "data;\n" \
                  "param n := $n;\n" \
                  "param r := $r;\n" \
                  "param s := $s;\n" \
                  "param pretty := 1;\n" \
                  "param prev :=\n" \
                  "$prev_connections;\n" \
                  "end;\n"
    data_file_name_template = "cluster-n$n-r$r-replicagen.data"
    result_file_name_template = "result-n$n-r$r-replicagen.txt"
    m = model.Model(model_file_name, data_string)
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


def build_replica_networks(node_count, replica_count, slave_factor, previous=None):
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
    m.working_dir = args.working
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


def twod_array_to_string(array, with_indices=False, end_of_index_line=""):
    result = ""
    size = len(array)
    if with_indices:
        for i in range(len(array[0])):
            result += "{0} ".format(i)
        result += " {0}\n".format(end_of_index_line)
    for i in range(size):
        if with_indices:
            result += " {0} ".format(i)
        for val in array[i]:
            result += " {0}".format(val)
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


def generate_vbmap(node_count, replica_count, replica_networks):
    model = get_vbmap_gen_model()
    params = dict(n=node_count,
                  r=replica_count,
                  prev_connections=make_3d_param_string(replica_networks))
    model.working_dir = args.working
    model.solve(params)
    return model


def generate_vbmap_with_prev(node_count, replica_count, replica_networks, prev_avb, prev_rvb, prev_x):
    m = get_vbmap_gen_model()
    prev_x = MultiDimArray(prev_x.values, node_count, node_count)
    params = dict(n=node_count,
                  r=replica_count,
                  prev_connections=make_3d_param_string(replica_networks),
                  prev_avb=make_1d_string(prev_avb),
                  prev_rvb=make_1d_string(prev_rvb),
                  prev_x=make_1d_string(prev_x))
    m.working_dir = args.working
    m.solve(params)
    return m


class VbMapProblem:
    def __init__(self, node_count, replica_count, slave_factor, working_dir, previous=None):
        self.name = "n{0}-r{1}".format(node_count, replica_count)
        self.node_count = node_count
        self.working_dir = working_dir
        self.replica_count = replica_count
        self.slave_factor = slave_factor
        self.replica_networks = None
        self.replication_map = None
        self.previous = previous
        self.xi = None
        self.xd = None
        self.color_count = -1
        self.avb_with_colors = None
        self.rvb_with_colors = None
        self.vbmap_model = None

    def generate_replica_networks(self):
        actuals = None
        if self.previous:
            self.previous.generate_vbmap()
            actuals = self.previous.get_actual_replica_networks()
            for k in range(len(actuals)):
                util.ensure_has_capacity(actuals[k], self.node_count, lambda: [])
                for i in range(len(actuals[k])):
                    util.ensure_has_capacity(actuals[k][i], self.node_count)
        map = build_replica_networks(self.node_count, self.replica_count, self.slave_factor, actuals)
        self.replica_networks = MultiDimArray(map, self.replica_count, self.node_count, self.node_count)

    def generate_vbmap(self):
        if not self.replica_networks:
            self.generate_replica_networks()
        if self.previous:
            avb = self.previous.get_active_vbuckets()
            util.ensure_has_capacity(avb, self.node_count)
            rvb = self.previous.get_replica_vbuckets()
            util.ensure_has_capacity(rvb, self.node_count)
            rep_map = self.previous.get_replication_map()
            self.vbmap_model = generate_vbmap_with_prev(self.node_count,
                                                        self.replica_count,
                                                        self.replica_networks,
                                                        avb,
                                                        rvb,
                                                        rep_map)
        else:
            self.vbmap_model = generate_vbmap(self.node_count, self.replica_count, self.replica_networks)

    def break_active_vbuckets_into_colors(self):
        """
        Returns a 2-d array representing the different colors of active
        vbuckets in this problem.
        E.g. if the solution the problem has the active vbuckets as
                [ 3, 0, 5 ]
        this methiod would return 2 arrays of:
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
        prev_avb = self.previous.break_active_vbuckets_into_colors()
        for val in prev_avb:
            util.ensure_has_capacity(val, self.node_count)
        return prev_avb

    def prev_rvb(self):
        prev_rvb = self.previous.break_replica_vbuckets_into_colors()
        for val in prev_rvb:
            util.ensure_has_capacity(val, self.node_count)
        return prev_rvb

    def generate_vbmap_with_colors(self):
        if not self.previous:
            self.generate_vbmap()
            return
        prev_avb = self.prev_avb()
        prev_rvb = self.prev_rvb()
        params = dict(n=self.node_count,
                      c=self.previous.color_count,
                      prev_avb=twod_array_to_string(prev_avb, True, ":="),
                      prev_rvb=twod_array_to_string(prev_rvb, True, ":="))
        m = get_vbmap_gen_with_colors_model()
        m.working_dir = self.working_dir
        m.solve(params)
        self.vbmap_model = m

    def get_replication_map(self):
        if not self.replication_map:
            map = self.vbmap_model.get_2d_variable_as_map("x")
            self.replication_map = MultiDimArray(map, self.node_count, self.node_count)
        return self.replication_map

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
        return list(self.vbmap_model.get_variable("avb"))

    def get_replica_vbuckets(self):
        return list(self.vbmap_model.get_variable("rvb"))

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
        if not self.is_colored_vbmap_problem():
            raise ValueError("VbMapProblem instance is not a colored map problem")
        network = Network()
        za = self.get_active_vbucket_moves()[color]
        for i, a in enumerate(self.prev_avb()):
            node = Node(("prev_avb", i))
            network.add_node(node)
            for j, x in enumerate(za[i]):
                if x > 0:
                    to_node = Node(('avb', j))
                    node.add_arc(Arc(to_node, x))
        for i, a in enumerate(self.get_active_vbuckets()):
            key = ("prev_avb", i)
            node = Node(key)
            network.add_node(node)
            for j, x in enumerate(za[i]):
                if x > 0:
                    to_node = Node(('avb', j))
                    node.add_arc(Arc(to_node, x))

    # Network looks like follows:
    #
    #   prev_avb
    #   avb
    #   rvb
    #   prev_rvb
    #
    def break_down_flows(self, color):
        za = copy.deepcopy(self.get_active_vbucket_moves()[color])
        path = []
        for i, x in enumerate(za):
            for j, y in enumerate(x):
                if y > 0:
                    pass

    def get_replica_vbucket_moves(self):
        return self.vbmap_model.get_variable("zr")

    def get_actual_replica_networks(self):
        rep_map = self.get_replication_map()
        result = []
        for k in range(self.replica_count):
            network = self.replica_networks[k]
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
        for k in range(len(self.replica_networks)):
            print "replica n/w {0}".format(k)
            print twod_array_to_string(self.replica_networks[k])
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

if not os.path.exists(args.working):
    os.makedirs(args.working)

if not os.path.isdir(args.working):
    print "path {0} exists but is not a directory, exiting"
    sys.exit(1)

use_prev = True
prev = None
if use_prev:
    prev = VbMapProblem(args.n - 1, args.r, min(args.s, args.n - 2), args.working)
    prev.generate_replica_networks()
    prev.generate_vbmap()
    prev.print_result()
problem = VbMapProblem(args.n, args.r, args.s, args.working, prev)
problem.generate_replica_networks()
problem.generate_vbmap_with_colors()
#problem.print_result()

print "active moves: ", problem.get_total_active_vbucket_moves()
print "replica moves: ", problem.get_total_replica_vbucket_moves()

