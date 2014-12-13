#!/usr/bin/env python
#
# TODO: good header comment
# TODO: convert all these nasty multi-dimensional array to maps
# TODO: handle rack / zone awareness
# TODO: put all generated files in common sub-directory
# TODO: need to figure out how to deal with vbucket identity -
#       may need to cost the difference in the flows in addition to / aswell as
#       the difference in the number of vbuckets

import argparse
from subprocess import call
import os
import sys
import re

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
parser.add_argument("-w", "--working", dest="working", type=str, help="working directory", default="./working")
args = parser.parse_args()

SOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"


def parse(filename, regex, processor):
    with open(filename, "r") as f:
        matcher = re.compile(regex)
        for line in f:
            m = matcher.search(line)
            if m:
                processor(m)


def build_replica_networks(node_count, replica_count, slave_factor, previous=None):
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param s := {2}; " \
                  "param pretty := 1; " \
                  "param prev :=\n" \
                  "{3};" \
                  "end;\n"
    if previous is None:
        previous = [[[0 for _ in range(node_count)]
                    for _ in range(node_count)]
                    for _ in range(replica_count)]

    cluster_file = "{0}/cluster-n{1}-r{2}-replicagen.data".format(args.working, node_count, replica_count)
    with open(cluster_file, "w") as text_file:
        text_file.write(data_string.format(node_count, replica_count, slave_factor,
                                           make_prev_connections_string(previous)))

    result_file = "{0}/result-n{1}-r{2}-replicagen.txt".format(args.working, node_count, replica_count)
    call([SOLVER,
          "-m", "models/replica-map-gen-with-prev.mod",
          "-d", cluster_file,
          "-o", result_file])

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
    parse(result_file, regex, my_processor)
    return rep


def ensure_has_capacity(array, size, default_value):
    current_size = len(array)
    if size > current_size:
        for _ in range(size - current_size):
            array.append(default_value())


def read_1d_variable(filename, var_name):
    result = []
    regex = "{0}\[(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        row = int(m.group(1))
        ensure_has_capacity(result, row + 1, lambda: 0)
        result[row] = int(m.group(3))
    parse(filename, regex, my_proc)
    return result


def read_2d_variable(filename, var_name):
    result = []
    regex = "{0}\[(\d+),(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        row = int(m.group(1))
        ensure_has_capacity(result, row + 1, lambda: [])
        array = result[row]
        col = int(m.group(2))
        ensure_has_capacity(array, col + 1, lambda: 0)
        result[row][col] = int(m.group(4))
    parse(filename, regex, my_proc)
    return result


def read_2d_variable_as_map(filename, var_name):
    result = {}
    regex = "{0}\[(\d+),(\d+)\]\s*(\*)?\s*(\d+)".format(var_name)

    def my_proc(m):
        value = int(m.group(4))
        if value == 0:
            return
        row = int(m.group(1))
        col = int(m.group(2))
        if row not in result:
            result[row] = {}
        result[row][col] = value
    parse(filename, regex, my_proc)
    return result


def twod_array_to_string(array, with_indices=False, end_of_index_line=""):
    result = ""
    size = len(array)
    if with_indices:
        for i in range(size):
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


def generate_vbmap(node_count, replica_count, replica_networks, result_file,
                   prev_avb=None, prev_rvb=None, prev_x=None):
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param v := 1024; " \
                  "param tol := 2; " \
                  "param conn :=\n" \
                  "{2};\n"
    cluster_file = "{0}/cluster-n{1}-r{2}-vbmap.data".format(args.working, node_count, replica_count)
    with open(cluster_file, "w") as text_file:
        text_file.write(data_string.format(node_count,
                                           replica_count,
                                           make_3d_param_string(replica_networks)))
        if prev_avb is not None:
            print "prev_avb: {0}".format(prev_avb)
            text_file.write("param prev_avb := {0};\n".format(make_1d_string(prev_avb)))
            text_file.write("param prev_rvb := {0};\n".format(make_1d_string(prev_rvb)))
            prev_x = MultiDimArray(prev_x.values, node_count, node_count)
            text_file.write("param prev_x :{0};".format(twod_array_to_string(prev_x, True, ":=")))
        text_file.write("end;\n")

    model = "models/vbmap-gen.mod" if prev_avb is None else "models/vbmap-gen-with-prev.mod"
    result = call([SOLVER,
                   "-m", model,
                   "-d", cluster_file,
                   "-o", result_file])
    # TODO: handle solver failure
    return result

class MultiDimArray:
    def __init__(self, values, *dims):
        self.dim_count = len(dims)
        self.dims = dims
        self.values = values

    def __len__(self):
        return self.dims[0]

    def __iter__(self):
        for x in range(self.dims[0]):
            yield self[x]

    def __getitem__(self, coord):
        if coord in self.values:
            value = self.values[coord]
            if self.dim_count == 1:
                return value
            return MultiDimArray(value, *self.dims[1:])
        if self.dim_count == 1:
            return 0
        return EmptyMultiDimArray.get_instance(self.dims[1:])

    def __setitem__(self, coord):
        raise Exception("Instances of {0} cannot be set".format(self.__class__.__name__))


class EmptyMultiDimArray(MultiDimArray):
    instances = {}

    @staticmethod
    def get_instance(dimensions):
        if dimensions < 256:
            if dimensions not in EmptyMultiDimArray.instances:
                EmptyMultiDimArray.instances[dimensions] = EmptyMultiDimArray(*dimensions)
            return EmptyMultiDimArray.instances[dimensions]
        return EmptyMultiDimArray(*dimensions)

    def __init__(self, *dims):
        MultiDimArray.__init__(self, None, *dims)

    def __getitem__(self, _):
        if self.dim_count > 1:
            return EmptyMultiDimArray.get_instance(self.dims[1:])
        return 0


class VbMapProblem:
    def __init__(self, node_count, replica_count, slave_factor, previous=None):
        self.name = "n{0}-r{1}".format(node_count, replica_count)
        self.result_file = "{0}/result-{1}-vbmap.txt".format(args.working, self.name)
        self.node_count = node_count
        self.replica_count = replica_count
        self.slave_factor = slave_factor
        self.replica_networks = None
        self.replication_map = None
        self.avb = None
        self.rvb = None
        self.za = None
        self.zr = None
        self.previous = previous
        self.xi = None
        self.xd = None

    def generate_replica_networks(self):
        actuals = None
        if self.previous:
            self.previous.generate_vbmap()
            actuals = self.previous.get_actual_replica_networks()
            for k in range(len(actuals)):
                ensure_has_capacity(actuals[k], self.node_count, lambda: [])
                for i in range(len(actuals[k])):
                    ensure_has_capacity(actuals[k][i], self.node_count, lambda: 0)
        map = build_replica_networks(self.node_count, self.replica_count, self.slave_factor, actuals)
        self.replica_networks = MultiDimArray(map, self.replica_count, self.node_count, self.node_count)

    def generate_vbmap(self):
        if not self.replica_networks:
            self.generate_replica_networks()
        avb = None
        rvb = None
        rep_map = None
        if self.previous:
            avb = self.previous.get_active_vbuckets()
            ensure_has_capacity(avb, self.node_count, lambda: 0)
            rvb = self.previous.get_replica_vbuckets()
            ensure_has_capacity(rvb, self.node_count, lambda: 0)
            rep_map = self.previous.get_replication_map()
        generate_vbmap(self.node_count, self.replica_count, self.replica_networks, self.result_file,
                       avb, rvb, rep_map)

    def get_replication_map(self):
        if not self.replication_map:
            map = read_2d_variable_as_map(self.result_file, "x")
            self.replication_map = MultiDimArray(map, self.node_count, self.node_count)
        return self.replication_map

    def get_flow_increases(self):
        if not self.xi:
            map = read_2d_variable_as_map(self.result_file, "xi")
            self.xi = MultiDimArray(map, self.node_count, self.node_count)
        return self.xi

    def get_flow_decreases(self):
        if not self.xd:
            map = read_2d_variable_as_map(self.result_file, "xd")
            self.xd = MultiDimArray(map, self.node_count, self.node_count)
        return self.xd

    def read_active_vbuckets(self):
        if not self.avb:
            self.avb = read_1d_variable(self.result_file, "avb")

    def get_active_vbuckets(self):
        self.read_active_vbuckets()
        return list(self.avb)

    def get_replica_vbuckets(self):
        self.read_replica_vbuckets()
        return list(self.rvb)

    def read_replica_vbuckets(self):
        if not self.rvb:
            self.rvb = read_1d_variable(self.result_file, "rvb")

    def get_active_vbucket_moves(self):
        if not self.za:
            self.za = read_2d_variable(self.result_file, "za")
        return self.za

    def get_replica_vbucket_moves(self):
        if not self.zr:
            self.zr = read_2d_variable(self.result_file, "zr")
        return self.zr

    def get_actual_replica_networks(self):
        rep_map = self.get_replication_map()
        result = []
        for k in range(self.replica_count):
            network = self.replica_networks[k]
            ensure_has_capacity(result, k + 1, lambda: [])
            for i in range(self.node_count):
                ensure_has_capacity(result[k], i + 1, lambda: [])
                for j in range(self.node_count):
                    ensure_has_capacity(result[k][i], j + 1, lambda: 0)
                    if network[i][j] > 0 and rep_map[i][j] > 0:
                        result[k][i][j] = 1
        return result

    def print_result(self):
        self.read_active_vbuckets()
        self.read_replica_vbuckets()
        rep_map = self.get_replication_map()
        for x in rep_map:
            for y in x:
                print " {0}".format(y),
            print
        for k in range(len(self.replica_networks)):
            print "replica n/w {0}".format(k)
            print twod_array_to_string(self.replica_networks[k])
        for i in range(len(self.avb)):
            print "avb[{0}]:\t{1}".format(i, self.avb[i])
        for i in range(len(self.rvb)):
            print "avb[{0}]:\t{1}".format(i, self.rvb[i])
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

prev = None
if False:
    prev = VbMapProblem(args.n - 1, args.r, args.s)
    prev.generate_replica_networks()
    prev.generate_vbmap()
    prev.print_result()
problem = VbMapProblem(args.n, args.r, args.s, None)
problem.generate_replica_networks()
problem.generate_vbmap()
problem.print_result()

