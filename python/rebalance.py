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
import re

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
args = parser.parse_args()

SOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"

def parse(filename, regex, processor):
    with open(filename, "r") as file:
        matcher = re.compile(regex)
        for line in file:
            m = matcher.search(line)
            if m:
                processor(m)

def build_replica_networks(node_count, replica_count, slave_factor, prev=None):
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param s := {2}; " \
                  "param pretty := 1; " \
                  "param prev :=\n" \
                  "{3};" \
                  "end;\n"
    if prev == None:
        prev = [[[0 for i in range(node_count)]
                    for i in range(node_count)]
                    for i in range(replica_count)]

    cluster_file = "cluster-n{0}-r{1}-replicagen.data".format(node_count, replica_count)
    with open(cluster_file, "w") as text_file:
        text_file.write(data_string.format(node_count, replica_count, slave_factor,
                                           make_prev_connections_string(prev)))

    result_file = "result-n{0}-r{1}-replicagen.txt".format(node_count, replica_count)
    call([SOLVER,
          "-m", "models/replica-map-gen-with-prev.mod",
          "-d", cluster_file,
          "-o", result_file])

    # next read replication matrix
    rep = [[[0 for i in range(node_count)]
               for i in range(node_count)]
               for i in range(replica_count)]
    regex = "x\[(\d+),(\d+)\,(\d+)]\s*\*\s*(\d+)"
    def my_processor(m):
        rep[int(m.group(3))][int(m.group(1))][int(m.group(2))] = int(m.group(4))
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

def twod_array_to_string(array, with_indices=False, end_of_index_line=""):
    str = ""
    size = len(array)
    if with_indices:
        for i in range(size):
            str += "{0} ".format(i)
        str += " {0}\n".format(end_of_index_line)
    for i in range(size):
        if with_indices:
            str += " {0} ".format(i)
        for val in array[i]:
            str += " {0}".format(val)
        str += '\n'
    return str

def make_replica_networks_string(replica_networks):
    str = ""
    for k in range(len(replica_networks)):
        str += "[*,*,{0}]: ".format(k)
        str += twod_array_to_string(array=replica_networks[k],
                                    with_indices=True,
                                    end_of_index_line=":=")
    return str

def make_prev_connections_string(prev_connections):
    str = ""
    for k in range(len(prev_connections)):
        str += "[*,*,{0}]: ".format(k)
        str += twod_array_to_string(array=prev_connections[k],
                                    with_indices=True,
                                    end_of_index_line=":=")
    return str

def make_1d_string (array):
    str = ""
    size = len(array)
    for i in range(size):
        str += " {0} {1}".format(i, array[i])
        if i < size - 1:
            str += ","
    return str

def generate_vbmap (node_count, replica_count, replica_networks, result_file, prev_avb=None, prev_rvb=None):
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param v := 1024; " \
                  "param tol := 2; " \
                  "param conn :=\n" \
                  "{2};\n"
    cluster_file = "cluster-n{0}-r{1}-vbmap.data".format(node_count, replica_count)
    with open(cluster_file, "w") as text_file:
        text_file.write(data_string.format(node_count,
                                           replica_count,
                                           make_replica_networks_string(replica_networks)))
        if not prev_avb == None:
            print "prev_avb: {0}".format(prev_avb)
            text_file.write("param prev_avb := {0};\n".format(make_1d_string(prev_avb)))
            text_file.write("param prev_rvb := {0};\n".format(make_1d_string(prev_rvb)))
        text_file.write("end;\n")

    model = "models/vbmap-gen.mod" if prev_avb == None else "models/vbmap-gen-with-prev.mod";
    result = call([SOLVER,
                   "-m", model,
                   "-d", cluster_file,
                   "-o", result_file])
    # TODO: handle solver failure
    return result

class VbMapProblem:
    def __init__(self, node_count, replica_count, slave_factor, previous=None):
        self.name = "n{0}-r{1}".format(node_count, replica_count)
        self.result_file = "result-{0}-vbmap.txt".format(self.name)
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

    def generate_replica_networks(self):
        actuals = None
        if self.previous:
            self.previous.generate_vbmap()
            actuals = self.previous.get_actual_replica_networks()
            for k in range(len(actuals)):
                ensure_has_capacity(actuals[k], self.node_count, lambda:[])
                for i in range(len(actuals[k])):
                    ensure_has_capacity(actuals[k][i], self.node_count, lambda:0)
        self.replica_networks = build_replica_networks(self.node_count,
                                                       self.replica_count,
                                                       self.slave_factor,
                                                       actuals)
    def generate_vbmap(self):
        if not self.replica_networks:
            self.generate_replica_networks()
        avb = None
        rvb = None
        if self.previous:
            avb = self.previous.get_active_vbuckets()
            ensure_has_capacity(avb, self.node_count, lambda: 0)
            rvb = self.previous.get_replica_vbuckets()
            ensure_has_capacity(rvb, self.node_count, lambda: 0)
        generate_vbmap(self.node_count, self.replica_count, self.replica_networks, self.result_file,
                       avb, rvb)

    def read_replication_map(self):
        if not self.replication_map:
            self.replication_map = read_2d_variable(self.result_file, "x")

    def get_replication_map(self):
        self.read_replication_map()
        return self.replication_map

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
        self.read_replication_map()
        result = []
        for k in range(len(self.replica_networks)):
            network = self.replica_networks[k]
            ensure_has_capacity(result, k + 1, lambda: [])
            for i in range(len(network)):
                ensure_has_capacity(result[k], i + 1, lambda: [])
                for j in range(len(network[i])):
                    ensure_has_capacity(result[k][i], j + 1, lambda: 0)
                    if network[i][j] > 0 and self.replication_map[i][j] > 0:
                        result[k][i][j] = 1
        return result

    def print_result(self):
        self.read_replication_map()
        for i in range(len(self.replication_map)):
            reps = self.replication_map[i]
            for val in reps:
                print " {0}".format(val),
            print
        self.read_active_vbuckets()
        self.read_replica_vbuckets()
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


previous = VbMapProblem(args.n - 1, args.r, args.s)
problem  = VbMapProblem(args.n, args.r, args.s, previous)
problem.generate_replica_networks()
problem.generate_vbmap()
problem.print_result()












