#!/usr/bin/env python

import argparse
from subprocess import call
import re

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
args = parser.parse_args()

LPSOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"

def build_replica_networks ():
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param s := {2}; " \
                  "param pretty := 1; " \
                  "end;"

    with open("cluster.data", "w") as text_file:
        text_file.write(data_string.format(args.n, args.r, args.s))

    call([LPSOLVER, "-m", "models/replica-map-gen.mod", "-d", "cluster.data", "-o", "result.txt"])

    # next read replication matrix
    rep = [[[0 for i in range(args.n)] for i in range(args.n)] for i in range(args.r)]
    with open("result.txt", "r") as result_file:
        matcher = re.compile("x\[(\d+),(\d+)\,(\d+)]\s*\*\s*(\d+)")
        for line in result_file:
            m = matcher.search(line)
            if m:
                #print "x[{0},{1}]={2}".format(m.group(1), m.group(2), m.group(3)), " line: ", line.strip()
                rep[int(m.group(3))][int(m.group(1))][int(m.group(2))] = int(m.group(4))
    return rep

def make_replica_networks_string (replica_networks):
    str = ""
    for kdx in range(len(replica_networks)):
        str += "[*,*,{0}]: ".format(kdx)
        replica_nw = replica_networks[kdx]
        node_count = len(replica_nw)
        for idx in range(node_count):
            str += "{0} ".format(idx)
        str += ' :=\n'
        for idx in range(node_count):
            str += " {0} ".format(idx)
            for val in replica_nw[idx]:
                str += "{0} ".format(val)
            str += '\n'
    return str

def generate_vbmap (replica_networks):
    data_string = "data; " \
                  "param n := {0}; " \
                  "param r := {1}; " \
                  "param v := 1024; " \
                  "param tol := 2; " \
                  "param conn :=\n" \
                  "{2};\n" \
                  "end;"

    with open("cluster2.data", "w") as text_file:
        text_file.write(data_string.format(args.n, args.r, make_replica_networks_string(replica_networks)))

    call([LPSOLVER, "-m", "models/balance.mod", "-d", "cluster2.data", "-o", "result2.txt"])

replica_networks = build_replica_networks()
#print make_replica_networks_string(replica_networks)
generate_vbmap(replica_networks)












