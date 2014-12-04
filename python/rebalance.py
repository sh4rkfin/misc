#!/usr/bin/env python

import argparse
from subprocess import call
import re

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
args = parser.parse_args()

data_string = "data; " \
              "param n := {0}; " \
              "param r := {1}; " \
              "param s := {2}; " \
              "param pretty := 1; " \
              "end;"

with open("cluster.data", "w") as text_file:
    text_file.write(data_string.format(args.n, args.r, args.s))

LPSOLVER = "/Users/dfinlay/eclipse-projects/glpk-4.35/examples/glpsol"

call([LPSOLVER, "-m", "models/basic-rebal.mod", "-d", "cluster.data", "-o", "result.txt"])

# next read replication matrix
rep = [[[0 for i in range(args.n)] for i in range(args.n)] for i in range(args.r)]
with open("result.txt", "r") as result_file:
    matcher = re.compile("x\[(\d+),(\d+)\,(\d+)]\s*\*\s*(\d+)")
    for line in result_file:
        m = matcher.search(line)
        if m:
            #print "x[{0},{1}]={2}".format(m.group(1), m.group(2), m.group(3)), " line: ", line.strip()
            rep[int(m.group(3))][int(m.group(1))][int(m.group(2))] = int(m.group(4))

idx = 0
for kdx in range(len(rep)):
    print "replica: {0}".format(kdx)
    for idx in range(len(rep[kdx])):
        print "n[{0}]:".format(idx),
        for val in rep[kdx][idx]:
            print "{0} ".format(val),
        print










