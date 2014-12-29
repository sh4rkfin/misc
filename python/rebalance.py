#!/usr/bin/env python
#
# TODO: good header comment
# TODO: handle rack / zone awareness
#

import argparse
import os
import sys
from vbmap import VbMapProblem
import util

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes", required=True)
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
parser.add_argument("-w", "--working", dest="working", type=str, help="working directory", default="./working")
parser.add_argument("-e", "--existing-solution", dest="existing", action='store_true', help="use existing solution",
                    default=False)
args = parser.parse_args()


if not os.path.exists(args.working):
    os.makedirs(args.working)

if not os.path.isdir(args.working):
    print "path {0} exists but is not a directory, exiting"
    sys.exit(1)

use_prev = True
prev = None
if use_prev:
    prev = VbMapProblem(args.n - 1, args.r, min(args.s, args.n - 2), args.working)
    prev.set_use_existing_solution(args.existing)
    prev.generate_replica_networks()
    prev.generate_vbmap()
    prev.print_result()
problem = VbMapProblem(args.n, args.r, args.s, args.working, prev)
problem.set_use_existing_solution(args.existing)
problem.generate_replica_networks()
problem.generate_vbmap_with_colors()
# problem.print_result()

print "active moves: ", problem.get_total_active_vbucket_moves()
print "replica moves: ", problem.get_total_replica_vbucket_moves()

rank = {'prev_avb': 0, 'avb': 1, 'rvb': 2, 'prev_rvb': 3}


def compare(n1, n2):
    r1, r2 = rank[n1.key[0]], rank[n2.key[0]]
    if r1 == r2:
        return -1 if n1.key < n2.key else 1 if n1.key > n2.key else 0
    return r1 - r2

print "color count:", problem.previous.color_count
print "creating network for color:", problem.previous.color_count - 1
n = problem.create_network(problem.previous.color_count - 1)
n.draw(compare)
n.break_into_flows()
print "active vbuckets"
avb = problem.get_colored_avb()
for a in avb:
    print a
avb = problem.get_active_vbuckets()
print avb

print "flows"
x = problem.get_colored_replication_map()
x_agg = util.accumulate(x, util.add_to)
for a in x_agg:
    print a

print "replica vbuckets"
rvb = problem.get_replica_vbuckets()
print rvb
rvb = problem.get_colored_rvb()
for a in rvb:
    print a
