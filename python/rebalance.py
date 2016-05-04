#!/usr/bin/env python
#
# TODO: good header comment
# TODO: handle rack / zone awareness
#

import argparse
import os
import sys
import vbmap
import util

parser = argparse.ArgumentParser(description="Models the rebalance of a Couchbase cluster")
parser.add_argument("-n", "--node-count", dest="n", type=int, help="number of nodes")
parser.add_argument("-r", "--replica-count", dest="r", type=int, help="number of replicas", default=1)
parser.add_argument("-s", "--slave-factor", dest="s", type=int, help="slaves factor", default=1)
parser.add_argument("-w", "--working", dest="working", type=str, help="working directory", default="./working")
parser.add_argument("-e", "--existing-solution", dest="existing", action='store_true', help="use existing solution",
                    default=False)
parser.add_argument("-a", "--solver-algorithm", dest="solver",
                    help="Which algorithm to use to solve the data placement problem. Options: [glpsol, custom]",
                    default='glpsol')
parser.add_argument("--begin-bucket-config", dest="begin_bucket_config")
parser.add_argument("--end-bucket-config", dest="end_bucket_config")
args = parser.parse_args()

if not args.n and not args.begin_bucket_config:
    print "Either node count or begin_bucket_config is required"
    sys.exit(1)

if not os.path.exists(args.working):
    os.makedirs(args.working)

if not os.path.isdir(args.working):
    print "path {0} exists but is not a directory, exiting"
    sys.exit(1)

if args.begin_bucket_config:
    if not args.end_bucket_config:
        vbmap.do_bucket_config(args.begin_bucket_config)
    else:
        vbmap.calculate_cost(args.begin_bucket_config, args.end_bucket_config)
else:
    use_prev = True
    prev = None
    if use_prev:
        prev = vbmap.VbMapProblem(args.n - 1, args.r, min(args.s, args.n - 2), args.working)
        prev.set_use_existing_solution(args.existing)
        prev.generate_replica_networks()
        prev.generate_vbmap()
        prev.print_result()

    problem = vbmap.VbMapProblem(args.n, args.r, args.s, args.working, prev)
    problem.set_use_existing_solution(args.existing)
    problem.generate_replica_networks()

    if args.solver == 'custom':
        problem.solve_min_cost_flow()
    else:
        problem.generate_vbmap_with_colors()
        problem.print_result()

        print "active moves: ", problem.get_total_active_vbucket_moves()
        print "replica moves: ", problem.get_total_replica_vbucket_moves()

        print "color count:", problem.previous.color_count
        plan = problem.make_plan()
        for p in plan:
            print p

        print "active vbuckets"
        avb = util.accumulate(problem.get_active_vbucket_moves(), util.add_to)
        print vbmap.twod_array_to_string(array=avb, with_indices=True, delimiter='\t')

        print "flows"
        x = problem.get_colored_replication_map()
        x_agg = util.accumulate(x, util.add_to)
        print vbmap.twod_array_to_string(x_agg, True, '', '\t')

        print "replica vbuckets"
        rvb = util.accumulate(problem.get_replica_vbucket_moves(), util.add_to)
        print vbmap.twod_array_to_string(rvb, True, '', '\t')
