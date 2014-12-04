/* ASSIGN, Assignment Problem */

/* Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> */

/* The assignment problem is one of the fundamental combinatorial
   optimization problems.

   In its most general form, the problem is as follows:

   There are a number of agents and a number of tasks. Any agent can be
   assigned to perform any task, incurring some cost that may vary
   depending on the agent-task assignment. It is required to perform all
   tasks by assigning exactly one agent to each task in such a way that
   the total cost of the assignment is minimized.

   (From Wikipedia, the free encyclopedia.) */

param n, integer, > 0;  /* number of active nodes */

param r, integer, > 0;  /* number of replicas */

param s, integer, > 0;  /* slave machine factor */

param pretty, binary;   /* whether to pretty print or not */

set I := 0..(n-1);          /* set of nodes */

set R := 0..(r-1);
        /* replicas */

var x{i in I, j in I, k in R}, binary;
        /* x[i,j,k] = 1 if replica traffic may flow from node i to node j
           for the k-th replica */

s.t. sep{i in I, j in I}: sum{k in R} x[i,j,k] <= 1;
        /* different replicas don't share connections between nodes */

s.t. connout{i in I, k in R}: sum{j in I} x[i,j,k] = s;
        /* total # of outbound connections from node i for the k-th replica
           should be equal to the number of specified slave machines */

s.t. connin{j in I}: sum{i in I, k in R} x[i,j,k] = r * s;
        /* total # of inbound connections to node i for the k-th replica
           should be equal to the number of specified slave machines */

s.t. noselfrep{i in I, k in R}: x[i,i,k] = 0;        /* no self replication */

minimize obj: sum{i in I, j in I} (0);             /* minimize the excess */

solve;

for {k in R} {
   printf "replica matrix: %d\n", k;
   printf "       ";
   for {j in I}
   {
      printf "\t%s%d%s", if pretty then "n[" else "", j, if pretty then "]:" else "";
   }
   printf "\t%s\n", if pretty then "sum" else "";

   for {i in I}
   {
      printf "%s%d%s ", if pretty then "n[" else "", i, if pretty then "]:" else "";
      for {j in I} printf "\t%d", x[i,j,k];
      printf "\t%s", if pretty then sum{j in I} x[i,j,k] else "";
      printf("\n");
   }

   printf "%s    ", if pretty then "sum" else "";
   for {j in I}
   {
      printf "\t%s", if pretty then sum{i in I} x[i,j,k] else "";
   }
   printf("\n");
}

