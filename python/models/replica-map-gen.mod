/*
 Replica network generation.

 TODO: comment this better

 Try this model out as follows:
    glpsol -m models/replica-map-gen.mod -d data/9-node.data
 
*/

param n, integer, > 0;  /* number of active nodes */

param r, integer, > 0;  /* number of replicas */

param s, integer, > 0;  /* slave machine factor */

param pretty, binary;   /* whether to pretty print or not */

set N := 0..(n-1);          /* set of nodes */

set R := 0..(r-1);
        /* replicas */

var x{i in N, j in N, k in R}, binary;
        /* x[i,j,k] = 1 if replica traffic may flow from node i to node j
           for the k-th replica */

s.t. sep{i in N, j in N}: sum{k in R} x[i,j,k] <= 1;
        /* different replicas don't share connections between nodes */

s.t. connout{i in N, k in R}: sum{j in N} x[i,j,k] = s;
        /* total # of outbound connections from node i for the k-th replica
           should be equal to the number of specified slave machines */

s.t. connin{j in N}: sum{i in N, k in R} x[i,j,k] = r * s;
        /* total # of inbound connections to node i for the k-th replica
           should be equal to the number of specified slave machines */

s.t. noselfrep{i in N, k in R}: x[i,i,k] = 0;        /* no self replication */

minimize obj: sum{i in N, j in N} (0);             /* degenerate minimization = simple feasibility */

solve;

for {k in R} {
   printf "replica matrix: %d\n", k;
   printf "       ";
   for {j in N}
   {
      printf "\t%s%d%s", if pretty then "n[" else "", j, if pretty then "]:" else "";
   }
   printf "\t%s\n", if pretty then "sum" else "";

   for {i in N}
   {
      printf "%s%d%s ", if pretty then "n[" else "", i, if pretty then "]:" else "";
      for {j in N} printf "\t%d", x[i,j,k];
      printf "\t%s", if pretty then sum{j in N} x[i,j,k] else "";
      printf("\n");
   }

   printf "%s    ", if pretty then "sum" else "";
   for {j in N}
   {
      printf "\t%s", if pretty then sum{i in N} x[i,j,k] else "";
   }
   printf("\n");
}

