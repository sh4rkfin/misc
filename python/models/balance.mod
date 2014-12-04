/*  */

param n, integer, > 0;
/* number of nodes */

param v, integer > 0;
/* number of vbuckets */

param r, integer > 0;
/* number of replicas */

param tol, > 0;
/* number of replicas */

set N := 0..(n-1);
/* set of nodes */

set R := 0..(r-1);
/* set of replicas */

param conn{i in N, j in N, k in R}, >= 0;
/* if there's a connection from i -> j for the k-th replica */

var avb{i in N}, >= 0;
/* number of active vbuckets on each node */

s.t. tot_vbuckets: sum{i in N} avb[i] = v;

var rvb{i in N}, >= 0;
/* number of replicat vbuckets on each node */

s.t. tot_rvbuckets: sum{i in N} rvb[i] = r * v;

var x{i in N, j in N}, integer, >= 0;
/* x[i,j] = 1 number of vbuckets replicated from node i to node j */

s.t. conn_const{i in N, j in N}: (1 - sum{k in R}conn[i,j,k]) * x[i,j] = 0;
/* connection constraint; only use permitted connections */

s.t. replicas_balance_out{i in N, k in R}: sum{j in N}(conn[i,j,k] * x[i,j]) - avb[i] = 0;
s.t. replicas_balance_in{j in N}: sum{i in N} x[i,j] - rvb[j] = 0;

var ein{i in N}, >= 0;
/* excess for node i */

var eout{i in N}, >= 0;
/* excess inbound for node i */

s.t. act_bal{i in N}: avb[i] - ein[i] <= ceil(v/n);
/*  active vbuckets on each node less the excess close to ceil(v/n)  */

s.t. act_min{i in N}: avb[i] >= floor((100 - tol) * v / (100 * n));
/*  active vbuckets on each node within 5% of max */

s.t. rep_bal{i in N}: rvb[i] - eout[i] <= ceil(r * v / n);
/*  outbound replication on each node + excess less than or equal to close to v/n  */

s.t. rep_min{i in N}: rvb[i] >= floor((100 - tol) * r * v / (100 * n));
/*  outbound replication on each node + excess less than or equal to close to v/n  */

s.t. noselfreplication{i in N}: x[i,i] = 0;
/* no self replication */

minimize obj: sum{i in N} (ein[i] + eout[i]);
/* minimize the excess */

solve;

printf "\n";
printf{i in N} "active vbuckets on node %d: %4.1f\n", i, avb[i];
printf         "sum active vbuckets: %4.1f\n", sum{i in N} avb[i];
printf "\n";

printf "\n";
printf{i in N} "replica vbuckets on node %d: %4.1f\n", i, rvb[i];
printf         "sum replica vbuckets: %4.1f\n", sum{i in N} rvb[i];
printf "\n";

for {i in N}
{  
   printf "node[%d]: ", i; 
   for {j in N} printf "\t%d", x[i,j];
   printf("\n");
}

