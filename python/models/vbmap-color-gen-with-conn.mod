/*
 VBucket Map Generation.

 TODO: file to be better named.
 TODO: better commented

 Try this file with ./data/10-node-2-replicas.data

*/

param n, integer, > 0;      /* number of nodes */

param v, integer > 0;       /* number of vbuckets */

param c, integer > 0;       /* number of colors */

param tol, > 0;             /* tolerance as a percentage number */

set N := 0..(n-1);          /* set of nodes */

set C := 0..(c-1);          /* set of colors */

var avb{k in C, i in N}, >= 0;      /* number of active vbuckets on each node */

s.t. tot_vbuckets: sum{k in C, i in N} avb[k,i] = v;

var rvb{k in C, i in N}, >= 0;      /* number of replica vbuckets on each node */

s.t. tot_rvbuckets: sum{k in C, i in N} rvb[k,i] = v;

var x{k in C, i in N, j in N}, integer, >= 0;
/* x[k,i,j] = 1 number of vbuckets replicated from node i to node j */

param prev_avb{k in C, i in N}, >= 0;
param prev_rvb{k in C, i in N}, >= 0;
/* param prev_x{i in N, j in N}, >= 0;      not sure if I need this */

var za{k in C, i in N, j in N}, integer >= 0;
/* movememt of active vbuckets i -> j */

var zr{k in C, i in N, j in N}, integer >= 0;
/* movememt of replica vbuckets i -> j */

param conn{i in N, j in N}, binary;
/* if there's a connection from i -> j */

s.t. conn_const{k in C, i in N, j in N}: (1 - conn[i,j]) * x[k,i,j] = 0;
/* connection constraint; only use permitted connections */

s.t. la_out{k in C, i in N}: prev_avb[k,i] = sum{j in N} za[k,i,j];
/* prev active vbuckets = sum of the active moves */

s.t. la_in{k in C, j in N}: avb[k,j] = sum{i in N} za[k,i,j];
/* prev active vbuckets = sum of the active moves */

s.t. lr_out{k in C, i in N}: rvb[k,i] = sum{j in N} zr[k,i,j];
/* prev active vbuckets = sum of the active moves */

s.t. lr_in{k in C, j in N}: prev_rvb[k,j] = sum{i in N} zr[k,i,j];
/* prev active vbuckets = sum of the active moves */

s.t. replicas_balance_out{k in C, i in N}: avb[k,i] = sum{j in N} x[k,i,j];
s.t. replicas_balance_in{k in C, j in N}:  rvb[k,j] = sum{i in N} x[k,i,j];

var ein{i in N}, >= 0;      /* excess inbound for node i */

var eout{i in N}, >= 0;     /* excess outbound for node i */

s.t. act_bal{i in N}: sum{k in C} avb[k,i] - ein[i] <= ceil(v/n);
/*  active vbuckets on each node less the excess close to ceil(v/n)  */

s.t. act_min{i in N}: sum{k in C} avb[k,i] >= floor((100 - tol) * v / (100 * n));
/*  active vbuckets on each node within 5% of max */

s.t. rep_bal{i in N}: sum{k in C} rvb[k,i] - eout[i] <= ceil(v / n);
/*  outbound replication on each node + excess less than or equal to close to v/n  */

s.t. rep_min{i in N}: sum{k in C} rvb[k,i] >= floor((100 - tol) * v / (100 * n));
/* outbound total replication (over all colors) is within tolerance of v/n  */

s.t. rep_smooth{i in N, j in N}: sum{k in C} x[k,i,j] <= ceil( v / ( n * sum{m in N}conn[i,m] ) );
/* outbound total replication (over all colors) is less than ceil (v/n)  */

/*s.t. conn_even{k in C, i in N, j in N}: x[k,i,j] * (-1 + sum{m in N}conn[i,m]) <= avb[k,i];
 add constraint to smoothly distribute replica flow to each slave from a given node and color
 don't think this constraint is needed. What matters is the total replication outbound from
 each node is approx equal on each of the replication links. */

s.t. noselfreplication{k in C, i in N}: x[k,i,i] = 0;
/* no self replication */

minimize obj: sum{i in N} (ein[i] + eout[i] +
              sum{k in C, j in N}(if i = j then 0 else (za[k,i,j] + zr[k,i,j])));
/* minimize the excess */

solve;

for {k in C} {

    printf "\n", k;
    printf "Color: %d \n", k;
    printf "---------\n", k;
    printf{i in N} "avb[%d]:\t%.1f\n", i, avb[k,i];
    printf         "sum:\t%5.1f\n", sum{i in N} avb[k,i];
    printf "\n";

    printf{i in N} "rvb[%d]:\t%.1f\n", i, rvb[k,i];
    printf         "sum:\t%4.1f\n", sum{i in N} rvb[k,i];
    printf "\n";

    for {i in N} {
       printf "pavb[%d]:\t%d\tavb[%d]:\t%d\n", i, prev_avb[k,i], i, avb[k,i];
    }

    printf "\n";
    for {i in N} {
       printf "prvb[%d]:\t%d\trvb[%d]:\t%d\n", i, prev_rvb[k,i], i, rvb[k,i];
    }

    printf "\n";
    for {i in N} {
       printf "za[%d]: ", i;
       for {j in N} printf "\t%d", za[k,i,j];
       printf("\n");
    }

    printf "\n";
    for {i in N} {
       printf "x[%d]: ", i;
       for {j in N} printf "\t%d", x[k,i,j];
       printf("\n");
    }

    printf "\n";
    for {i in N} {
       printf "zr[%d]: ", i;
       for {j in N} printf "\t%d", zr[k,i,j];
       printf("\n");
    }
}

printf "\n";
printf{i in N} "avb[%d]:\t%.1f\n", i, sum{k in C} avb[k,i];
printf "\n";
printf{i in N} "rvb[%d]:\t%.1f\n", i, sum{k in C} rvb[k,i];
printf "\n";
for {i in N} {
   printf "x[%d]: ", i;
   for {j in N} printf "\t%d", sum{k in C} x[k,i,j];
   printf("\n");
}


end;

