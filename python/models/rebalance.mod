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

param m, integer, > 0;
/* number of total nodes */

param n, integer, > 0;
/* number of active nodes */

param r, integer, > 0;
/* number of replicas */

param pretty, binary;

set I := 1..n;
/* set of nodes */

set T := 1..m;
/* set of total nodes */

set R := 1..r;
/* set of replicas */

param prev{i in I, j in I}, >= 0;
/* cost of allocating task j to agent i */

var x{i in T, j in T}, binary;
/* x[i,j] = 1 if replications going from node i to node j */

var s1{i in T, j in T}, integer, >= 0;
/* x[i,j] = 1 if replications going from node i to node j */

var s2{i in T, j in T}, binary, >= 0;
/* x[i,j] = 1 if replications going from node i to node j */

s.t. totrep{i in I}: sum{j in I} x[i,j] = r;
/* total amount of replication = v */

s.t. diff1{i in I, j in I}: x[i,j] - prev[i,j] - s1[i,j] <= 0;
/* */

s.t. diff2{i in I, j in I}: prev[i,j] - x[i,j] - s2[i,j] <= 0;
/* */

s.t. totrepin{j in I}: sum{i in I} x[i,j] = r;
/* total amount of replication = v */

s.t. noselfreplication{i in I}: x[i,i] = 0;
/* no self replication */

minimize obj: sum{i in I, j in I} (s1[i,j] + s2[i,j]);
/* minimize the excess */

solve;

printf "replica matrix\n";
printf "       "; 
for {j in T}
{  
   printf "\t%s%d%s", if pretty then "n[" else "", j, if pretty then "]:" else "";
}
printf "\t%s\n", if pretty then "sum" else "";

for {i in T}
{   
   printf "%s%d%s ", if pretty then "n[" else "", i, if pretty then "]:" else ""; 
   for {j in T} printf "\t%d", x[i,j];
   printf "\t%s", if pretty then sum{j in I} x[i,j] else "";
   printf("\n");
}

printf "%s    ", if pretty then "sum" else ""; 
for {j in I}
{  
   printf "\t%s", if pretty then sum{i in I} x[i,j] else "";
}
printf("\n");

data;

param n := 9;

param m := 9;

param r := 2;

param pretty := 1;

param prev : 1   2   3   4   5   6   7   8   9 :=  
1   0   1   1   0   0   0   0   0   0   
2   1   0   1   0   0   0   0   0   0   
3   0   1   0   0   0   1   0   0   0   
4   0   0   0   0   1   1   0   0   0   
5   1   0   0   1   0   0   0   0   0   
6   0   0   0   0   0   0   1   1   0   
7   0   0   0   1   0   0   0   1   0   
8   0   0   0   0   1   0   1   0   0   
9   0   0   0   0   0   0   0   0   0   
;

end;
