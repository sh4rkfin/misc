%% Copyright
-module(graph).
-author("i835364").


-export([find_max_length/1]).

find_max_length(ULOAT) ->
  ULOAT = [T1|Tail],
  Tail = [T2|Remainder],
  case is_adjacent(T1, T2) of
    true ->
      DistanceList = [T1, T2],
      find_max_length(Tail, DistanceList);
    false ->
      dead_end
  end.

find_max_length(RULOAT, DistanceList) ->
  RULOAT = [T1 | Tail],
  Tail = [T2 | Remainder],
  case is_adjacent(T1, T2) of
    true ->
      DL = [DistanceList | T1, T2],
      find_max_length(Tail, DL);
    false ->
      dead_end
  end.

find_max_length([], DL) ->
  len(DL).

is_adjacent({_,A},{A,_}) -> true;
is_adjacent(A, B) -> false.
