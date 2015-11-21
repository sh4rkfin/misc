%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2015 10:07 AM
%%%-------------------------------------------------------------------
-module(util).
-author("dfinlay").

-include_lib("eunit/include/eunit.hrl").

%% -record(result, {name :: string(), children=[] :: [#node{}]}).
%% -type node_record() :: #node{}.

%% API
-export([]).

compare(A, B) ->
  %%io:format("~p == ~p: ~p~n", [A, B, A == B]),
  if
    A == B ->
      0;
    true -> 
      1
  end.

list_prepender(X, none) ->
  [X];
list_prepender(X, Y) ->
  [X] ++ Y.

counter(X, none) ->
  X;
counter(X, Y) ->
  %%io:format("eval ~p + ~p:~n", [X, Y]),
  X + Y.

compare_list([],[], _, _) ->
  none;
compare_list([],_, _, _) ->
  none;
compare_list(_,[], _, _) ->
  none;
compare_list([H1 | T1],[H2 | T2], Comparator, Accumulator) ->
  Result = compare_list(T1, T2, Comparator, Accumulator),
  Diff = Comparator(H1, H2),
  Accumulator(Diff, Result).

fold2l(_, Accumulation, [], _) ->
  Accumulation;
fold2l(_, Accumulation, _, []) ->
  Accumulation;
fold2l(F, Accumulation, [H1 | T1], [H2 | T2]) ->
  fold2l(F, F(H1, H2, Accumulation), T1, T2).

diff_summer(X, Y, Accumulation) ->
  if
    X == Y -> Accumulation;
    true -> Accumulation + 1
  end.

fold2r(F, Accumulation, Marker, [H1 | T1], [H2 | T2]) ->
  F(H1, H2, fold2r(F, Accumulation, Marker, T1, T2));
fold2r(_, Accumulation, _, [], []) ->
  Accumulation;
fold2r(F, Accumulation, Marker, [], [H | T]) ->
  F(Marker, H, Accumulation, fold2r(F, Accumulation, Marker, [], T));
fold2r(F, Accumulation, Marker, [H | T], []) ->
  F(H, Marker, Accumulation, fold2r(F, Accumulation, Marker, [], T)).

array_summer(X, Y) ->
  F = fun(V1, V2, A) -> [V1 + V2] ++ A end,
  fold2r(F, [], 0, X, Y).

differences(X, Y) ->
  F = fun(V1, V2, A) -> [compare(V1,V2)] ++ A end,
  fold2r(F, [], 0, X, Y).

array_summer_test() ->
  A = [1, 2, 3],
  B = [4, 5, 6],
  Result = array_summer(A, B),
  io:format("result: ~p~n", [Result]),
  ?assert(Result == [5, 7, 9]).

fold2l_test() ->
  A = [one, two, three],
  B = [one, two, four],
  Result = fold2l(fun diff_summer/3, 0, A, B),
  io:format("result: ~p~n", [Result]),
  ?assert(Result == 1).

fold2l_2d_test() ->
  A = [[1, 1], [2, 2], [3, 3]],
  B = [[1, 1], [2, 2], [4, 4]],
  G = fun(X, Y, Acc) -> fold2l(fun diff_summer/3, Acc, X, Y) end,
  Result = fold2l(G, 0, A, B),
  io:format("result: ~p~n", [Result]),
  ?assert(Result == 2).

fold2l_2d_more_test() ->
  A = [[1, 1], [3, 2]],
  B = [[1, 2], [4, 2]],
  G = fun(X, Y, Acc) ->
    io:format("X: ~p, Y: ~p, Acc: ~p~n", [X, Y, Acc]),
    Diffs = differences(X, Y),
    array_summer(Diffs, Acc)
  end,
  Result = fold2l(G, [0, 0], A, B),
  io:format("result: ~p~n", [Result]),
  ?assert(Result == [1, 1]).

compare_test() ->
  A = [one, two, three],
  B = [one, two, four],
  io:format("one == one: ~p~n", [one == one]),
  ?assert((one == one) =:= true),
  Result = compare_list(A, B, fun compare/2, fun list_prepender/2),
  io:format("result: ~p~n", [Result]), 
  ?assert(Result == [0,0,1]).

compare_with_counter_test() ->
  A = [one, two, three],
  B = [one, two, four],
  io:format("one == one: ~p~n", [one == one]),
  ?assert((one == one) =:= true),
  Result = compare_list(A, B, fun compare/2, fun counter/2),
  io:format("result: ~p~n", [Result]),
  ?assert(Result == 1).

compare2_test() ->
  A = [[1, 1], [2, 2], [3, 3]],
  B = [[1, 1], [2, 2], [4, 4]],
  Comp = fun(X, Y) -> compare_list(X, Y, fun compare/2, fun list_prepender/2) end,
  Result = compare_list(A, B, Comp, fun list_prepender/2),
  io:format("result: ~p~n", [Result]).

compare2_counter_test() ->
  A = [[1, 1], [2, 2], [3, 3]],
  B = [[1, 1], [2, 2], [4, 4]],
  Comp = fun(X, Y) -> compare_list(X, Y, fun compare/2, fun counter/2) end,
  Result = compare_list(A, B, Comp, fun counter/2),
  io:format("result: ~p~n", [Result]), 
  ?assert(Result == 2).

