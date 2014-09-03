%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2014 10:46 AM
%%%-------------------------------------------------------------------
-module(node).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([max_depth/2]).

-record(node, {name :: string(), children=[] :: [#node{}]}).
-type node_record() :: #node{}.

map_find(_, none) ->
  error;
map_find(Key, Map) ->
  maps:find(Key, Map).

map_put(_, _, none) ->
  none;
map_put(Key, Value, Map) ->
  maps:put(Key, Value, Map).

map_merge(none, _) ->
  none;
map_merge(M1, M2) ->
  maps:merge(M1, M2).

-spec aggregate_depth_info(node_record(), { integer(), integer(), map()}) -> { integer(), integer(), map()}.
aggregate_depth_info (Child, {DepthSoFar, OpCount, Visited}) ->
  %io:format("aggregating child: ~p~n", [Child#node.name]),
  {ChildDepth, NOps, ChildVisited} = max_depth(Child, Visited),
  {max(ChildDepth, DepthSoFar), NOps + OpCount, map_merge(Visited, ChildVisited)}.

-spec max_depth(node_record(), map()) -> {integer(), integer(), map()}.
max_depth(#node{children=[]}, Visited) ->
  {0, 1, Visited};
max_depth(#node{name=Name, children=Children}, Visited) ->
  Depth = map_find(Name, Visited),
  %io:format("processing node: ~p, found depth:~p, visited: ~p~n", [Name, Depth, Visited]),
  if
    Depth =:= error ->
      {Max, NOps, NewVisited} = lists:foldl(fun aggregate_depth_info/2, {0,0, Visited}, Children),
      {Max + 1, NOps + 1, map_put(Name, Max + 1, NewVisited)};
    true ->
      {element(2, Depth), 0, Visited}
  end.

create_nodes(0) ->
  [];
create_nodes(Count) ->
  Name = lists:flatten(io_lib:format("~p", [Count])),
  Nodes = create_nodes(Count - 1),
  Node = #node{name=Name, children=Nodes},
  [Node] ++ Nodes.

node_test() ->
  [First | _] = create_nodes(25),
  %io:format("processing node: ~p~n", [First]),
  io:format("processing node: ~p~n", [max_depth(First, maps:new())]),
  io:format("processing node: ~p~n", [max_depth(First, none)]).

node2_test() ->
  A = #node{name="1"},
  B = #node{name="2", children=[A]},
  C = #node{name="3", children=[A, B]},
  D = #node{name="4", children=[A, B, C]},
  E = #node{name="5", children=[A, B, C, D]},
  F = #node{name="6", children=[A, B, C, D, E]},
  G = #node{name="7", children=[A, B, C, D, E, F]},
  H = #node{name="8", children=[A, B, C, D, E, F, G]},
  Node = D,
  io:format("processing node: ~p~n", [Node]),
  max_depth(Node, maps:new()).


