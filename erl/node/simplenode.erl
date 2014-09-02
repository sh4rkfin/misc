%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2014 10:46 AM
%%%-------------------------------------------------------------------
-module(simplenode).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([max_depth/1]).

-record(node, {name, children=[]}).

max_depth(#node{children=[]}) ->
  0;
max_depth(#node{children=Children}) ->
  lists:max([max_depth(C) || C <- Children]) + 1.

create_nodes(0) ->
  [];
create_nodes(Count) ->
  Name = lists:flatten(io_lib:format("~p", [Count])),
  Nodes = create_nodes(Count - 1),
  Node = #node{name=Name, children=Nodes},
  [Node] ++ Nodes.

node_test() ->
  [First | _] = create_nodes(25),
  io:format("processing node: ~p~n", [max_depth(First)]).


node2_test() ->
  A = #node{name="a"},
  B = #node{name="b", children=[A]},
  C = #node{name="c", children=[A, B]},
  D = #node{name="c", children=[A, B, C]},
  E = #node{name="c", children=[A, B, C, D]},
  F = #node{name="c", children=[A, B, C, D, E]},
  max_depth(F).


