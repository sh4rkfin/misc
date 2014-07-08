%% Copyright
-module(simplebinarytree).
-author("dave").

%% API
-export([test/0]).


insert(nil, Value) ->
  {nil, Value, nil};
insert({Left, nil, Right}, Value) ->
  {Left, Value, Right};
insert({Left, Data, Right}, Value) when Value < Data ->
  {insert(Left, Value), Data, Right};
insert({Left, Data, Right}, Value) when Value > Data ->
  {Left, Data, insert(Right, Value)}.

print(nil) ->
  ok;
print({Left, Data, Right}) ->
  print(Left),
  io:format("Data: ~p~n", [Data]),
  print(Right).

test() ->
  Tree = insert(nil, 1),
  print(Tree),
  Tree2 = insert(Tree, 2),
%io:format("tuple> ~p~n", Tree2),
  print(Tree2),
  Tree3 = insert(Tree2, 3),
  Tree4 = insert(Tree3, 0),
  Tree5 = insert(Tree4, -1),
  io:format("Tree5~n"),
  print(Tree5).



