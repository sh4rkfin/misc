%% Copyright
-module(binarytree).
-author("dave").

%% API
-export([make/0, make/1, binarytree_test/0, test1/0, test2/0, test3/0, test4/0, test5/0]).

make(Comp) ->
  {{nil, nil, nil}, Comp}.
make() ->
  make(fun util:signum/2).

insert(BinaryTree = {Root, Comp}, []) ->
  BinaryTree;
insert({Root, Comp}, [First | Rest]) ->
  BT = {insert(Root, First, Comp), Comp},
  insert(BT, Rest).

collect(BinaryTree = {Root, Comp}) ->
  collectData(Root).

% node functions
collectData(nil) ->
  [];
collectData({Left, Data, Right}) ->
  collectData(Left) ++ [Data] ++ collectData(Right).

insert(nil, Value, Comp) ->
  {nil, Value, nil};
insert({Left, nil, Right}, Value, Comp) ->
  {Left, Value, Right};
insert(BinaryTree = {Left, Data, Right}, Value, Comp) ->
  Val = Comp(Value, Data),
  if
    Val < 0 -> {insert(Left, Value, Comp), Data, Right};
    Val > 0 -> {Left, Data, insert(Right, Value, Comp)};
    Val == 0 -> BinaryTree
  end.

min(nil, Y, Comp) ->
  Y;
min(X, nil, Comp) ->
  X;
min(X, Y, Comp) ->
  Comp(X,Y).

ceilingValue(BinaryTree = {Root, Comp}, Value) ->
  ceilingValue(Root, Value, Comp).

ceilingValue(nil, Value, Comp) ->
  nil;
ceilingValue(Node = {Left, Data, Right}, Value, Comp) ->
  Val = Comp(Value, Data),
  if
    Val < 0 -> min(ceilingValue(Left, Value, Comp), Data);
    Val > 0 -> ceilingValue(Right, Value, Comp);
    Val == 0 -> Value
  end.

print(nil) ->
  ok;
print({Left, Data, Right}) ->
  print(Left),
  io:format("Data: ~p~n", [Data]),
  print(Right).

test1() ->
  Comp = fun(First, Second) -> Second - First end,
  Tree = insert(nil, 1, Comp),
  print(Tree),

  Tree2 = insert(Tree, 2, Comp),
  %io:format("tuple> ~p~n", Tree2),
  print(Tree2),
  Tree3 = insert(Tree2, 3, Comp),
  Tree4 = insert(Tree3, 0, Comp),
  Tree5 = insert(Tree4, -1, Comp),
  io:format("Tree5~n"),
  print(Tree5),
  C = collectData(Tree5),
  io:format("collection len ~p~n", [length(C)]),
  io:format("collection: ~p~n", [util:join(C, ",")]).

test2() ->
  List = [2, 1, 6, 11, 0, -1],
  BT = insert(make(), List),
  io:format("collection: ~p~n", [util:join(collect(BT), ",")]),
  Ceil = ceilingValue(BT, 12),
  io:format("ceil: ~p~n", [Ceil]),
  BT2 = insert(make(fun(X,Y) -> Y - X end), List),
  io:format("collection: ~p~n", [util:join(collect(BT2), ",")]).


test3() ->
  List = ["2", "1", "3", "11", "0", "-1"],
  BT = insert(make(), List),
  io:format("collection: ~p~n", [util:join(collect(BT), ",")]),
  Test = {one, "two", 3},
  String = io_lib:format("~p", [Test]),
  io:format("String: ~s~n", [String]),
  ok.

test4() ->
  List = [{2, "s1"}, {1, "s2"}, {6, "s3"}, {11, "s4"}, {0, "s5"}],
  Comp = fun({X1, X2}, {Y1, Y2}) ->
    X1 - Y1
  end,
  BT = insert(make(Comp), List),
  io:format("BT: ~p~n", [BT]),
  Stringer = fun(X) ->
    Val = io_lib:format("~p", [X]),
    io_lib:format("~s", [Val])
  end,
  %io:format("collection: ~p~n", [util:join(collect(BT), ",", Stringer)]).
  Terms = collect(BT),
  Strings = util:stringifyNonTailRecursively(Terms, Stringer),
  io:format("concat: ~s~n", [string:join(Strings, ",")]),
  Strings2 = util:stringify(Terms, Stringer),
  io:format("concat: ~s~n", [string:join(Strings2, ",")]).

test5() ->
  F = fun util:signum/2,
  F(2,3).

binarytree_test() ->
  test1(),
  test2(),
  test3(),
  test4(),
  test5().




