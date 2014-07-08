%% Copyright
-module(topology).
-author("dave").

%% API
-export([loop/0, start/0, spawn_servers/1, server_loop/1, start_servers/0, hash/1,
         test/0]).

-include("servers.hrl").

servers() ->
  [s1, s2, s3, s4].

loop() ->
  io:format("loop"),
  receive
    #servers{from_pid=From, list=_} ->
      From ! #servers{from_pid=self(), list=servers()},
      loop()
  end.

start() ->
  Pid = spawn(?MODULE, loop(), []),
  register(topology, Pid).

server_loop(Name) ->
  receive
    foo ->
      io:format("received foo: ~p~n", [Name]),
      server_loop(Name);
    _ ->
      server_loop(Name)
  end.

spawn_servers([]) ->
  done;
spawn_servers([First | Rest]) ->
  io:format("server_loop starting: ~p~n", [First]),
  Pid = spawn(?MODULE, server_loop, [First]),
  register(First, Pid),
  spawn_servers(Rest).

start_servers() ->
  spawn_servers(servers()).

hash(Value) ->
  erlang:phash2(Value).

binary_tree() ->
  {nil, nil, nil}.

insert(nil, Value, Comp) ->
  {nil, Value, nil};

insert({Left, nil, Right}, Value, Comp) ->
  {Left, Value, Right};

insert(BinaryTree = {Left, Data, Right}, Value, Comp) ->
  Val = Comp(Value, Data),
  if
    Val > 0 -> {insert(Left, Value, Comp), Data, Right};
    Val < 0 -> {Left, Data, insert(Right, Value, Comp)};
    Val == 0 -> BinaryTree
  end.

print(nil) ->
  ok;
print({Left, Data, Right}) ->
  print(Left),
  io:format("Data: ~p~n", [Data]),
  print(Right).

test() ->
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
  print(Tree5).






