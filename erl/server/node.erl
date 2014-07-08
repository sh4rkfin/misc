%% Copyright
-module(node).
-author("dave").

-include("node.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/2, killAll/0, server_loop/4, allServers/0, allServers_test/0, makeServerName/1,
         getServerNumber/1, rotate/2, findIndex/2]).

% State Diagram
%
server_loop(BaseState, Timeout, BootstrapFunction, TransitionFunction) ->
  {State, BootstrapAction} = BootstrapFunction(BaseState),
  Delay = 7000,
  %io:format("~p,~p,~p,~p,~p,process will timeout in ~p millis~n",
  %   [State#state.name, self(), State#state.state, nil, Delay, Delay]),
  receive
    die ->
      io:format("dying ~p,~p,~p,~p~n",
        [State#state.name, State#state.state, State#state.leader, State#state.messageCount]),
      unregister(State#state.name);
    Message ->
      {BaseNextState, Action} = TransitionFunction(State, Message),
      NextState = BaseNextState#state{messageCount=BaseNextState#state.messageCount + 1},
      Millis = trunc(timer:now_diff(now(), NextState#state.created) / 1000),
      %io:format("Message: ~p~n", [Message]),
      io:format("~p,~p,~p,\"~p\",~p,~p,~p,~p,~p,~p,\"~s\"~n",
        [State#state.name, Message#message.fromName, Message#message.type, Message#message.args,
          State#state.state, NextState#state.state,
          State#state.leader, NextState#state.leader,
          NextState#state.messageCount,
          Millis,
          Action]),
      server_loop(NextState, 1000, BootstrapFunction, TransitionFunction);
    _ ->
      server_loop(State, 1000, BootstrapFunction, TransitionFunction)
  after
  Delay ->
    server_loop(State, 1000, BootstrapFunction, TransitionFunction)
  end.

spawn_servers([], All, BootstrapFunction, TransitionFunction) ->
  done;
spawn_servers([First | Rest], All, BootstrapFunction, TransitionFunction) ->
  State = #state{name=First, state=nil,allServers=All, created=now()},
  Pid = spawn(?MODULE, server_loop, [State, 1000, BootstrapFunction, TransitionFunction]),
  io:format("~p,~p,~p,\"~p\",~p,~p,~p,~p,server_loop started~n", [First, nil, spawn, undefined, nil, State#state.state, nil, nil]),
  register(First, Pid),
  spawn_servers(Rest, All, BootstrapFunction, TransitionFunction).

makeServerName(ServerNumber) ->
  Name = io_lib:format("s~p", [ServerNumber]),
  lists:flatten(Name).

getServerNumber(ServerAtom) ->
  [H | Tail] = atom_to_list(ServerAtom),
  {Value, _} = string:to_integer(Tail),
  Value.

allServers() ->
  allServers(20, []).

allServers(0, Acc) ->
  Acc;
allServers(Count, Acc) ->
  NameString = makeServerName(Count),
  allServers(Count - 1, [list_to_atom(NameString)] ++ Acc).

rotate(List, N) ->
  M = N rem length(List),
  if
    M > 0 -> rotate(List, M, [], []);
    true -> List
  end.

findIndex(Value, List) ->
  findIndex(Value, 0, List).

findIndex(Value, Index, []) ->
  -1;
findIndex(Value, Index, [First | Rest]) ->
  if
    First == Value -> Index;
    true -> findIndex(Value, Index + 1, Rest)
  end.

rotate([], N, Prefix, Suffix) ->
  Suffix ++ Prefix;
rotate([First | Rest], N, Prefix, Suffix) when N =< 0->
  rotate(Rest, N - 1, Prefix, Suffix ++ [First]);
rotate([First | Rest], N, Prefix, Suffix) when N > 0 ->
  rotate(Rest, N - 1, Prefix ++ [First], Suffix).

rotate([], Term, N, Prefix, Suffix) ->
  Suffix ++ Prefix;
rotate([First | Rest], Term, N, Prefix, Suffix) when N =< 0->
  rotate(Rest, N - 1, Prefix, Suffix ++ [First]);
rotate([First | Rest], Term, N, Prefix, Suffix) when N > 0 ->
  rotate(Rest, N - 1, Prefix ++ [First], Suffix).

allServers_test() ->
  List = allServers(5, []),
  io:format("allServers: ~p~n", [List]),
  getServerNumber(s15),
  io:format("rotate: ~p~n", [rotate(allServers(), 7)]),
  Idx = findIndex(s3, allServers()),
  io:format("findIdx: ~p~n", [Idx]).

killServer(Name) ->
  Pid = whereis(Name),
  if
    Pid == undefined -> not_alive;
    true ->
      %io:format("sending die message to: ~p~n", [Pid]),
      Pid ! die
  end.

killAll() ->
  [killServer(Name) || Name <- allServers()].

start(BootstrapFunction, TransitionFunction) ->
  io:format("Process,From Process,Message,Args,Before State,After State,Before Leader,After Leader,Message Count,Time,Action~n"),
  All = allServers(),
  spawn_servers(All, All, BootstrapFunction, TransitionFunction),
  ok.


