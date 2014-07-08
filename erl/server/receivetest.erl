%% Copyright
-module(receivetest).
-author("dave").

-record(message,{type, value, thing}).

-include_lib("eunit/include/eunit.hrl").
%% API
-export([start/0, allServers/0, kill/1, killAll/0, server_loop/1, sendRecord/1]).

server_loop(Name) ->
  receive
    die ->
      ok;
    R ->
      io:format("Server ~p got this: ~p~n", [Name, R]),
      io:format("Is message record?: ~p~n", [is_record(R, message)]),
      server_loop(Name);
    _ ->
      server_loop(Name)
  end.

spawn_servers([], All) ->
  done;
spawn_servers([First | Rest], All) ->
  Pid = spawn(?MODULE, server_loop, [First]),
  io:format("server: ~p started~n", [First]),
  register(First, Pid),
  spawn_servers(Rest, All).

makeServerName(ServerNumber) ->
  Name = io_lib:format("s~p", [ServerNumber]),
  lists:flatten(Name).

allServers() ->
  allServers(5, []).

allServers(0, Acc) ->
  Acc;
allServers(Count, Acc) ->
  NameString = makeServerName(Count),
  allServers(Count - 1, [list_to_atom(NameString)] ++ Acc).

kill(Name) ->
  Pid = whereis(Name),
  if
    Pid == undefined -> not_alive;
    true ->
      %io:format("sending die message to: ~p~n", [Pid]),
      Pid ! die
  end.

killAll() ->
  [kill(Name) || Name <- allServers()].

sendRecord(Server) ->
  Record = #message{type=simple, value=1, thing="that"},
  Pid = whereis(Server),
  if
    Pid =/= undefined -> Pid ! Record;
    true ->
      io:format("Server: ~p not alive; message not sent~n", [Server])
  end.

start() ->
  All = allServers(),
  spawn_servers(All, All),
  ok.




