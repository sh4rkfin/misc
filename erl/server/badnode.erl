%% Copyright
-module(badnode).
-author("dave").

-record(state, {name, state=nil, time, allServers, leader=nil}).

%% API
-export([start/0, killAll/0, server_loop/2, server_loop_direct/1, test/0]).

start_election(State) when State#state.state == electing ->
  State;
start_election(State) ->
  Bigger = [whereis(X) || X <- State#state.allServers, X > State#state.name],
  [Pid ! {self(), inquiry, State#state.name} || Pid <- Bigger, Pid /= undefined],
  Timeout = util:addSeconds(now(), 5),
  State#state{state=electing,time=Timeout}.

announce_leadership(State) ->
  All = [whereis(X) || X <- State#state.allServers, X /= State#state.name],
  [Pid ! {self(), leader, State#state.name} || Pid <- All, Pid /= undefined],
  Timeout = util:addSeconds(now(), 5),
  State#state{state=nil,time=Timeout,leader=State#state.name}.

% State Diagram
%
%
%
server_loop(State, Timeout) ->
  Delay = if
    is_number(State#state.time) ->
      TimeDiff = trunc((timer:now_diff(State#state.time, now()) / 1000)),
      util:max(TimeDiff, 1);
    true -> Timeout
  end,
  io:format("~p,~p,~p,~p,~p,process will timeout in ~p millis~n",
    [State#state.name, self(), State#state.state, nil, Delay, Delay]),
  receive
    {From, inquiry, FromName} ->
      if
        FromName < State#state.name ->
          From ! {self(), alive, State#state.name},
          % got an inquiry message from a lower node
          % if not electing, start elections
          if
            State#state.name == nil ->
              io:format("~p,~p,~p,~p,~p,starting election got inquiry from lower node~n",
                        [State#state.name, self(), State#state.state, FromName, inquiry]),
              server_loop(start_election(State), 1000);
            true ->
              io:format("~p,~p,~p,~p,~p,not starting election from inquiry from lower node~n",
                [State#state.name, self(), State#state.state, FromName, inquiry]),
              server_loop(State, 1000)
          end;
        true ->
          % got an inquiry message from a higher node, backoff
          NextState = State#state{state=waitingforresult,time=util:addSeconds(now(),5)},
          io:format("~p,~p,~p,~p,~p,got inquiry from higher node waiting for result~n",
            [State#state.name, self(), State#state.state, FromName, inquiry]),
          server_loop(NextState, 3000)
      end;
    {From, leader, FromName} ->
      if
        FromName < State#state.name ->
          % got an leader message from a lower node
          % if not electing, start elections
          io:format("~p,~p,~p,~p,~p,starting election got leader from lower node~n",
            [State#state.name, self(), State#state.state, FromName, leader]),
          server_loop(start_election(State), 1000);
        true ->
          % got an leader message from a higher node, accept it
          io:format("~p,~p,~p,~p,~p,accepted leader message from higher node~n",
            [State#state.name, self(), State#state.state, FromName, leader]),
          server_loop(State#state{leader=FromName,time=nil}, 1000)
      end;
    {From, alive, FromName} ->
      io:format("~p,~p,~p,~p,~p,alive message received~n",
        [State#state.name, self(), State#state.state, FromName, alive]),
      % someone else is going to be the leader
      server_loop(State#state{state=nil,time=nil},1000);
    die ->
      io:format("dying ~p~n", [State]),
      unregister(State#state.name);
    _ ->
      server_loop(State, 1000)
  after
    Delay ->
      if
        State#state.state == electing ->
          Diff = timer:now_diff(now(), State#state.time),
          if
            Diff >= 0 ->
              io:format("~p,~p,~p,~p,~p,announcing leader~n",
                [State#state.name, self(), State#state.state, nil, State#state.name]),
              server_loop(announce_leadership(State),1000);
            true -> server_loop(State, 1000)
          end;
        State#state.state == waitingforresult ->
          server_loop(State,1000);
        State#state.leader == nil ->
          io:format("~p,~p,~p,~p,~p,starting election no leader~n",
            [State#state.name, self(), State#state.state, nil, nil]),
          server_loop(start_election(State),1000);
        true ->
          server_loop(State,1000)
      end
  end.

spawn_servers([], All) ->
  done;
spawn_servers([First | Rest], All) ->
  Pid = spawn(?MODULE, server_loop, [#state{name=First, allServers=All}, 1000]),
  io:format("~p,~p,~p,~p,~p,server_loop started~n", [First, Pid, nil, nil, nil]),
  register(First, Pid),
  spawn_servers(Rest, All).

allServers() ->
  [s1, s2, s3, s4, s5].

killServer(Name) ->
  Pid = whereis(Name),
  if
    Pid == undefined -> not_alive;
    true ->
      io:format("sending die message to: ~p~n", [Pid]),
      Pid ! die
  end.

killAll() ->
  [killServer(Name) || Name <- allServers()].

start() ->
  All = allServers(),
  spawn_servers(All, All),
  ok.

server_loop_direct(Name) ->
  Record = #state{name=Name, allServers=[Name]},
  io:format("record: ~p~n", [Record]),
  server_loop(Record, 1000).

test() ->
  io:format("timer:time: ~p~n", [now()]).


