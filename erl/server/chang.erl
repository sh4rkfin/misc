%% Copyright
-module(chang).
-author("dave").

-include("node.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([transition/2, start/0, get_successor_test/0]).

get_nth_successor (State, N) ->
  get_nth_successor(N, 1, State#state.successors).

get_nth_successor (N, Idx, []) ->
  {Idx, undefined};
get_nth_successor (N, Idx, [Term]) ->
  {Idx, undefined};
get_nth_successor (N, Idx, [First | Rest]) ->
  %io:format("First: ~p~n", [First]),
  Pid =
    if
      N =< Idx -> whereis(First);
      true -> undefined
    end,
  if
    Pid =/= undefined ->
      %io:format("got successor: ~p, ~p, ~p~n", [N, Idx, First]),
      {util:max(N, Idx), Pid};
    true ->
      get_nth_successor(N, Idx + 1, Rest)
  end.

wait_for_result(State) ->
  {State, "no action"}.
  %timer:cancel(State#state.timer),
  %{ok, TimerRef} = timer:send_after(5000, {self(), waiting_for_result_timeout, State#state.name}),
  %NextState = State#state{state=waiting_for_result,timer=TimerRef},
  %{NextState, "move to wait for result"}.

wait_for_successor(State) ->
  timer:cancel(State#state.timer),
  {ok, TimerRef} = timer:send_after(500, #message{from=self(), type=monitor, fromName=State#state.name}),
  {State#state{timer=TimerRef}, "no successor, time @ 500 ms"}.

start_election(State) when State#state.state =:= participant ->
  {State, "no action"};
start_election(State) ->
  {Idx, Successor} = get_nth_successor(State, 1),
  %io:format("Me: ~p, Idx: ~p, Successors: ~p, Successor: ~p~n", [State#state.name, Idx, State#state.successors, Successor]),
  if
    Successor =:= undefined ->
      % can't start election
      wait_for_successor(State);
    true ->
      Successor ! #message{from=self(), type=election, fromName=State#state.name, args={State#state.name}},
      {State#state{state=participant}, "started election"}
  end.

announce_leadership(State) ->
  {State, "announcing leadership"}.

transition (State, Message) when Message#message.type =:= inquiry ->
  if
    Message#message.fromName < State#state.name ->
      if
        State#state.state =:= electing orelse State#state.state =:= waiting_for_result ->
          % if electing or waiting and got an inquiry from lower node do nothing
          {State, "no action"};
        true ->
          % got an inquiry message from a lower node if not electing, start elections
          start_election(State)
      end;
    true ->
      if
        State#state.state =:= has_leader ->
          % got inquiry from higher node, stay in has_leader state, higher node
          % will likely message soon with leader announcement
          {State, "no action"};
        true ->
          % got an inquiry message from a higher node, go to waiting for result
          wait_for_result(State)
      end
  end;

transition (State, Message) when Message#message.type =:= monitor ->
  bootstrap(State);

transition (State, Message) when Message#message.type =:= election ->
  %{State, "election transition not yet implemented"};
  {Candidate} = Message#message.args,
  {Idx, Successor} = get_nth_successor(State, 1),
  if
    Candidate < State#state.name andalso State#state.state =/= non_participant->
      {State, "dropped message from lower node during election"};
    Successor =:= undefined ->
      wait_for_successor(State);
    Candidate > State#state.name ->
      Successor ! Message#message{from=self(), fromName=State#state.name},
      {State#state{state=participant}, "forwarded election message"};
    Candidate < State#state.name andalso State#state.state =:= non_participant->
      Successor ! Message#message{from=self(), fromName=State#state.name, args={State#state.name}},
      {State#state{state=participant}, "replaced self as candidate"};
    true ->
      % i'm the leader
      Successor ! #message{from=self(), fromName=State#state.name, type=elected, args={State#state.name}},
      {State#state{state=non_participant, leader=State#state.name}, "I'm the leader"}
  end;

transition (State, Message) when Message#message.type =:= elected ->
  {Leader} = Message#message.args,
  if
    Leader =/= State#state.name ->
      {Idx, Successor} = get_nth_successor(State, 1),
      Successor ! Message#message{from=self(), fromName=State#state.name},
      {State#state{leader=Leader,state=non_participant}, "forwarded elected message"};
    true ->
      % discard message, we're done
      {State, "election is over"}
  end;

transition (State, Message) when Message#message.type =:= alive ->
  if
    State#state.state =:= electing orelse State#state.state =:= no_leader ->
% someone else is going to be the leader
      wait_for_result(State);
    true ->
      {State, "no action"}
  end;

transition (State, Message) when Message#message.type =:= waiting_for_result_timeout ->
  if
    State#state.state =:= waiting_for_result ->
      if
        State#state.leader =:= nil ->
          {State#state{state=no_leader}, "waiting for result timed out so moving to no_leader"};
        true ->
          {State#state{state=has_leader}, "waiting for result timed out so keeping old leader"}
      end;
    true ->
      {State, "no action"}
  end;

transition (State, Message) when Message#message.type =:= election_timeout->
  if
    State#state.state =:= electing ->
      announce_leadership(State);
    true ->
      {State, "no action"}
  end;

transition (State, Message) ->
  {State, "Oh oh got here***"}.

bootstrap (BaseState) ->
  %io:format("BaseState: ~p~n", [BaseState]),
  Me = BaseState#state.name,
  AllServers = BaseState#state.allServers,
  State =
    if
      BaseState#state.state =:= nil ->
        Idx = node:findIndex(Me, AllServers),
        Count = length(AllServers),
        Successors = node:rotate(AllServers, (Idx + 1) rem Count),
        BaseState#state{state=non_participant, successors=Successors};
      true -> BaseState
    end,
  if
    State#state.state =:= non_participant andalso State#state.leader =:= nil ->
      {BaseNextState, Action} = start_election(State),
      NextState = BaseNextState#state{messageCount=BaseNextState#state.messageCount + 1},
      io:format("~p,~p,~p,\"~p\",~p,~p,~p,~p,~p,~p,\"~s\"~n",
        [State#state.name, nil, bootstrap, undefined,
          State#state.state, NextState#state.state,
          State#state.leader, NextState#state.leader,
          State#state.messageCount,
          timer:now_diff(now(), State#state.created) / 1000,
          Action]),
      {NextState, Action};
    true ->
      {State, "no action"}
  end.

start() ->
  node:start(fun bootstrap/1, fun transition/2).

get_successor_test() ->
  io:format("~p~n", [get_nth_successor(1, 1, node:allServers())]),
  io:format("~p~n", [get_nth_successor(2, 1, node:allServers())]),
  io:format("~p~n", [get_nth_successor(3, 1, node:allServers())]),
  io:format("~p~n", [get_nth_successor(4, 1, node:allServers())]),
  io:format("~p~n", [get_nth_successor(5, 1, node:allServers())]),
  io:format("~p~n", [get_nth_successor(6, 1, node:allServers())]).