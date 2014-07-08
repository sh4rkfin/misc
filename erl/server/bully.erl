%% Copyright
-module(bully).
-author("dave").

-include("node.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([transition/2, start/0]).

wait_for_result(State) ->
  timer:cancel(State#state.timer),
  {ok, TimerRef} = timer:send_after(5000, #message{from=self(),type=waiting_for_result_timeout,fromName=State#state.name}),
  NextState = State#state{state=waiting_for_result,timer=TimerRef},
  {NextState, "move to wait for result"}.

start_election(State) when State#state.state =:= electing ->
  {State, "no action"};
start_election(State) ->
  timer:cancel(State#state.timer),
  Bigger = [{X, whereis(X)} || X <- State#state.allServers, X > State#state.name],
  BiggerNames = [X || {X, Y} <- Bigger, Y =/= undefined ],
  %io:format("~p,~p,~p,~p,~p,sending inquiries ~p~n",
  %  [State#state.name, nil, nil, State#state.state, State#state.state, BiggerNames]),
  Message = #message{type=inquiry, from=self(), fromName=State#state.name},
  [Pid ! Message || {X, Pid} <- Bigger, Pid /= undefined],
  {ok, TimerRef} = timer:send_after(5000, #message{from=self(),type=election_timeout,fromName=State#state.name}),
  NextState = State#state{state=electing, timer=TimerRef},
  {NextState, io_lib:format("sending inquiries: ~p", [BiggerNames])}.

announce_leadership(State) ->
  All = [whereis(X) || X <- State#state.allServers, X /= State#state.name],
  [Pid ! #message{from=self(),type=leader, fromName=State#state.name} || Pid <- All, Pid /= undefined],
  NextState = State#state{state=has_leader,leader=State#state.name},
  {NextState, "announcing leadership"}.

transition (State, Message) when Message#message.type =:= inquiry ->
  if
    Message#message.fromName < State#state.name ->
      Message#message.from ! #message{from=self(), type=alive, fromName=State#state.name}, % always reply back
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

transition (State, Message) when Message#message.type =:= alive ->
  if
    State#state.state =:= electing orelse State#state.state =:= no_leader ->
      % someone else is going to be the leader
      wait_for_result(State);
    true ->
      {State, "no action"}
  end;

transition (State, Message) when Message#message.type =:= leader ->
  if
    Message#message.fromName < State#state.name ->
      % got a leader message from a lower node
      % if not electing, start elections
      start_election(State);
    true ->
      % got a leader message from a higher node, accept it
      {State#state{state=has_leader,leader=Message#message.fromName}, "accept new leader"}
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
  end.

bootstrap (State) ->
  if
    State#state.state =:= no_leader orelse State#state.state =:= nil ->
      {BaseNextState, Action} = start_election(State),
      NextState = BaseNextState#state{messageCount=BaseNextState#state.messageCount + 1},
      io:format("~p,~p,~p,~p,~p,~p,~p,~p,~p,\"~s\"~n",
        [State#state.name, nil, bootstrap,
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