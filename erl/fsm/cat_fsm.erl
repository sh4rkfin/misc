%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2014 2:19 PM
%%%-------------------------------------------------------------------
-module(cat_fsm).
-author("dfinlay").

%% API
-export([start/0, event/2]).

start() ->
  spawn(fun() -> dont_give_crap() end).


%% note the cat responds to events synchronously - it waits to receive a message
%% back from the state transition before it fully processes the event
event(Pid, Event) ->
  Ref = make_ref(), % won't care for monitors here
  Pid ! {self(), Ref, Event},
  receive
    {Ref, Msg} -> {ok, Msg}
  after 5000 ->
    {error, timeout}
  end.

dont_give_crap() ->
  receive
    {Pid, Ref, _Msg} -> Pid ! {Ref, meh};
    _ -> ok
  end,
  io:format("Switching to 'dont_give_crap' state~n"),
  dont_give_crap().