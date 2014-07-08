%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2014 11:19 AM
%%%-------------------------------------------------------------------
-module(handler).
-author("dfinlay").

%% API
-export([init/1, handle_event/2, name/0, eval/1, eval/0]).

-record(state, {name}).

init(Name) ->
  io:format("Created handler ~p~n", [Name]),
  {ok, #state{name=Name}}.

% Use this in conjunction with the following commands in the shell
% mgr:start_link().
% gen_event:add_handler(mgr, handler,[first]).
% gen_event:add_sup_handler(mgr, handler,[second]).
% gen_event:notify(mgr,happening).
% gen_event:notify(mgr,do_remove).
% gen_event:notify(mgr,do_exit).
% gen_event:stop(mgr).
%
handle_event(do_exit, State) ->
  io:format("Received event: ~p, state: ~p~n", [do_exit, State]),
  exit(do_exit);
handle_event(do_remove, State) ->
  io:format("Received event: ~p, state: ~p~n", [do_remove, State]),
  remove_handler;
handle_event(Event, State) ->
  % these sleeps clearly show that the handlers receive & process the events synchronously
  io:format("Received event, beginning sleep: ~p, state: ~p~n", [Event, State]),
  timer:sleep(2000),
  io:format("Received event, done: ~p, state: ~p~n", [Event, State]),
  {ok, State}.

name() ->
  ?MODULE.

eval(Function) ->
  Val = Function(),
  io:format("in handler: value: ~p~n", [Val]).

eval() ->
  F = fun() ->
    name()
  end,
  eval(F),
  mgr:eval(F).
