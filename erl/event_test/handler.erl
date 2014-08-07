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
-export([init/1, handle_event/2, handle_info/2, terminate/2, name/0, eval/1, eval/0,
         test_calling_process_dies_sup/0, test_calling_process_dies_non_sup/0]).

-record(state, {name}).

init(Name) ->
  io:format("Created handler ~p~n", [Name]),
  {ok, #state{name=Name}}.

%% Running the following commands in the erlang shell shows a number of behaviors:
%% 1) synchronous dispatching of events by the event manager
%% 2) supervised handlers versus non-supervised handlers. It seems the main point of
%%    supervised handlers is to supervise the liveness of the *calling* process and to
%%    automatically remove the listener if the calling process goes away.
%%
%% 105> {ok, Pid} = mgr:start_link().
%% {ok,<0.236.0>}
%% 106>
%% 106> gen_event:which_handlers(mgr).
%% []
%% 107> gen_event:add_handler(mgr, {handler, first}, [first]).
%% Created handler [first]
%% ok
%% 108> gen_event:add_sup_handler(mgr, {handler, second}, [second]).
%% Created handler [second]
%% ok
%% 109> gen_event:notify(mgr,tired).
%% Received event tired; beginning sleep. State: {state,[second]}. Pid: <0.236.0> (note handlers process event synchronously)
%% ok
%% Received event, done: tired, state: {state,[second]}
%% Received event tired; beginning sleep. State: {state,[first]}. Pid: <0.236.0> (note handlers process event synchronously)
%% Received event, done: tired, state: {state,[first]}
%% 110>
%% 110>
%% 110> gen_event:which_handlers(mgr).
%% [{handler,second},{handler,first}]
%% 111>
%% 111> handler:test_calling_process_dies_sup().
%% Created handler []
%% <0.243.0>
%% Terminating handler. Arg: {stop,normal}, State: {state,[]}, Pid: <0.236.0>
%% Received raw message: {'EXIT',<0.243.0>,normal}, state: {state,[second]}, Pid: <0.236.0>
%% Received raw message: {'EXIT',<0.243.0>,normal}, state: {state,[first]}, Pid: <0.236.0>
%% 112>
%% 112>
%% 112> handler:test_calling_process_dies_non_sup().
%% Created handler []
%% <0.245.0>
%% 113>

handle_event(do_exit, State) ->
  io:format("Received event: ~p, state: ~p, Pid: ~p~n", [do_exit, State, self()]),
  exit(do_exit);
handle_event(do_remove, State) ->
  io:format("Received event: ~p, state: ~p, Pid: ~p~n", [do_remove, State, self()]),
  remove_handler;
handle_event(Event, State) ->
  % these sleeps clearly show that the handlers receive & process the events synchronously
  io:format("Received event ~p; beginning sleep. State: ~p. Pid: ~p (note handlers process event synchronously) ~n",
            [Event, State, self()]),
  timer:sleep(2000),
  io:format("Received event, done: ~p, state: ~p~n", [Event, State]),
  {ok, State}.

handle_info(Event, State) ->
  io:format("Received raw message: ~p, state: ~p, Pid: ~p~n", [Event, State, self()]),
  {ok, State}.

terminate(Arg, State) ->
  io:format("Terminating handler. Arg: ~p, State: ~p, Pid: ~p ~n", [Arg, State, self()]).

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

test_calling_process_dies_sup() ->
  spawn(
    fun() ->
      gen_event:add_sup_handler(mgr, {handler, test_calling_process_dies_supervised}, []),
      timer:sleep(1000)
    end
  ).

test_calling_process_dies_non_sup() ->
  spawn(
    fun() ->
      gen_event:add_handler(mgr, {handler, test_calling_process_dies_non_supervised}, []),
      timer:sleep(1000)
    end
  ).
