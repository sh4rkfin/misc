%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2014 3:43 PM
%%%-------------------------------------------------------------------
-module(picky).
-author("dfinlay").

%% API
-export([start/0, loop/0]).

%% simple example that shows how recives can be picky and
%% how you can drain a message queue using "after 0"

%% 1> c(picky).
%% {ok,picky}
%% 2> Pid = picky:start().
%% <0.39.0>
%% 3> Pid ! like.
%% Like this one
%% like
%% 4> Pid ! foo.
%% foo
%% 5> Pid ! foo.
%% foo
%% 6> Pid ! foo.
%% foo
%% 7> Pid ! like.
%% Like this one
%% like
%% 8> Pid ! drain.
%% Draining ...
%% received foo, draining...
%% drain
%% received foo, draining...
%% received foo, draining...
%% 9>
%%

start() ->
  spawn(fun loop/0).

loop() ->
  receive
    like ->
      io:format("Like this one~n"),
      loop();
    drain ->
      io:format("Draining ...~n"),
      drain()
  end.

drain() ->
  receive
    Msg ->
      io:format("received ~p, draining...~n", [Msg]),
      drain()
  after 0 ->
    loop()
  end.
