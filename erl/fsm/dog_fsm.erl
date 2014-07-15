%%%-------------------------------------------------------------------
%%% @author dfinlay
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2014 2:14 PM
%%%-------------------------------------------------------------------
-module(dog_fsm).
-author("dfinlay").

%% API
-export([start/0, squirrel/1, pet/1, die/1]).

start() ->
  spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.

%% Note - dog responds to events asynchronously. The pet event is sent to the dog
%% and it responds right away (after putting the pet message in the queue.)
pet(Pid) -> Pid ! pet.

die(Pid) -> Pid ! die.

bark() ->
  io:format("Dog says: BARK! BARK!~n"),
  receive
    pet ->
      wag_tail();
    die ->
      io:format("Dog dies!"),
      done;
    _ ->
      io:format("Dog is confused~n"),
      bark()
  after 2000 ->
    bark()
  end.

wag_tail() ->
  io:format("Dog wags its tail~n"),
  receive
    pet ->
      sit();
    die ->
      io:format("Dog dies!"),
      done;
    _ ->
      io:format("Dog is confused~n"),
      wag_tail()
  after 30000 ->
    bark()
  end.

sit() ->
  io:format("Dog is sitting. Gooooood boy!~n"),
  receive
    squirrel ->
      bark();
    die ->
      io:format("Dog dies!"),
      done;
    _ ->
      io:format("Dog is confused~n"),
      sit()
  end.
