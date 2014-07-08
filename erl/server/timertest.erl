%% Copyright
-module(timertest).
-author("dave").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([main/0, process/1]).

process(die) ->
  io:format("dying...~n"),
  normal;
process(persist) ->
  receive
    die ->
      process(die);
    _ ->
      io:format("persisting...~n"),
      process(persist)
  end.

messageNonExistentProcess_test() ->
  Pid = spawn(?MODULE, process, [persist]),
  ?assert(is_process_alive(Pid)),
  io:format("pid: ~p is_pid: ~p is_process_alive: ~p~n", [Pid, is_pid(Pid), is_process_alive(Pid)]),
  timer:sleep(100),
  Pid ! are_you_alive,
  timer:sleep(100),
  io:format("pid: ~p is_pid: ~p is_process_alive: ~p~n", [Pid, is_pid(Pid), is_process_alive(Pid)]),
  Pid ! die,
  timer:sleep(100),
  io:format("pid: ~p is_pid: ~p is_process_alive: ~p~n", [Pid, is_pid(Pid), is_process_alive(Pid)]),
  ?assertNot(is_process_alive(Pid)),
  io:format("message non existent process~n"),
  Pid ! die_again,
  io:format("done messaging non existent process~n"),
  timer:sleep(100),
  io:format("done with messageNonExistentProcess_test~n").

timer_test(DelayMillis, SleepMillis) ->
  StartTime = now(),
  timer:send_after(DelayMillis, timertest),
  if
    SleepMillis > 0 ->
      io:format("sleeping for ~p millis~n", [SleepMillis]),
      timer:sleep(SleepMillis);
    true ->
      ok
  end,
  io:format("waiting to receive ...~n"),
  receive
    X ->
      ReceivedTime = now(),
      io:format("received: ~p millis later: ~p~n", [X, timer:now_diff(ReceivedTime, StartTime)/1000]),
      ?assert(X =:= timertest)
  end,
  io:format("done with timertest~n").

timer_test() -> [timer_test(1000, 0), timer_test(100, 500)].

function() ->
  {{1}, 2}.

assignment_test () ->
  {X, Y} = function(),
  io:format("X: ~p, Y: ~p~n", [X, Y]).

main() ->
  timer_test(1000,0),
  timer_test(100,500).


