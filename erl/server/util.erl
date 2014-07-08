%% Copyright
-module(util).
-author("dave").

%% API
-export([min/2, max/2, signum/2, join/2, joinList/2, join/3, joinList/3, stringify/2, stringifyNonTailRecursively/2,
         addSeconds/2]).

signum(X, Y) when X < Y -> -1;
signum(X, Y) when X > Y -> 1;
signum(X, Y) -> 0.

min(X,Y) ->
  if
    X < Y -> X;
    X > Y -> Y;
    true -> X
  end.

max(X,Y) ->
  if
    X > Y -> X;
    X < Y -> Y;
    true -> X
  end.

addSeconds(TimeStamp, Interval) ->
  setelement(2, TimeStamp, element(2, TimeStamp) + Interval).

join(List, Delimiter) ->
  lists:concat(joinList(List, Delimiter, nil)).

stringify(List, Stringer) ->
  stringify(List, Stringer, []).

% say stringify([a,b,c], Stringer)
% s([a,b,c], .)
% ["a"] ++ s([b,c], .)
% ["a"] ++ ["b"] ++ s([c], .)
% ["a"] ++ ["b"] ++ ["c"] ++ s([], .)
% ["a"] ++ ["b"] ++ ["c"] ++ []
% ["a"] ++ ["b"] ++ ["c"]
% ["a"] ++ ["b","c"]
% ["a","b","c"]
stringifyNonTailRecursively([], Stringer) ->
  [];
stringifyNonTailRecursively([First | Rest], Stringer) ->
  [Stringer(First)] ++ stringifyNonTailRecursively(Rest, Stringer).

% say stringify([a,b,c], Stringer)
% s([a,b,c], [])
% s([b,c], ["a"])
% s([c], ["a","b"])
% s([], ["a","b","c"])
% ["a","b","c"])
stringify([], Stringer, Acc) ->
  Acc;
stringify([First | Rest], Stringer, Acc) ->
  stringify(Rest, Stringer, Acc ++ [Stringer(First)]).

join(List, Delimiter, Stringer) ->
  string:join(joinList(List, Delimiter, Stringer)).

joinList(List, Delimiter) ->
  join(List, Delimiter, nil, []).

joinList(List, Delimiter, Stringer) ->
  join(List, Delimiter, Stringer, []).

toString(Value, nil) ->
  Value;
toString(Value, Stringer) ->
  Result = Stringer(Value),
  io:format("Result: ~s~n", [Result]),
  Result.

join([], Delimiter, Stringer, Acc) ->
  Acc;
join([Single], Delimiter, Stringer, Acc) ->
  Acc ++ [toString(Single, Stringer)];
join([First | Rest], Delimiter, Stringer, Acc) ->
  NewAcc = Acc ++ [toString(First, Stringer), Delimiter],
  join(Rest, Delimiter, Stringer, NewAcc).

