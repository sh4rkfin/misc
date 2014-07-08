%% Copyright
-module(tut1).
-author("dave").

%% API
-export([fac/1, mymax/2, list_length/1, list_max/1, is_empty/1]).
fac (1) -> 1;
fac (N) -> N * fac(N - 1).

list_length ([])             -> 0;
list_length ([First | Rest]) -> 1 + list_length(Rest).

is_empty([]) ->
  true;
is_empty([Head|Rest]) ->
  false.

mymax ( X, Y ) ->
  case X > Y of
    true -> X;
    false -> Y
  end.

list_max([Head | Rest]) ->
  case is_empty(Rest) of
    true -> Head;
    false -> mymax(Head, list_max(Rest))
  end.