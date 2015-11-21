%%%-------------------------------------------------------------------
-module(merge).
-include_lib("eunit/include/eunit.hrl").
-export([ukeymerge/3, key_merge_dedup/3]).

%% Basics:
%% Write down the recursion relation for doing a merge
%% merge(L1 = [H1 | T1], L2 = [H2 | T2], K1, K2)
%% if K1 <= K2
%%   write down H1
%%   merge(T1, L2, key(H1), K2)
%% else 
%%   write down H2
%%   merge(L1, T2, key(H1), K2)
%%


%% tail_key_not_equal(Idx, L, K) returns the tail list of tuples, L, 
%% beginning with the first tuple where the Idx’th element of that tuple
%% is not equal to K
tail_key_not_equal(_, [], _) ->
  [];
tail_key_not_equal(Idx, [H|T], K) when K =:= element(Idx, H) -> 
  tail_key_not_equal(Idx, T, K);
tail_key_not_equal(_, L, _) ->
  L.

do_key_merge_dedup(_, L1, [], M) ->
  L1 ++ M;
do_key_merge_dedup(_, [], L2, M) ->
  L2 ++ M;
do_key_merge_dedup(Idx, L1=[H1|T1], L2=[H2|T2], M) ->
  K2 = element(Idx, H2),
  case element(Idx, H1) of
    K1 when K1 < K2 ->
      do_key_merge_dedup(Idx, tail_key_not_equal(Idx, T1, K1), L2, [H1 | M]);
    K1 when K1 =:= K2 ->
      do_key_merge_dedup(Idx, tail_key_not_equal(Idx, T1, K1), tail_key_not_equal(Idx, T2, K1), [H1 | M]);
    _ ->
      do_key_merge_dedup(Idx, L1, tail_key_not_equal(Idx, T2, K2), [H2 | M])
  end.

%% key_merge_dedup(Idx, L1, L2) returns the result of merging two lists of
%% tuples L1 and L2 based on the Idx'th element of these tuples being 
%% interpreted as the key. 
%% L1, L2 are assumed to be in sorted order, but may contain successive tuples with
%% the same keys. When this happens the first key is kept and the remainder of the 
%% tuples are discarded.
%% If tuples from L1 and L2 have the same key the first key from L1 is kept and those from
%% L2 are discarded.
key_merge_dedup(Idx, L1, L2) ->
  M = do_key_merge_dedup(Idx, L1, L2, []),
  lists:reverse(M).

%% Elements from the first list are kept and prioritized.
ukeymerge2_1(I, [H1 | T1], K2, HdM, T2, M, H2) ->
  case element(I, H1) of
    E1 when E1 =< K2 ->
      ukeymerge2_1(I, T1, K2, E1, T2, [H1 | M], H2);
    E1 when K2 == HdM ->
      ukeymerge2_2(I, T1, E1, H1, T2, M);
    E1 ->
      ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
  end;
ukeymerge2_1(_I, [], E2, HdM, T2, M, _H2) when E2 == HdM ->
  lists:reverse(T2, M);
ukeymerge2_1(_I, [], _E2, _HdM, T2, M, H2) ->
  lists:reverse(T2, [H2 | M]).


%% T1: is tail of first list
%% K1: key of first element of first list
%% H1: first element of first list
%% H2: head of the second list
%% T2: tail of the second list
%%  M: accumulator
ukeymerge2_2(I, T1, K1, H1, [H2 | T2], M) ->
  case element(I, H2) of
    K2 when K1 =< K2 ->
      %% if key of first list is <= key of second list then
      ukeymerge2_1(I, T1, K2, K1, T2, [H1 | M], H2);
    _E2 ->
      ukeymerge2_2(I, T1, K1, H1, T2, [H2 | M])
  end;
ukeymerge2_2(_I, T1, _E1, H1, [], M) ->
  lists:reverse(T1, [H1 | M]).

%% L1 is the first list
%% T2 is the second list
ukeymerge(Index, L1, T2) when is_integer(Index), Index > 0 ->
  case L1 of
    [] ->
      %% L1 empty - then just use T2
      T2;
    [H1 | T1] ->
      K1 = element(Index, H1),
      %% T1: is tail of first list
      %% K1: key of first element of first list
      %% H1: first element of first list
      %% T2: second list
      M = ukeymerge2_2(Index, T1, K1, H1, T2, []),
      lists:reverse(M, [])
  end.


simple_test() ->
  ?assert(true).
