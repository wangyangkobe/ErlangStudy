-module(bubbleSort).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

sort(L) ->
    sort(L, length(L), []).

sort(_L, 0, _Res) -> [];
sort([H | _T], 1, Res) -> [H | Res];
sort(L, Len, Res) ->
    T1 = lists:sublist(L, 1, Len),
    T2 = inner_sort(T1, []),
    Last = lists:last(T2),
    sort(T2, Len - 1, [Last | Res]).

inner_sort([A, B], Res) when (A < B)->
    Res ++ [A, B];
inner_sort([A, B], Res) ->
    Res ++ [B, A];
inner_sort([A, B | T], Res) when (A < B) ->
    inner_sort([B | T], Res ++ [A]);
inner_sort([A, B | T], Res) ->
    inner_sort([A | T], Res ++ [B]).

test()->
    L = [5, 3, -1, 10, 6, 100, 99],
    ?assert(sort([])  =:= []),
    ?assert(sort([1]) =:= [1]),
    ?assert(sort([1, 2, 3, 4]) =:= [1, 2, 3, 4]),
    ?assert(sort([10, 5, 3, 2, 1]) =:= [1, 2, 3, 5, 10]),
    ?assert(sort(L) =:= [-1, 3, 5, 6, 10, 99, 100]).
