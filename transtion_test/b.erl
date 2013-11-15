-module(b).
-compile(export_all).

-record(counter, {key, value}).
-define(KEY, "ABCD").

increase_counter_value(Key, Value) ->
    Fun = fun() ->
		  [Old] = mnesia:read(counter, Key),
		  NewValue = Old#counter.value + Value,
		  New = Old#counter{value = NewValue},
		  mnesia:write(New)
	  end,
    mnesia:transaction(Fun),
    io:format("~p~n", [ets:tab2list(counter)]).
