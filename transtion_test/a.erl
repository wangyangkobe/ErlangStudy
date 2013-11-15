-module(a).
-compile(export_all).

-record(counter, {key, value}).
-define(KEY, "ABCD").
-define(NODE, 'b@esekilvxen245').

create_table()->
    erlang:set_cookie('abc', node()),
    net_adm:ping(?NODE),
    mnesia:create_schema([node() | nodes()]),
    mnesia:start(),
    lists:foreach(fun(Node) -> 
			  rpc:call(Node, mnesia, start, []),
			  mnesia:change_config(extra_db_nodes, [Node])
		  end, 
		  nodes()),
    mnesia:create_table(counter, [{ram_copies, [node() | nodes()]},
				  {attributes, record_info(fields, counter)}]).


update_counter(Key, Value) ->
    io:format("~p: call~n", [self()]),
    rpc:call(?NODE, b, increase_counter_value, [Key, Value]).

main() ->
    mnesia:dirty_write(#counter{key = ?KEY, value = 0}),
    Fun = fun() -> update_counter(?KEY, 1) end,
    lists:foreach(fun(_X) -> spawn(Fun) end, lists:seq(1, 1600)).

