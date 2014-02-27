    -module(server).

    -compile(export_all).

    start()->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 4}, 
					     {active, true}, {reuseaddr, true}]),
	spawn(fun() -> per_connect(ListenSocket) end).

    per_connect(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> per_connect(ListenSocket) end),
	loop(Socket).

    loop(Socket) ->
	receive
	    {tcp, Socket, Data} ->
		io:format("~p~n", [binary_to_list(Data)]),
		Res = lists:flatten(io_lib:format("You send ~p bytes.~n", [size(Data)])),
		gen_tcp:send(Socket, list_to_binary(Res)),
		loop(Socket);
	    {tcp_closed, Socket} ->
		io:format("closed~n"),
		gen_tcp:close(Socket)
	end.
