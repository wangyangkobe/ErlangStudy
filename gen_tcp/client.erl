-module(client).
-compile(export_all).

send(Message) ->
    {ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, list_to_binary(Message)),
    receive
	{tcp, Socket, Data} ->
	    io:format("Server send: ~p~n", [binary_to_list(Data)])
    end,
    gen_tcp:close(Socket).


main() ->
    Message = ["atom" ++ integer_to_list(X) || X <- lists:seq(1, 1000)],
    send(Message).
