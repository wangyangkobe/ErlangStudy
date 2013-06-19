-module(client).
-compile(export_all).

-include("common.hrl").

%%%客户端一起来，就应该调用该函数得到对应的Socket，之后的交互都是通过此Socket.
connect()->
    {ok, Socket} = gen_tcp:connect("localhost", ?PORT, [binary, {packet, 0}]),
    Socket.

say(Socket, Data) ->
    ok = gen_tcp:send(Socket, term_to_binary(Data)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received:~p~n", [binary_to_term(Bin)]);
        _Other ->
            io:format("Client received:~p~n", [_Other])
    end.

logout(Socket)->
    ok = gen_tcp:close(Socket).
