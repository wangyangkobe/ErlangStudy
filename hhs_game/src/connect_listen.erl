-module(connect_listen).
-compile(export_all).

-include("common.hrl").

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> loop(Listen) end).

loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    Pid = spawn(fun() -> handle_conn(Socket) end),
    gen_tcp:controlling_process(Socket, Pid),
    loop(Listen).


%% This function used to handle multi user connection.
handle_conn(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("Data:~p~n", [binary_to_term(Data)]),
            case decode_data(Data) of
                ["register", Name, PassWord] ->
                    Reply = gen_server:call(hall, {register, Name, PassWord}),
                    case Reply of
			{atomic, exist} ->
                            gen_tcp:send(Socket, term_to_binary("The user is existed!"));
                        {atomic, ok} ->
                            gen_tcp:send(Socket, term_to_binary("Register successfully!"))
                    end;
                ["login", Name, PassWord] ->
                    Reply = gen_server:call(hall, {login, Name, PassWord, Socket}),
                    case Reply of
                        {failed, Reason} ->
			    gen_tcp:send(Socket, term_to_binary(Reason));
                        {success, Info}->
                            gen_tcp:send(Socket, term_to_binary(Info))
                    end;
		["joinroom", RoomId] ->
		    Reply = gen_server:call(hall, {joinroom, list_to_integer(RoomId), Socket}),
		    io:format("Reply:~p~n", [Reply]),
		    case Reply of
                        {failed, Reason} ->
                            gen_tcp:send(Socket, term_to_binary(Reason));
                        {success, Info}->
                            gen_tcp:send(Socket, term_to_binary(Info))
                    end;
                _ ->
                    gen_tcp:send(Socket, term_to_binary("yang"))
            end,
            handle_conn(Socket);
        {tcp_closed, Socket} ->
            gen_server:call(hall, {logout, Socket})
    end.

decode_data(Bin) ->
    L = binary_to_term(Bin),
    string:tokens(L, "@").


