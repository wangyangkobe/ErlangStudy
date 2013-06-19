%%%-------------------------------------------------------------------
%%% @author flybird <flybird@flybird>
%%% @copyright (C) 2013, flybird
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by flybird <flybird@flybird>
%%%-------------------------------------------------------------------
-module(findNodeClient).

-author('flybird@flybird').

-compile(export_all).

broadcast() ->
    mnesia:start(),
    Address = {226,0,0,1},
    Port    = 6969,
    Ttl     = 10,

    Opts1 = [ { active, true },
	      { ip, Address },
	      { add_membership, { Address, { 0, 0, 0, 0 } } },
	      { multicast_loop, true },
	      { reuseaddr, true },
	      list ],

    %% Add localhost address to the multicast group.
    {ok, _RecvSocket} = gen_udp:open(Port, Opts1),

    Opts2 = [ { ip,             { 0, 0, 0, 0 } },
	      { multicast_ttl,  Ttl }, 
	      { multicast_loop, true } ],
    {ok, SendSocket} = gen_udp:open(0, Opts2),
    
    %% send local node to the findNode server.
    ok = gen_udp:send(SendSocket, Address, Port, atom_to_list(node())).

