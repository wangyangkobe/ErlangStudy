%%%-------------------------------------------------------------------
%%% @author flybird <flybird@flybird>
%%% @copyright (C) 2013, flybird
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by flybird <flybird@flybird>
%%%-------------------------------------------------------------------
-module(findNodeServer).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([find_node/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sendSocket, recvSocket, multiAddr, multiPort}).

%%%===================================================================
%%% API
%%%===================================================================
find_node() ->
    gen_server:call(?MODULE, find_node).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Parameter) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Parameter], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{Address, Port, Ttl}]) ->
    process_flag(trap_exit, true),
    Opts = [ { active, true },
	     { ip, Address },
	     { add_membership, { Address, { 0, 0, 0, 0 } } },
	     { multicast_loop, true },
	     { reuseaddr, true },
	     list ],

    {ok, RecvSocket} = gen_udp:open(Port, Opts),
    SendSocket = open_send_socket(Ttl),

    State = #state{sendSocket = SendSocket,
		   recvSocket = RecvSocket,
		   multiAddr  = Address,
		   multiPort  = Port},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(find_node, _From, State = #state{sendSocket = SendSocket,
					     multiAddr  = Address,
					     multiPort  = Port}) ->
    NodeString = atom_to_list(node()),
    ok = gen_udp:send(SendSocket, Address, Port, NodeString),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, Socket, _Address, _Port, Data}, 
	    State = #state{recvSocket = Socket}) ->
    io:format("Receive data:~p~n", [Data]),
    net_adm:ping(list_to_atom(Data)),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
open_send_socket(Ttl) ->
    Opts = [ { ip,             { 0, 0, 0, 0 } },
	     { multicast_ttl,  Ttl }, 
	     { multicast_loop, true } ],
    {ok, SendSocket} = gen_udp:open(0, Opts),
    SendSocket.
