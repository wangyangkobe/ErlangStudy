%%%-------------------------------------------------------------------
%%% @author flybird <flybird@flybird>
%%% @copyright (C) 2013, flybird
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2013 by flybird <flybird@flybird>
%%%-------------------------------------------------------------------
-module(db_proxy).

-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0]).
-export([add_db_server/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DB_CLUSER_SUP, db_cluser_sup).
-record(serverState, {serverName,
		       used   = 0, 
		       free   = 1000, 
		       backup = 0}).

%%%===================================================================
%%% API
%%%===================================================================
add_db_server(ServerName) when is_atom(ServerName)->
    gen_server:call(?MODULE, {add_db_server, ServerName});

add_db_server(ServerName) when is_list(ServerName)->
    gen_server:call(?MODULE, {add_db_server, list_to_atom(ServerName)}).

ls_db_server()->
    gen_server:call(?MODULE, ls_db_server).

rm_db_server(ServerName) when is_atom(ServerName)->
    gen_server:call(?MODULE, {rm_db_server, ServerName});
rm_db_server(ServerName) when is_list(ServerName)->
    gen_server:call(?MODULE, {rm_db_server, list_to_atom(ServerName)}).

insert(Key, Value) ->
    gen_server:call(?MODULE, {insert, Key, Value}).

update(Key, Value) ->
    gen_server:call(?MODULE, {update, Key, Value}).

search(Key)->
    gen_server:call(?MODULE, {search, Key}).

delete(Key)->
    gen_server:call(?MODULE, {delete, Key}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, []}.

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
handle_call({add_db_server, ServerName}, _From, State) ->
    case lists:keyfind(ServerName, #serverState.serverName, State) of
	false ->
	    start_db_server(ServerName),
	    {reply, ok, [#serverState{serverName = ServerName} | State]};
	_ ->
	    {reply, {error, "The db server is existed!"}, State}	
    end;

handle_call(ls_db_server, _From, State) ->
    {reply, get_server_state(State), State};

handle_call({rm_db_server, ServerName}, _From, State) ->
    case lists:keyfind(ServerName, #serverState.serverName, State) of
	false ->
	    {reply, {error, "The db server is not existed!"}, State};
	_ ->
	    supervisor:terminate_child(?DB_CLUSER_SUP, ServerName),
	    supervisor:delete_child(?DB_CLUSER_SUP, ServerName),
	    {reply, ok, lists:keydelete(ServerName, #serverState.serverName, State)}
    end;

handle_call({insert, Key, Value}, _From, State) ->
    case State of 
	[] -> % no db server
	    {reply, {error, "There is no db sever, pleae create it!"}, State};
	[#serverState{free = Free, used = Used}] -> % just one db server
	    gen_server:cast(State#serverState.serverName, {insert, Key, Value}),
	    {reply, ok, State#serverState{free = Free - 1, used = Used + 1}};
	_ -> % more than one db server, need to backup
	    S = handle_insert({Key, Value}, State),
	    {reply, ok, S}
    end;

handle_call({update, Key, Value}, _From, State) ->
    case handle_search(Key, State) of
	undefined -> 
	    {reply, {error, "This data isn't exist, you cann't update!"}, State};
	{_Value, ServerName} ->
	    Reply = gen_server:call(ServerName, {update, Key, Value}),
	    lists:foreach(fun(#serverState{serverName = Name}) 
			     -> gen_server:cast(Name, {update_backup, ServerName, 
						       Key, Value}) end, State),
	    {reply, Reply, State}
    end;

handle_call({search, Key}, _From, State) ->
    {reply, handle_search(Key, State), State};

handle_call({delete, Key}, _From, State) ->
    case handle_search(Key, State) of
	undefined ->
	    {reply, {error, "This data is not existed!"}, State};
	{_Value, ServerName} ->
	    gen_server:call(ServerName, {delete, Key}),

	    {value, #serverState{used = Used, free = Free} = Old} 
		= lists:keysearch(ServerName, #serverState.serverName, State),

	    S = lists:keyreplace(ServerName, #serverState.serverName, State,
				 Old#serverState{used = Used - 1, free = Free + 1}),
 	    lists:foreach(fun(#serverState{serverName = Name}) 
			     -> gen_server:cast(Name, {delete_backup, ServerName, 
	    				       Key}) end, State),
	    NewState = decrese_backup_counter(S),
	    {reply, ok, NewState}
    end;

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
handle_cast({server_alive, ServerName}, State) ->
    NewState = lists:keyreplace(ServerName, #serverState.serverName, 
				State, #serverState{serverName = ServerName}),
    lists:foreach(fun(#serverState{serverName = Name}) 
		     -> gen_server:cast(Name, {server_alive, ServerName}) end, 
		  NewState),
    {noreply, NewState};

handle_cast({increace_used_counter, ServerName, Value}, State) ->
    {value, #serverState{used = Used, free = Free} = Old} 
	= lists:keysearch(ServerName, #serverState.serverName, State),
    NewState = lists:keyreplace(ServerName, #serverState.serverName, State,
				Old#serverState{used =  Used + Value, 
						free = Free - Value}),
    {noreply, NewState};

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
start_db_server(ServerName) ->
    supervisor:start_child(?DB_CLUSER_SUP,
			   {ServerName, {db_server, start_link, [ServerName]},
			    permanent, 5000, worker, [db_server]}).
%% get the state of db servers.
get_server_state(StateList) ->
    get_server_state(StateList, []).
get_server_state([], Res) ->
    Res;
get_server_state([#serverState{serverName = Name,
			       used = Use,
			       free = Free,
			       backup = BackUp} | Tail], Res) ->
    get_server_state(Tail, [{Name, Use, Free, BackUp} | Res]).

handle_insert({Key, Value}, State) ->
    [H1, H2 | _T] = lists:reverse(lists:keysort(#serverState.free, State)),
    #serverState{serverName = Name1, used = Used, free   = Free1} = H1,
    #serverState{serverName = Name2, free = Free2, backup = Back} = H2,
    gen_server:cast(Name1, {insert, Name1, Key, Value}),
    gen_server:cast(Name2, {backup, Name1, Key, Value}),
    S1 = lists:keyreplace(Name1, #serverState.serverName, State, 
			  H1#serverState{free = Free1 - 1, used = Used + 1}),
    lists:keyreplace(Name2, #serverState.serverName, S1,    
			  H2#serverState{free = Free2 - 1, backup = Back + 1}).

handle_search(Key, State) ->
    case State of
	[] -> undefined;
	_ ->
	    Res = lists:map(fun(#serverState{serverName = Name}) 
			       -> gen_server:call(Name, {search, Key}) end, State),
	    case lists:filter(fun(X) -> X =/= error end, Res) of
		[] -> undefined;
		[Result] -> Result
	    end
    end.

%% when delete one data, we need to delete the backup data.	    
decrese_backup_counter(State) ->
    receive
	{decrease_backup, ServerName} ->
	    {value, #serverState{backup = BackUp, free = Free} = Old} 
		= lists:keysearch(ServerName, #serverState.serverName, State),

	    lists:keyreplace(ServerName, #serverState.serverName, State,
			     Old#serverState{backup =  BackUp - 1, free = Free + 1})
    after 1000 ->
	    State
    end.
