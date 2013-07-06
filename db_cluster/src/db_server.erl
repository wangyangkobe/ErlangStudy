%%%-------------------------------------------------------------------
%%% @author flybird <flybird@flybird>
%%% @copyright (C) 2013, flybird
%%% @doc
%%%
%%% @end
%%% Created :  5 Jul 2013 by flybird <flybird@flybird>
%%%-------------------------------------------------------------------
-module(db_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%% -record(state, {}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName], []).

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
init([TableName]) ->
    TableId = ets:new(TableName, [set, public, named_table]),
    {ok, TableId}.

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
handle_call({update, Key, Value}, _From, TableId) ->
    TableName = ets:info(TableId, name),
    case ets:lookup(TableId, {Key, TableName}) of
	[] ->
	    {reply, error, TableId};
	_ ->
	    ets:insert(TableId, {{Key, TableName}, Value}),
	    {reply, ok, TableId}
    end;
handle_call({search, Key}, _From, TableId) ->
    TableName = ets:info(TableId, name),
     case ets:lookup(TableId, {Key, TableName}) of
	[] ->
	    {reply, error, TableId};
	[{_, Value}] ->  
	    {reply, {Value, TableName}, TableId}
    end;
handle_call({delete, Key}, _From, TableId) ->
    TableName = ets:info(TableId, name),
    ets:delete(TableId, {Key, TableName}),
    {reply, ok, TableId};
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
handle_cast({insert, TableName, Key, Value}, TableId) ->
    ets:insert(TableId, {{Key, TableName}, Value}),
    {noreply, TableId};
handle_cast({backup, TableName, Key, Value}, TableId) ->
    ets:insert(TableId, {{Key, TableName}, Value}),
    {noreply, TableId};
handle_cast({update_backup, TableName, Key, Value}, TableId) ->
    case ets:info(TableId, name) of
	TableName ->  % The self db server needn't to be updated.
	    {noreply, TableId}; 
	_ ->
	    ets:insert(TableId, {{Key, TableName}, Value}),
	    {noreply, TableId}
    end;
handle_cast({delete_backup, ServerName, Key}, TableId) ->
    case ets:lookup(TableId, {Key, ServerName}) of
	[] -> {noreply, TableId};
	[_Value] ->
	    ets:delete(TableId, {Key, ServerName}),
	    db_proxy ! {decrease_backup, ets:info(TableId, name)},
	    {noreply, TableId}
    end;
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
