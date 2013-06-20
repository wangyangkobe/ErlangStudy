%%%-------------------------------------------------------------------
%%% @author Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%% @copyright (C) 2013, Yang Wang
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2013 by Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%%-------------------------------------------------------------------
-module(hall).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,
	{id,               % user ID, increased one by one
	 name,             % user name
	 pid,              % the process ID of the player
	 port,             % the socket of the player
	 roomPlayers = []
	}).

-include("common.hrl").
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
    [room:start_link(I) || I <- lists:seq(1, 5)],
    mnesia:subscribe({table, roomInfo, simple}),
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
handle_call({register, Name, PassWord}, _From, State)->
    Reply =  handle_register(Name, PassWord),
    {reply, Reply, State};
handle_call({login, Name, PassWord, Socket}, _Form, State)->
    io:format("State0:~p~n", [State]),
    case mnesia:dirty_match_object(#userInfo{name = Name,
                                             password = PassWord}) of
        [] ->
            {reply, {failed, "You not register!"}, State};
        _ ->
            case lists:keyfind(Name, #state.name, State) of
                #state{} ->
                    {reply, {failed, "You have logged!"}, State};
                _ ->
                    Pid = spawn(fun() -> player(Socket) end),
                    Id = case State of
                             [] -> 1;
                             _ ->
                                 #state{id = LastId} =
                                     lists:last(lists:keysort(#state.id, State)),
                                 LastId + 1
                         end,
                    S = #state{id = Id, name = Name, pid = Pid, port = Socket},
                    {reply, {success, "Welcome you!"}, [S | State]}
            end
    end;
handle_call({logout, Socket}, _From, State)->
    io:format("State1:~p~n", [Socket]),
    NewState =  lists:keydelete(Socket, #state.port, State),
    io:format("State2:~p~n", [NewState]),
    {reply, ok, NewState};
handle_call({joinroom, RoomId, Socket}, _From, State) ->
    case lists:member(Socket, State#state.roomPlayers) of
        true ->
	    [Value = #roomInfo{players = Players}] = 
		mnesia:dirty_match_object(roomInfo, #roomInfo{id = RoomId, _ = '_'}),
	    Res = case length(Players) of
		      0 -> mnesia:dirty_write(Value#roomInfo{players = [Socket]}),
			   {success, "Join successfully!"};
		      1 -> mnesia:dirty_write(Value#roomInfo{state = full,
							     players = [Socket | Players]}),
			   {success, "Join successfully!"};
		      2 ->
			  {failed, "The room is full!"}
		  end,
	    {reply, Res, State};
	false ->
	    {reply, {failded, ""}, State}
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
handle_info({mnesia_table_event,{write, RoomInfoData, _Activity}}, State) ->
    io:format("~p~n~p~n", [RoomInfoData, State]),
    {noreply, State};
handle_info({mnesia_table_event,{delete, RoomInfoData, _Activity}}, State) ->
    io:format("~p~n~p~n", [RoomInfoData, State]),
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
handle_register(Name, PassWord) ->
    F = fun() ->
                case mnesia:match_object(#userInfo{name = Name, _ = '_'}) of
                    [] ->
                        mnesia:write(#userInfo{name = Name, password = PassWord});
                    [#userInfo{}] ->
                        exist
                end
        end,
    mnesia:transaction(F).

player(_Socket)->
    io:format("I am a player~n").

