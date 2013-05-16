%%%-------------------------------------------------------------------
%%% @author Yang Wang 
%%% @copyright (C) 2013, Yang Wang
%%% @doc
%%%       1、对mnesia的订阅的学习，mnesia:subscribe
%%%       2、erlang数据库Mnesia怎样实现主键自增
%%% @end
%%% Created : 16 May 2013 by Yang Wang
%%%-------------------------------------------------------------------
-module(mnesia_test).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%  user api
-export([add_person/4,
         delete_person/1,
         stop/0]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(unique_id, {key, id}).
-record(person, {id, name, sex, age, address}).

%%%===================================================================
%%% API
%%%===================================================================
add_person(Name, Sex, Age, Address) ->
    gen_server:call(?SERVER, {add, Name, Sex, Age, Address}).
delete_person(Name) ->
    gen_server:call(?SERVER, {delete, Name}).
stop() ->
    gen_server:cast(?SERVER, stop).
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
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(person, [{ram_copies, [node()]},
                                 {type,set},
                                 {attributes, record_info(fields, person)}]),
    mnesia:create_table(unique_id, [{type,set},
                                    {attributes, record_info(fields, unique_id)}]),
                                
    %% 对表person的mnesia订阅，收到的消息是在handle_info中处理
    %% http://mooooscar.blogspot.jp/2009/04/mnesia_11.html
    mnesia:subscribe({table, person, simple}),
    {ok, #state{}}.

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
handle_call({add, Name, Sex, Age, Address}, _From, State) ->
    Fun = fun() ->
                  %% 生成表person的自增id
                  Id = mnesia:dirty_update_counter(unique_id, person, 1),
                  io:format("Id:~p~n", [Id]),
                  mnesia:write(#person{id = Id, name = Name,
                                       sex = Sex, age = Age, address = Address})
          end,
    mnesia:transaction(Fun),
    {reply, ok, State};
handle_call({delete, Name}, _From, State)->
    Persons = mnesia:dirty_match_object(#person{name = Name, _ = '_'}),
    Fun = fun(X) ->
                  mnesia:transaction(fun() -> mnesia:delete_object(X) end)
          end,
    lists:foreach(Fun, Persons),
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
handle_cast(stop, State) ->
    io:format("The server is being stopped."),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    {stop, normal, State};
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
handle_info({mnesia_table_event, {write, _Record, _ActivityId}} = Info, State) ->
    io:format("The subscribe Info is:~p~n", [Info]),
    {noreply, State};
handle_info({mnesia_table_event, {delete, _Record, _ActivityId}} = Info, State) ->
    io:format("The subscribe Info is:~p~n", [Info]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("The subscribe Info is:~p~n", [Info]),
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
