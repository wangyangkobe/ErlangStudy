%%%-------------------------------------------------------------------
%%% @author Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%% @copyright (C) 2013, Yang Wang
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2013 by Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%%-------------------------------------------------------------------
-module(game_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("common.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    connect_listen:start(?PORT),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild1 = {'hall', {'hall', start_link, []},
               Restart, Shutdown, Type, ['hall']},

    % AChild2 = {'room', {'room', start_link, []},
    %	      Restart, Shutdown, Type, ['room']},

    {ok, {SupFlags, [AChild1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
