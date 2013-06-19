%%%-------------------------------------------------------------------
%%% @author Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%% @copyright (C) 2013, Yang Wang
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2013 by Yang Wang <elqstux@esekilvxen245.rnd.ki.sw.ericsson.se>
%%%-------------------------------------------------------------------
-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("common.hrl").
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(userInfo, [{disc_copies, [node()]},
                                   {attributes,  record_info(fields, userInfo)}]),

    mnesia:create_table(roomInfo, [{ram_copies, [node()]},
                                   {attributes,  record_info(fields, roomInfo)}]),
                               
    case game_supervisor:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
