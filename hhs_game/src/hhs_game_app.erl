-module(hhs_game_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("common.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(userInfo, [{disc_copies, [node()]},
                                   {attributes,  record_info(fields, userInfo)}]),

    mnesia:create_table(roomInfo, [{ram_copies, [node()]},
                                   {attributes,  record_info(fields, roomInfo)}]),
                               
    case hhs_game_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
