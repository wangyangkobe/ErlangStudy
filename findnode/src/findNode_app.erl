%%%-------------------------------------------------------------------
%%% @author flybird <flybird@flybird>
%%% @copyright (C) 2013, flybird
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by flybird <flybird@flybird>
%%%-------------------------------------------------------------------
-module(findNode_app).

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

    table_init(),

    {ok, MultiCastAddr} = application:get_env(findNode, multicast_addr),
    {ok, MultiCastPort} = application:get_env(findNode, multicast_port),
    {ok, MultiCastTtl}  = application:get_env(findNode, multicast_ttl),

    error_logger:info_msg("MultiCastAddr:~p~nMultiCastPort:~p~nMultiCastTtl:~p~n",
			  [MultiCastAddr, MultiCastPort, MultiCastTtl]),

    Parameter = {MultiCastAddr, MultiCastPort, MultiCastTtl},

    case findNode_sup:start_link(Parameter) of
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
table_init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(person, [{attributes, record_info(fields, person)},
				 {disc_copies, [node()]}]).
