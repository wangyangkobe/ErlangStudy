-module(flowrate).

%% API
-export([start_link/0]).
-export([start_link/1]).
-export([test/1, test/2]).
%% Supervisor callbacks
-export([init/1]).

-record(state, {}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Res = supervisor:start_link({local, ?MODULE}, ?MODULE, supervisor_process),
    Fun = fun(Method) -> 
		  supervisor:start_child(?MODULE, [Method])
     	  end,
    MethodList = ["REGISTER", "INVITE", "MESSAGE", "PUBLISH"],
    lists:foreach(Fun, MethodList),
    Res.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Method) ->
    io:format("~p~n", [Method]),
    gen_server:start_link({local, method_to_servername(Method)}, 
			  ?MODULE, [Method], []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(supervisor_process) ->
    {ok, {{simple_one_for_one, 12, 60},
          [{child, {?MODULE, start_link, []},
            permanent, brutal_kill, worker, [?MODULE]}]}};
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
init([Method]) ->
    io:format("~p, call gen_server:init~n", [Method]),
    {ok, #state{}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
method_to_servername("REGISTER") -> sip_fc_register;
method_to_servername("INVITE")   -> sip_fc_invite;
method_to_servername("OPTIONS")  -> sip_fc_options;
method_to_servername("PUBLISH")  -> sip_fc_publish;
method_to_servername("MESSAGE")  -> sip_fc_message;
method_to_servername("SUBSCRIBE")-> sip_fc_subscribe;
method_to_servername(_Other)     -> undefined.

test(start)->
    application:start(flowrate);
test(add) ->
    supervisor:start_child(?MODULE, ["OPTIONS"]);
test(stop) ->
    application:stop(flowrate).

test(stopchild, Method) ->
    %% The second parameter must be PID when restart strategy
    %% is simple_one_for_one
    supervisor:terminate_child(flowrate, 
			       whereis(method_to_servername(Method)));
test(addchild, Method) ->
    supervisor:start_child(?MODULE, [Method]).

