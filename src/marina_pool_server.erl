-module(marina_pool_server).
-include("marina_internal.hrl").

-export([
    start_link/0
]).

%% metal callbacks
-export([
    init/3,
    handle_msg/2,
    terminate/2
]).

-define(MSG_BOOTSTRAP, bootstrap_pool).

-record(state, {
    bootstrap_ips :: list(),
    datacenter    :: undefined | binary(),
    node_count    :: undefined | pos_integer(),
    port          :: pos_integer(),
    strategy      :: random | token_aware,
    timer_ref     :: undefined | reference()
}).

-type state() :: #state {}.

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    metal:start_link(?MODULE, ?MODULE, undefined).

%% metal callbacks
-spec init(atom(), pid(), undefined) ->
    no_return().

init(_Name, _Parent, undefined) ->
    BootstrapIps = ?GET_ENV(bootstrap_ips, ?DEFAULT_BOOTSTRAP_IPS),
    Port = ?GET_ENV(port, ?DEFAULT_PORT),
    Strategy = ?GET_ENV(strategy, ?DEFAULT_STRATEGY),

    self() ! ?MSG_BOOTSTRAP,

    {ok, #state {
        bootstrap_ips = BootstrapIps,
        port = Port,
        strategy = Strategy
    }}.

-spec handle_msg(term(), state()) ->
    {ok, state()}.

handle_msg(?MSG_BOOTSTRAP, #state {
        bootstrap_ips = BootstrapIps,
        port = Port,
        strategy = Strategy
    } = State) ->

    case nodes(BootstrapIps, Port) of
        {ok, Nodes} ->
            marina_pool:start(Strategy, Nodes),
            {ok, State#state {
                node_count = length(Nodes)
            }};
        {error, _Reason} ->
            shackle_utils:warning_msg(?MODULE, "bootstrap failed~n", []),
            {ok, State#state {
                timer_ref = erlang:send_after(500, self(), ?MSG_BOOTSTRAP)
            }}
    end.

-spec terminate(term(), state()) ->
    ok.

terminate(_Reason, #state {node_count = NodeCount}) ->
    marina_pool:stop(NodeCount),
    ok.

%% private
filter_datacenter([], _Datacenter) ->
    [];
filter_datacenter([[RpcAddress, _Datacenter, Tokens] | T], undefined) ->
    [{RpcAddress, Tokens} | filter_datacenter(T, undefined)];
filter_datacenter([[RpcAddress, Datacenter, Tokens] | T], Datacenter) ->
    [{RpcAddress, Tokens} | filter_datacenter(T, Datacenter)];
filter_datacenter([_ | T], Datacenter) ->
    filter_datacenter(T, Datacenter).

nodes([], _Port) ->
    {error, bootstrap_failed};
nodes([Ip | T], Port) ->
    case peers(Ip, Port) of
        {ok, Rows, Datacenter} ->
            case filter_datacenter(Rows, Datacenter) of
                [] ->
                    nodes(T, Port);
                Nodes ->
                    {ok, Nodes}
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(?MODULE,
                "bootstrap error: ~p~n", [Reason]),
            nodes(T, Port)
    end.

peers(Ip, Port) ->
    case marina_utils:connect(Ip, Port) of
        {ok, Socket} ->
            {ok, {result, _ , _, Rows}} =
                marina_utils:query(Socket, ?LOCAL_QUERY),
            {ok, {result, _ , _, Rows2}} =
                marina_utils:query(Socket, ?PEERS_QUERY),
            gen_tcp:close(Socket),
            [[_, Datacenter, _]] = Rows,
            {ok, Rows ++ Rows2, Datacenter};
        {error, Reason} ->
            {error, Reason}
    end.
