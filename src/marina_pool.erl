-module(marina_pool).
-include("marina_internal.hrl").

-compile({no_auto_import, [
    node/1
]}).

-export([
    init/0,
    node/0,
    node/1,
    node_id/1,
    start/2,
    stop/1,
    sync/3
]).

%% public
-spec init() ->
    ok.

init() ->
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec node() ->
    {ok, atom()} | {error, marina_pool_not_started}.

node() ->
    node(undefined).

-spec node(binary() | undefined) ->
    {ok, atom()} | {error, marina_pool_not_started}.

node(RoutingKey) ->
    case foil:lookup(?MODULE, strategy) of
        {ok, Strategy} ->
            case node(Strategy, RoutingKey) of
                undefined ->
                    {error, marina_pool_not_started};
                {ok, Node} ->
                    {ok, Node};
                {error, _Reason} ->
                    {error, marina_pool_not_started}
            end;
        {error, _Reason} ->
            {error, marina_pool_not_started}
    end.

-spec node_id(binary()) ->
    atom().

node_id(<<A, B, C, D>>) ->
    RpcAddress = lists:flatten(lists:join(".", [integer_to_list(X) ||
        X <- [A, B, C, D]])),
    list_to_atom("marina_" ++ RpcAddress).

-spec start(random | token_aware, [{binary(), binary()}]) ->
    ok.

start(Strategy, Nodes) ->
    sync(Strategy, Nodes, []).

-spec stop(non_neg_integer()) ->
    ok.

stop(0) ->
    foil:delete(?MODULE, strategy),
    foil:load(?MODULE);
stop(N) ->
    {ok, NodeId} = foil:lookup(?MODULE, {node, N}),
    ok = shackle_pool:stop(NodeId),
    ok = foil:delete(?MODULE, {node, N}),
    stop(N - 1).

-spec sync(random | token_aware,
           [{binary(), binary()}],
           [{binary(), binary()}]) -> ok.

sync(Strategy, NewNodes, OldNodes) ->
    OldAddrs = [Addr || {Addr, _} <- OldNodes],
    NewAddrs = [Addr || {Addr, _} <- NewNodes],
    lists:foreach(fun stop_node/1, OldAddrs -- NewAddrs),
    lists:foreach(fun start_node/1, NewAddrs -- OldAddrs),
    rebuild_index(Strategy, NewNodes, length(OldNodes)),
    case Strategy of
        token_aware -> marina_ring:build(NewNodes);
        random -> ok
    end.

%% private
node({random, NodeCount}, undefined) ->
    N = shackle_utils:random(NodeCount),
    foil:lookup(?MODULE, {node, N});
node({token_aware, NodeCount}, undefined) ->
    N = shackle_utils:random(NodeCount),
    foil:lookup(?MODULE, {node, N});
node({token_aware, _NodeCount}, RoutingKey) ->
    marina_ring:lookup(RoutingKey).

start(<<A, B, C, D>> = RpcAddress) ->
    BacklogSize = ?GET_ENV(backlog_size, ?DEFAULT_BACKLOG_SIZE),
    Ip = lists:flatten(io_lib:format("~b.~b.~b.~b", [A, B, C, D])),
    NodeId = node_id(RpcAddress),
    PoolSize = ?GET_ENV(pool_size, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?GET_ENV(pool_strategy, ?DEFAULT_POOL_STRATEGY),
    Port = ?GET_ENV(port, ?DEFAULT_PORT),
    Reconnect = ?GET_ENV(reconnect, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?GET_ENV(reconnect_time_max,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?GET_ENV(reconnect_time_min,
        ?DEFAULT_RECONNECT_MIN),
    SocketOptions = ?GET_ENV(socket_options, ?DEFAULT_SOCKET_OPTIONS),

    ClientOptions = [
        {ip, Ip},
        {port, Port},
        {reconnect, Reconnect},
        {reconnect_time_max, ReconnectTimeMax},
        {reconnect_time_min, ReconnectTimeMin},
        {socket_options, SocketOptions}
    ],
    PoolOptions = [
        {backlog_size, BacklogSize},
        {pool_size, PoolSize},
        {pool_strategy, PoolStrategy}
    ],

    case shackle_pool:start(NodeId, ?CLIENT, ClientOptions, PoolOptions) of
        ok ->
            {ok, NodeId};
        {error, Reason} ->
            {error, Reason}
    end.

start_node(RpcAddress) ->
    _ = start(RpcAddress),
    ok.

stop_node(RpcAddress) ->
    NodeId = node_id(RpcAddress),
    _ = catch shackle_pool:stop(NodeId),
    _ = marina_cache:erase_pool(NodeId),
    ok.

rebuild_index(Strategy, NewNodes, OldCount) ->
    _ = [foil:delete(?MODULE, {node, N}) || N <- lists:seq(1, OldCount)],
    lists:foldl(fun ({Addr, _}, N) ->
        foil:insert(?MODULE, {node, N}, node_id(Addr)),
        N + 1
    end, 1, NewNodes),
    foil:insert(?MODULE, strategy, {Strategy, length(NewNodes)}),
    foil:load(?MODULE),
    ok.
