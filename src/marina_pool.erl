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
    stop_nodes/3
]).
-export([
    node_up/2,
    node_down/2
]).

%% public
-spec init() ->
    ok.

init() ->
    ets:new(?MODULE, [named_table, public, set]),
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
    RpcAddress = lists:flatten(string:join([integer_to_list(X) ||
        X <- [A, B, C, D]], ".")),
    list_to_atom("marina_" ++ RpcAddress).

-spec start(random | token_aware, [{binary(), binary()}]) ->
    ok.

start(random, Nodes) ->
    start_nodes(Nodes, random, 1);
start(token_aware, Nodes) ->
    marina_ring:build(Nodes),
    start_nodes(Nodes, token_aware, 1).

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

-spec node_down(atom(), non_neg_integer()) -> no_return().
node_down(PoolName, _FailedWorkerCount) ->
    shackle_utils:warning_msg(?MODULE, "cassandra connection pool down! ~p failed workers ~p", [PoolName]),
    ets:insert(?MODULE, {PoolName, true}).

-spec node_up(atom(), non_neg_integer()) -> no_return().
node_up(PoolName, FailedWorkerCount) ->
    shackle_utils:warning_msg(?MODULE, "cassandra connection pool up! ~p failed workers ~p", [PoolName, FailedWorkerCount]),
    ets:delete(?MODULE, PoolName).

%% private
node(Strategy, RoutingKey) ->
    node(Strategy, RoutingKey, 1).

node({_, NodeCount} , undefined, N) when N >= NodeCount ->
    %% too many failures, pick first.
    foil:lookup(?MODULE, {node, 1});
node({random, NodeCount} = Strategy, undefined, N) ->
    X = shackle_utils:random(NodeCount),
    check_node(foil:lookup(?MODULE, {node, X}), Strategy, undefined, N);
node({token_aware, NodeCount} = Strategy, undefined, N) ->
    X = shackle_utils:random(NodeCount),
    check_node(foil:lookup(?MODULE, {node, X}), Strategy, undefined, N);
node({token_aware, _NodeCount} = Strategy, RoutingKey, N) ->
    check_node(marina_ring:lookup(RoutingKey), Strategy, RoutingKey, N).

check_node({error, _}, Strategy, _RoutingKey, N) ->
    %% cannot find a proper node when routing,
    %% remove the routing key, so the node selection will fall back to
    %% random or roken_aware without routing key.
    node(Strategy, undefined, N+1);
check_node({ok, Node}, Strategy, _RoutingKey, N) ->
    case is_node_down(Node) of
        true ->
            shackle_utils:warning_msg(?MODULE, "get a dead node when finding node, node id ~p, retrying", [Node]),
            %% the selected node is marked as down.
            %% remove routing key to fallback to random or token_aware without routing key.
            node(Strategy, undefined, N+1);
        false ->
            {ok, Node}
    end.

start_nodes([], random, N) ->
    foil:insert(?MODULE, strategy, {random, N - 1}),
    foil:load(?MODULE);
start_nodes([], token_aware, N) ->
    foil:insert(?MODULE, strategy, {token_aware, N - 1}),
    foil:load(?MODULE);
start_nodes([{RpcAddress, _Tokens} | T], Strategy, N) ->
    case start_node(RpcAddress) of
        {ok, NodeId} ->
            foil:insert(?MODULE, {node, N}, NodeId),
            start_nodes(T, Strategy, N + 1);
        {error, pool_already_started} ->
            start_nodes(T, Strategy, N + 1);
        {error, _Reason} ->
            start_nodes(T, Strategy, N)
    end.

start_node(<<A, B, C, D>> = RpcAddress) ->
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
    PoolFailureThresholdPercentage = ?GET_ENV(pool_failure_threshold_percentage, 0.2),
    PoolRecoverThresholdPercentage = ?GET_ENV(pool_recover_threshold_percentage, 0.1),
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
        {pool_strategy, PoolStrategy},
        {pool_failure_threshold_percentage, PoolFailureThresholdPercentage},
        {pool_recover_threshold_percentage, PoolRecoverThresholdPercentage},
        {pool_failure_callback_module, ?MODULE},
        {pool_recover_callback_module, ?MODULE}
    ],
    
    case shackle_pool:start(NodeId, ?CLIENT, ClientOptions, PoolOptions) of
        ok ->
            {ok, NodeId};
        {error, Reason} ->
            {error, Reason}
    end.

stop_nodes(NodesToStop, Strategy, NewNodes) ->
    L1 = length(NewNodes),
    L2 = L1 + length(NodesToStop),
    
    %% update pool dispatch first.
    %% insert updated nodes into foil.
    lists:foldl(fun({RpcAddress, _Token}, N) ->
        NodeId = node_id(RpcAddress),
        foil:insert(?MODULE, {node, N}, NodeId),
        N + 1
                end, 1, NewNodes),
    
    %% if strategy is token aware, rebuild the ring.
    (Strategy == token_aware) andalso marina_ring:build(NewNodes),
    
    %% update strategy
    foil:insert(?MODULE, strategy, {Strategy, L1}),
    
    %% delete old nodes from foil.
    [foil:delete(?MODULE, {node, X}) || X<- lists:seq(L1+1, L2)],
    %% reload foil.
    foil:load(?MODULE),
    
    %% delete stopped nodes from node down list
    %% and stop the corresponding pool
    [
        begin
            NodeId = node_id(RpcAddress),
            ets:delete(?MODULE, NodeId),
            shackle_pool:stop(NodeId)
        end || {RpcAddress, _} <- NodesToStop
    ].

    
is_node_down(NodeName) ->
    ets:lookup(?MODULE, NodeName) /= [].