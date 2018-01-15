-module(marina_pool).
-include("marina_internal.hrl").

-export([
    init/0,
    random/0,
    start/0,
    stop/0
]).

%% public
-spec init() ->
    ok.

init() ->
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec random() ->
    {ok, atom()} | {error, marina_not_started}.

random() ->
    case foil:lookup(?MODULE, node_count) of
        {ok, NodeCount} ->
            Index = shackle_utils:random(NodeCount),
            case foil:lookup(?MODULE, {node, Index}) of
                {ok, Name} ->
                    {ok, Name};
                {error, _Reason} ->
                    {error, marina_not_started}
            end;
        {error, _Reason} ->
            {error, marina_not_started}
    end.

-spec start() ->
    ok | {error, marina_not_started | pool_already_started}.

start() ->
    case foil:lookup(?MODULE, node_count) of
        {ok, _} ->
            {error, pool_already_started};
        {error, foil_not_started} ->
            {error, marina_not_started};
        {error, key_not_found} ->
            BootstrapIps = ?GET_ENV(bootstrap_ips, ?DEFAULT_BOOTSTRAP_IPS),
            Datacenter = ?GET_ENV(datacenter, ?DEFAULT_DATACENTER),
            Port = ?GET_ENV(port, ?DEFAULT_PORT),
            Nodes = nodes(BootstrapIps, Port, Datacenter),
            start_pools(Nodes);
        {error, module_not_found} ->
            {error, marina_not_started}
    end.

-spec stop() ->
    ok | {error, marina_not_started| pool_not_started}.

stop() ->
    case foil:lookup(?MODULE, node_count) of
        {ok, NodeCount} ->
            stop_pools(NodeCount);
        {error, foil_not_started} ->
            {error, marina_not_started};
        {error, key_not_found} ->
            {error, pool_not_started};
        {error, module_not_found} ->
            {error, marina_not_started}
    end.

%% private
filter_datacenter([], _Datacenter) ->
    [];
filter_datacenter([[HostId, _Datacenter, RpcAddress] | T], undefined) ->
    [{HostId, RpcAddress} | filter_datacenter(T, undefined)];
filter_datacenter([[HostId, Datacenter, RpcAddress] | T], Datacenter) ->
    [{HostId, RpcAddress} | filter_datacenter(T, Datacenter)];
filter_datacenter([_ | T], Datacenter) ->
    filter_datacenter(T, Datacenter).

name(HostId) ->
    HostId2 = marina_utils:uuid_to_string(HostId),
    list_to_atom("marina_" ++ HostId2).

nodes([], _Port, _Datacenter) ->
    Ip = ?GET_ENV(ip, ?DEFAULT_IP),
    [{<<0:128>>, marina_utils:ip_to_bin(Ip)}];
nodes([Ip | T], Port, Datacenter) ->
    case peers(Ip, Port) of
        {ok, {result, _ , _, []}} ->
            [{<<0:128>>, marina_utils:ip_to_bin(Ip)}];
        {ok, {result, _ , _, Rows}} ->
            filter_datacenter(Rows, Datacenter);
        {error, _Reason} ->
            nodes(T, Port, Datacenter)
    end.

peers(Ip, Port) ->
    case marina_utils:connect(Ip, Port) of
        {ok, Socket} ->
            Msg = marina_request:startup([]),
            case marina_utils:sync_msg(Socket, Msg) of
                {ok, undefined} ->
                    select_system_peers(Socket);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

select_system_peers(Socket) ->
    Query = <<"select host_id, data_center, rpc_address from system.peers;">>,
    Msg = marina_request:query(0, [], Query, [], 1, [{skip_metadata, true}]),
    marina_utils:sync_msg(Socket, Msg).

start_pool(HostId, <<A, B, C, D>>) ->
    BacklogSize = ?GET_ENV(backlog_size, ?DEFAULT_BACKLOG_SIZE),
    Ip = lists:flatten(io_lib:format("~b.~b.~b.~b", [A, B, C, D])),
    Name = name(HostId),
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

    case shackle_pool:start(Name, ?CLIENT, ClientOptions, PoolOptions) of
        ok ->
            {ok, Name};
        {error, Reason} ->
            {error, Reason}
    end.

start_pools(Nodes) ->
    start_pools(Nodes, 1).

start_pools([], Index) ->
    foil:insert(?MODULE, node_count, Index - 1),
    foil:load(?MODULE);
start_pools([{HostId, RpcAddress} | T], Index) ->
    case start_pool(HostId, RpcAddress) of
        {ok, Name} ->
            foil:insert(?MODULE, {node, Index}, Name),
            start_pools(T, Index + 1);
        {error, _Reason} ->
            start_pools(T, Index)
    end.

stop_pools(0) ->
    foil:delete(?MODULE, node_count),
    foil:load(?MODULE);
stop_pools(N) ->
    {ok, Name} = foil:lookup(?MODULE, {node, N}),
    ok = shackle_pool:stop(Name),
    ok = foil:delete(?MODULE, {node, N}),
    stop_pools(N - 1).
