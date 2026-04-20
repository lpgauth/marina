%% Owns a long-lived CQL control connection to the cluster, subscribes to
%% server-side EVENT frames (TOPOLOGY_CHANGE, STATUS_CHANGE, SCHEMA_CHANGE),
%% and drives a full ring re-sync whenever the topology changes.
%%
%% Design is events-only and reconnect-on-change:
%%
%%   connecting -> dial any bootstrap_ip -> STARTUP + AUTH -> system.local +
%%     system.peers -> post {topology_full_sync, Nodes} to marina_pool_server
%%     -> REGISTER -> active-once -> subscribed
%%   subscribed -> any TOPOLOGY_CHANGE event closes the socket and re-enters
%%     connecting, which naturally re-queries system.peers and re-posts
%%     topology_full_sync. STATUS_CHANGE / SCHEMA_CHANGE are logged and
%%     otherwise ignored — shackle already reconnects transparently on
%%     per-node outages.
%%
%% Every reconnect does the full peers query again, which closes the window
%% where a topology event could have been missed while the control
%% connection was down.
-module(marina_control).
-include("marina_internal.hrl").

-behaviour(gen_statem).

-export([
    start_link/0
]).

-export([
    callback_mode/0,
    code_change/4,
    init/1,
    terminate/3
]).

-export([
    connecting/3,
    subscribed/3
]).

-define(CONTROL_STREAM, 0).
-define(EVENT_STREAM, -1).
-define(EVENT_TYPES, [
    <<"TOPOLOGY_CHANGE">>,
    <<"STATUS_CHANGE">>,
    <<"SCHEMA_CHANGE">>
]).
-define(BACKOFF_MIN, 500).
-define(BACKOFF_MAX, 30000).

-record(data, {
    bootstrap_ips :: [string()],
    port          :: pos_integer(),
    backoff       :: pos_integer(),
    socket        :: undefined | inet:socket(),
    buffer        :: binary()
}).

-type data() :: #data {}.

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, undefined, []).

%% gen_statem callbacks
-spec callback_mode() -> state_functions.

callback_mode() ->
    state_functions.

-spec init(undefined) ->
    {ok, connecting, data(), [gen_statem:action()]}.

init(undefined) ->
    BootstrapIps = ?GET_ENV(bootstrap_ips, ?DEFAULT_BOOTSTRAP_IPS),
    Port = ?GET_ENV(port, ?DEFAULT_PORT),
    {ok, connecting, #data {
        bootstrap_ips = BootstrapIps,
        port = Port,
        backoff = ?BACKOFF_MIN,
        buffer = <<>>
    }, [{next_event, internal, connect}]}.

-spec terminate(term(), atom(), data()) -> ok.

terminate(_Reason, _State, #data {socket = undefined}) ->
    ok;
terminate(_Reason, _State, #data {socket = Socket}) ->
    _ = gen_tcp:close(Socket),
    ok.

-spec code_change(term(), atom(), data(), term()) -> {ok, atom(), data()}.

code_change(_OldVsn, OldState, OldData, _Extra) ->
    {ok, OldState, OldData}.

%% state: connecting
-spec connecting(gen_statem:event_type(), term(), data()) ->
    gen_statem:event_handler_result(atom()).

connecting(internal, connect, #data {
        bootstrap_ips = Ips,
        port = Port
    } = Data) ->

    case establish(Ips, Port) of
        {ok, Socket, Nodes} ->
            marina_pool_server ! {topology_full_sync, Nodes},
            case subscribe(Socket) of
                ok ->
                    ok = inet:setopts(Socket, [{active, once}]),
                    {next_state, subscribed, Data#data {
                        socket = Socket,
                        buffer = <<>>,
                        backoff = ?BACKOFF_MIN
                    }};
                {error, Reason} ->
                    _ = gen_tcp:close(Socket),
                    shackle_utils:warning_msg(?MODULE,
                        "register failed: ~p~n", [Reason]),
                    backoff(Data)
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(?MODULE,
                "control connect failed: ~p~n", [Reason]),
            backoff(Data)
    end;
connecting(state_timeout, reconnect, Data) ->
    {keep_state, Data, [{next_event, internal, connect}]};
connecting(_, _, Data) ->
    {keep_state, Data}.

%% state: subscribed
-spec subscribed(gen_statem:event_type(), term(), data()) ->
    gen_statem:event_handler_result(atom()).

subscribed(info, {tcp, Socket, Chunk}, #data {
        socket = Socket,
        buffer = Buffer
    } = Data) ->

    {Rest, Frames} = marina_frame:decode(<<Buffer/binary, Chunk/binary>>),
    case process_frames(Frames) of
        cycle ->
            _ = gen_tcp:close(Socket),
            {next_state, connecting,
                Data#data {socket = undefined, buffer = <<>>,
                           backoff = ?BACKOFF_MIN},
                [{next_event, internal, connect}]};
        continue ->
            ok = inet:setopts(Socket, [{active, once}]),
            {keep_state, Data#data {buffer = Rest}}
    end;
subscribed(info, {tcp_closed, Socket}, #data {socket = Socket} = Data) ->
    shackle_utils:warning_msg(?MODULE, "control socket closed~n", []),
    backoff(Data#data {socket = undefined, buffer = <<>>});
subscribed(info, {tcp_error, Socket, Reason}, #data {socket = Socket} = Data) ->
    shackle_utils:warning_msg(?MODULE,
        "control socket error: ~p~n", [Reason]),
    _ = gen_tcp:close(Socket),
    backoff(Data#data {socket = undefined, buffer = <<>>});
subscribed(_, _, Data) ->
    {keep_state, Data}.

%% private
backoff(#data {backoff = B} = Data) ->
    Next = min(B * 2, ?BACKOFF_MAX),
    {next_state, connecting, Data#data {backoff = Next},
        [{state_timeout, B, reconnect}]}.

establish([], _Port) ->
    {error, no_reachable_seed};
establish([Ip | Rest], Port) ->
    case marina_utils:connect(Ip, Port) of
        {ok, Socket} ->
            case handshake(Socket) of
                {ok, Nodes} ->
                    {ok, Socket, Nodes};
                {error, Reason} ->
                    _ = gen_tcp:close(Socket),
                    shackle_utils:warning_msg(?MODULE,
                        "seed ~s handshake failed: ~p~n", [Ip, Reason]),
                    establish(Rest, Port)
            end;
        {error, Reason} ->
            shackle_utils:warning_msg(?MODULE,
                "seed ~s unreachable: ~p~n", [Ip, Reason]),
            establish(Rest, Port)
    end.

handshake(Socket) ->
    case marina_utils:startup(Socket) of
        {ok, undefined} ->
            discover(Socket);
        {ok, _Authenticator} ->
            case marina_utils:authenticate(Socket) of
                ok -> discover(Socket);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

discover(Socket) ->
    case marina_utils:query(Socket, ?LOCAL_QUERY) of
        {ok, {result, _, _, [[_, Datacenter, _]] = LocalRows}} ->
            case marina_utils:query(Socket, ?PEERS_QUERY) of
                {ok, {result, _, _, PeerRows}} ->
                    {ok, filter_dc(LocalRows ++ PeerRows, Datacenter)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

filter_dc([], _Dc) ->
    [];
filter_dc([[Addr, _, Tokens] | T], undefined) ->
    [{Addr, Tokens} | filter_dc(T, undefined)];
filter_dc([[Addr, Dc, Tokens] | T], Dc) ->
    [{Addr, Tokens} | filter_dc(T, Dc)];
filter_dc([_ | T], Dc) ->
    filter_dc(T, Dc).

subscribe(Socket) ->
    FrameFlags = marina_utils:frame_flags(),
    Msg = marina_request:register(?CONTROL_STREAM, FrameFlags, ?EVENT_TYPES),
    case marina_utils:sync_msg(Socket, Msg) of
        {ok, undefined} -> ok;
        {ok, Other} -> {error, {unexpected_register_response, Other}};
        {error, Reason} -> {error, Reason}
    end.

process_frames([]) ->
    continue;
process_frames([#frame {stream = ?EVENT_STREAM} = Frame | Rest]) ->
    case marina_body:decode(Frame) of
        {ok, {event, topology_change, Kind, Addr}} ->
            shackle_utils:warning_msg(?MODULE,
                "topology change: ~p ~p~n", [Kind, Addr]),
            cycle;
        {ok, {event, status_change, Kind, Addr}} ->
            shackle_utils:warning_msg(?MODULE,
                "status change: ~p ~p~n", [Kind, Addr]),
            process_frames(Rest);
        {ok, {event, schema_change, _Body}} ->
            process_frames(Rest);
        {ok, Other} ->
            shackle_utils:warning_msg(?MODULE,
                "unexpected event frame: ~p~n", [Other]),
            process_frames(Rest)
    end;
process_frames([_Frame | Rest]) ->
    %% stray non-event response (e.g. late STARTUP reply) — ignore
    process_frames(Rest).
