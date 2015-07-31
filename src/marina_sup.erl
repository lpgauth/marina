-module(marina_sup).
-include("marina.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    BacklogSize = application:get_env(?APP, backlog_size, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    PoolStrategy = application:get_env(?APP, pool_strategy, ?DEFAULT_POOL_STRATEGY),

    ok = shackle_pool:start(?APP, ?CLIENT, [
        {backlog_size, BacklogSize},
        {pool_size, PoolSize},
        {pool_strategy, PoolStrategy}
    ]),

    marina_cache:init(),

    {ok, {{one_for_one, 5, 10}, []}}.
