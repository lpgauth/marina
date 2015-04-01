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
init([]) ->
    marina_backlog:init(),
    marina_cache:init(),
    marina_queue:init(),

    {ok, {{one_for_one, 5, 10},
        childs_specs()
    }}.

%% pivate
child_name(N) ->
    list_to_atom(?SERVER_BASE_NAME ++ integer_to_list(N)).

child_spec(N) ->
    Name = child_name(N),
    Module = marina_server,
    StartFunc = {Module, start_link, [Name]},
    {Name, StartFunc, permanent, 5000, worker, [Module]}.

childs_specs() ->
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    [child_spec(N) || N <- lists:seq(1, PoolSize)].
