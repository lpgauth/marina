-module(marina_sup).
-include("marina_internal.hrl").

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
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.

init([]) ->
    marina_cache:init(),
    marina_pool:init(),

    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(marina_pool_server)
    ]}}.
