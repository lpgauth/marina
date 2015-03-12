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
    marina_queue:init(),

    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(marina_server_1, marina_server)
    ]}}.
