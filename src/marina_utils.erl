-module(marina_utils).
-include("marina.hrl").

-export([
    childs_specs/0,
    child_name/1,
    info_msg/2,
    timeout/2,
    warning_msg/2

]).

%% public
childs_specs() ->
    PoolSize = application:get_env(?APP, pool_size, ?DEFAULT_POOL_SIZE),
    [child_spec(N) || N <- lists:seq(1, PoolSize)].

child_name(N) ->
    list_to_atom(?SERVER_BASE_NAME ++ integer_to_list(N)).

info_msg(Format, Data) ->
    error_logger:info_msg(Format, Data).

timeout(Timestamp, Timeout) ->
    Diff = timer:now_diff(os:timestamp(), Timestamp) div 1000,
    case Timeout - Diff of
        X when X < 0 -> 0;
        X -> X
    end.

warning_msg(Format, Data) ->
    error_logger:warning_msg(Format, Data).

%% private
child_spec(N) ->
    Name = child_name(N),
    Module = marina_server,
    StartFunc = {Module, start_link, [Name]},
    {Name, StartFunc, permanent, 5000, worker, [Module]}.
