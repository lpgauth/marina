-module(marina_app).
-include("marina_internal.hrl").

-export([
    start/0,
    stop/0
]).

-behaviour(application).
-export([
    prep_stop/1,
    start/2,
    stop/1
]).

%% public
-spec start() -> {ok, [atom()]}.

start() ->
    application:ensure_all_started(?APP).

-spec stop() -> ok | {error, {not_started, ?APP}}.

stop() ->
    application:stop(?APP).

%% application callbacks
-spec prep_stop(term()) -> term().

prep_stop(State) ->
    marina_pool:stop(),
    State.

-spec start(application:start_type(), term()) -> {ok, pid()}.

start(_StartType, _StartArgs) ->
    marina_sup:start_link().

-spec stop(term()) -> ok.

stop(_State) ->
    ok.
