-module(marina_profile).
-include("test.hrl").

-export([
    fprofx/0
]).

-define(N, 1000).
-define(P, 20).

%% public
-spec fprofx() -> ok.

fprofx() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") ||
        Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    Self = self(),
    Query = <<"SELECT * FROM rtb.users WHERE key = ?;">>,
    Uid = <<115, 123, 69, 190, 177, 106, 17, 229, 160, 86, 27, 159, 238, 8, 37,
        193>>,

    marina_app:start(),

    [spawn(fun () ->
        [marina:reusable_query(Query, 10, [Uid], 1000) ||
            _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    marina_app:stop(),
    ok.

%% private
wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
