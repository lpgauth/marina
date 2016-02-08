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

    marina_app:start(),
    setup(),
    marina_app:stop(),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    Self = self(),
    Query = <<"SELECT * FROM rtb.users WHERE key = ?;">>,
    Uid = <<153, 73, 45, 254, 217, 74, 17, 228, 175, 57, 88,
        244, 65, 16, 117, 125>>,
    Opts = [{skip_metadata, true}],

    marina_app:start(),

    [spawn(fun () ->
        [marina:reusable_query(Query, [Uid], ?CONSISTENCY_LOCAL_ONE,
            Opts, ?TIMEOUT) || _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    marina_app:stop(),
    ok.

%% private
setup() ->
    [marina:query(Query, ?CONSISTENCY_LOCAL_ONE, [], ?TIMEOUT) || Query <- [
        <<"DROP KEYSPACE test;">>,
        <<"CREATE KEYSPACE test WITH REPLICATION =
            {'class':'SimpleStrategy', 'replication_factor':1};">>,
        <<"CREATE TABLE test.users (key uuid, column1 text,
            column2 text, value blob, PRIMARY KEY (key, column1, column2));">>,
        <<"INSERT INTO test.users (key, column1, column2, value)
            values (99492dfe-d94a-11e4-af39-58f44110757d, 'test', 'test2',
            intAsBlob(0))">>
    ]].

wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
