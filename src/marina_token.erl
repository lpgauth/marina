-module(marina_token).
-include("marina_internal.hrl").

-export([
    m3p/1
]).

%% public
-spec m3p(binary()) ->
    integer().

m3p(Key) ->
    <<Hash:64/signed-little-integer, _/binary>> =
        murmur:murmur3_cassandra_x64_128(Key),
    Hash.
