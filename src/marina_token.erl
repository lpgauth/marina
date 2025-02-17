-module(marina_token).
-include("marina_internal.hrl").

-export([
    m3p/1
]).

%% public
-spec m3p(integer() | binary()) ->
    integer().

m3p(Key) when is_integer(Key) ->
    Key;

m3p(Key) when is_binary(Key) ->
    <<Hash:64/signed-little-integer, _/binary>> =
        murmur:murmur3_cassandra_x64_128(Key),
    Hash.
