-module(marina_ring).
-include("marina_internal.hrl").

-ignore_xref([
    {marina_ring_utils, lookup, 1}
]).

-export([
    build/1,
    lookup/1
]).

%% public
-spec build([{binary(), binary()}]) ->
    ok.

build(Nodes) ->
    Ring = lists:map(fun ({RpcAddress, Tokens}) ->
        {Tokens2, <<>>} = marina_types:decode_long_string_set(Tokens),
        [{binary_to_integer(Token), RpcAddress} || Token <- Tokens2]
    end, Nodes),
    Ranges = ranges(lists:usort(lists:flatten(Ring))),
    marina_compiler:ring_utils(Ranges).

-spec lookup(binary()) ->
    atom().

lookup(Key) ->
    marina_ring_utils:lookup(marina_token:m3p(Key)).

%% private
ranges(Ring) ->
    ranges(Ring, undefined, []).

ranges([], LastToken, Acc) ->
    [{_Range, HostId} | _] = Ranges = lists:reverse(Acc),
    Ranges ++ [{{LastToken, undefined}, HostId}];
ranges([{Token, HostId} | T], LastToken, Acc) ->
    ranges(T, Token, [{{LastToken, Token}, HostId} | Acc]).