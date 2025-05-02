-module(marina_ring).
-include("marina_internal.hrl").

-ignore_xref([
    {marina_ring_utils, lookup_tree, 1}
]).

-dialyzer({nowarn_function, lookup/1}).

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
    Sorted = lists:usort(lists:flatten(Ring)),
    Tree = build_tree(Sorted),
    Ranges = ranges(Sorted),
    marina_compiler:ring_utils(Ranges, Tree).

-spec lookup(routing_key()) ->
    atom().

lookup(Key) ->
    marina_ring_utils:lookup_tree(marina_token:m3p(Key)).

%% private
ranges(Ring) ->
    ranges(Ring, undefined, []).

ranges([], LastToken, Acc) ->
    [{_Range, HostId} | _] = Ranges = lists:reverse(Acc),
    Ranges ++ [{{LastToken, undefined}, HostId}];
ranges([{Token, HostId} | T], LastToken, Acc) ->
    ranges(T, Token, [{{LastToken, Token}, HostId} | Acc]).

build_tree(Sorted) ->
    [{_, First} | _] = Sorted,
    Input = Sorted ++ [{undefined, First}],
    build_tree(Input, length(Input)).

build_tree([{Token, RpcAddress}], 1) ->
    [{Token, RpcAddress}];
build_tree([{LToken, LRpc}, {RToken, RRpc}], 2) ->
    [{LToken, LRpc}, {RToken, RRpc}];
build_tree(Bounds, Length) ->
    LLength = Length div 2,
    RLength = Length - LLength - 1,
    {LBounds, RBounds0} = lists:split(LLength, Bounds),
    [{Token, RpcAddress} | RBounds] = RBounds0,
    {{Token, RpcAddress}, build_tree(LBounds, LLength), build_tree(RBounds, RLength)}.
