%% Variant (b) for the marina_bench comparison: ring stored as a sorted tuple
%% of {Token, NodeAtom} pairs in persistent_term. Pure-Erlang binary search.
-module(marina_ring_pt).

-export([
    build/1,
    lookup/1,
    teardown/0
]).

-define(PT_KEY, {marina_ring_pt, ring}).
-define(MAX_TOKEN, 16#7fffffffffffffff).

-spec build([{integer(), atom()}]) -> ok.
build(SortedTokens) ->
    [{_, First} | _] = SortedTokens,
    Tuple = list_to_tuple(SortedTokens ++ [{?MAX_TOKEN + 1, First}]),
    persistent_term:put(?PT_KEY, Tuple),
    ok.

-spec lookup(integer()) -> atom().
lookup(Token) ->
    Tuple = persistent_term:get(?PT_KEY),
    bsearch(Tuple, Token, 1, tuple_size(Tuple)).

-spec teardown() -> ok.
teardown() ->
    _ = persistent_term:erase(?PT_KEY),
    ok.

%% private
bsearch(Tuple, _Token, Lo, Hi) when Lo >= Hi ->
    element(2, element(Lo, Tuple));
bsearch(Tuple, Token, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    case Token < element(1, element(Mid, Tuple)) of
        true -> bsearch(Tuple, Token, Lo, Mid);
        false -> bsearch(Tuple, Token, Mid + 1, Hi)
    end.
