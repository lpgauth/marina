%% Variant (c) for the marina_bench comparison: ring stored as a packed binary
%% (Token:64/signed + Idx:16/unsigned per entry) plus an atom tuple for
%% Idx -> NodeAtom resolution.
-module(marina_ring_bin).

-export([
    build/1,
    lookup/1,
    teardown/0
]).

-define(PT_KEY, {marina_ring_bin, ring}).
-define(MAX_TOKEN, 16#7fffffffffffffff).
-define(ENTRY_BITS, 80).

-spec build([{integer(), atom()}]) -> ok.
build(SortedTokens) ->
    [{_, First} | _] = SortedTokens,
    Uniq = lists:usort([A || {_, A} <- SortedTokens]),
    AtomTuple = list_to_tuple(Uniq),
    IdxMap = maps:from_list(lists:zip(Uniq, lists:seq(1, length(Uniq)))),
    Entries = SortedTokens ++ [{?MAX_TOKEN + 1, First}],
    Bin = << <<Tok:64/signed, (maps:get(A, IdxMap)):16/unsigned>>
             || {Tok, A} <- Entries >>,
    persistent_term:put(?PT_KEY, {Bin, AtomTuple}),
    ok.

-spec lookup(integer()) -> atom().
lookup(Token) ->
    {Bin, Atoms} = persistent_term:get(?PT_KEY),
    N = byte_size(Bin) div 10,
    Idx = bsearch(Bin, Token, 0, N - 1),
    element(Idx, Atoms).

-spec teardown() -> ok.
teardown() ->
    _ = persistent_term:erase(?PT_KEY),
    ok.

%% private
bsearch(Bin, _Token, Lo, Hi) when Lo >= Hi ->
    <<_:Lo/binary-unit:?ENTRY_BITS, _:64, Idx:16/unsigned, _/binary>> = Bin,
    Idx;
bsearch(Bin, Token, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    <<_:Mid/binary-unit:?ENTRY_BITS, MidTok:64/signed, _/binary>> = Bin,
    case Token < MidTok of
        true -> bsearch(Bin, Token, Lo, Mid);
        false -> bsearch(Bin, Token, Mid + 1, Hi)
    end.
