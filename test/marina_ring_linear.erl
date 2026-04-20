%% Variant (d) for the marina_bench comparison: the pre-9be2a67 linear-scan
%% implementation of marina_ring_utils:lookup/1 — one generated clause per ring
%% entry with guard `Token > Start, Token =< End`. Kept in this test-only
%% module so we can quantify the gap to the current tree-based version and
%% confirm the production pain point.
%%
%% Generates a separate module `marina_ring_linear_utils` on demand.
-module(marina_ring_linear).

-export([
    build/1,
    lookup/1,
    teardown/0
]).

-define(GENERATED, marina_ring_linear_utils).

-spec build([{integer(), atom()}]) -> ok.
build(SortedTokens) ->
    Ranges = ranges(SortedTokens),
    compile_and_load(Ranges),
    ok.

-spec lookup(integer()) -> atom().
lookup(Token) ->
    {ok, Atom} = ?GENERATED:lookup(Token),
    Atom.

-spec teardown() -> ok.
teardown() ->
    _ = code:soft_purge(?GENERATED),
    _ = code:delete(?GENERATED),
    ok.

%% private
ranges(Ring) ->
    ranges(Ring, undefined, []).

ranges([], LastToken, Acc) ->
    [{_Range, Atom} | _] = Rs = lists:reverse(Acc),
    Rs ++ [{{LastToken, undefined}, Atom}];
ranges([{Token, Atom} | T], LastToken, Acc) ->
    ranges(T, Token, [{{LastToken, Token}, Atom} | Acc]).

compile_and_load(Ranges) ->
    Forms = forms(Ranges),
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    _ = code:soft_purge(Module),
    {module, Module} = code:load_binary(Module,
        atom_to_list(Module) ++ ".erl", Bin),
    ok.

forms(Ranges) ->
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(?GENERATED)]),
    Exp = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([erl_syntax:arity_qualifier(
            erl_syntax:atom(lookup), erl_syntax:integer(1))])]),
    Fun = erl_syntax:function(erl_syntax:atom(lookup),
        [clause(R, A) || {R, A} <- Ranges]),
    [erl_syntax:revert(X) || X <- [Mod, Exp, Fun]].

clause(Range, Atom) ->
    Var = erl_syntax:variable('Token'),
    Body = [erl_syntax:tuple([erl_syntax:atom(ok),
        erl_syntax:atom(Atom)])],
    erl_syntax:clause([Var], guard(Range), Body).

guard({undefined, End}) ->
    [infix(erl_syntax:variable('Token'), '=<', End)];
guard({Start, undefined}) ->
    [infix(erl_syntax:variable('Token'), '>', Start)];
guard({Start, End}) ->
    [infix(erl_syntax:variable('Token'), '>', Start),
     infix(erl_syntax:variable('Token'), '=<', End)].

infix(Var, Op, Int) ->
    erl_syntax:infix_expr(Var, erl_syntax:operator(Op),
        erl_syntax:integer(Int)).
