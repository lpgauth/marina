-module(marina_compiler).
-include("marina_internal.hrl").

-export([
    ring_utils/1
]).

%% public
-spec ring_utils([{{integer(), integer()}, binary()}]) ->
    ok.

ring_utils(Ring) ->
    compile_and_load_forms(forms(Ring)),
    ok.

%% private
compile_and_load_forms(Forms) ->
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:soft_purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin).

forms(Ring) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(marina_ring_utils)]),
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:atom(lookup),
        erl_syntax:integer(1))],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function = erl_syntax:function(erl_syntax:atom(lookup),
        lookup_clauses(Ring)),
    Mod = [Module, Export, Function],
    [erl_syntax:revert(X) || X <- Mod].

lookup_clause(Range, RpcAddress) ->
    Patterns = [erl_syntax:variable('Token')],
    Guard = range_guard(Range),
    Body = [erl_syntax:tuple([erl_syntax:atom(ok),
        erl_syntax:atom(marina_pool:node_id(RpcAddress))])],
    erl_syntax:clause(Patterns, Guard, Body).

lookup_clauses(Ring) ->
    lookup_clauses(Ring, []).

lookup_clauses([], Acc) ->
    lists:reverse(Acc);
lookup_clauses([{Range, HostId} | T], Acc) ->
    lookup_clauses(T, [lookup_clause(Range, HostId) | Acc]).

range_guard({undefined, End}) ->
    Var = erl_syntax:variable('Token'),
    Guard = erl_syntax:infix_expr(Var, erl_syntax:operator('=<'),
        erl_syntax:integer(End)),
    [Guard];
range_guard({Start, undefined}) ->
    Var = erl_syntax:variable('Token'),
    Guard = erl_syntax:infix_expr(Var, erl_syntax:operator('>'),
        erl_syntax:integer(Start)),
    [Guard];
range_guard({Start, End}) ->
    Var = erl_syntax:variable('Token'),
    Guard1 = erl_syntax:infix_expr(Var, erl_syntax:operator('>'),
        erl_syntax:integer(Start)),
    Guard2 = erl_syntax:infix_expr(Var, erl_syntax:operator('=<'),
        erl_syntax:integer(End)),
    [Guard1, Guard2].
