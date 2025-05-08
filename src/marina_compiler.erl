-module(marina_compiler).
-include("marina_internal.hrl").

-export([
    ring_utils/1
]).

%% public
-spec ring_utils(tuple()) -> ok.

ring_utils(Tree) ->
    compile_and_load_forms(forms(Tree)),
    ok.

%% private
compile_and_load_forms(Forms) ->
    {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
    code:soft_purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin).

forms(Tree) ->
    Module = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(marina_ring_utils)]),
    ExportList = [
        erl_syntax:arity_qualifier(erl_syntax:atom(lookup), erl_syntax:integer(1)),
        erl_syntax:arity_qualifier(erl_syntax:atom(lookup_tree), erl_syntax:integer(1))
    ],
    Export = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportList)]),
    Function = erl_syntax:function(erl_syntax:atom(lookup),
        [tree_lookup_clause(Tree)]),
    Mod = [Module, Export, Function],
    [erl_syntax:revert(X) || X <- Mod].

tree_lookup_clause(Tree) ->
    Var = erl_syntax:variable('Token'),
    Body = [tree_lookup_body(Tree, Var, find_rval(Tree))],
    erl_syntax:clause([Var], none, Body).

find_rval({_M, _L, R}) -> find_rval(R);
find_rval([_L, RVal]) -> RVal;
find_rval([RVal]) -> RVal.

tree_lookup_body(Tree, Var, RVal) ->
    case Tree of
        % intermediate
        {{Bound, _} = LVal, Left, Right} ->
            LGuard = erl_syntax:infix_expr(Var, erl_syntax:operator('<'),
                erl_syntax:integer(Bound)),
            LBody = [tree_lookup_body(Left, Var, LVal)],
            LClause = erl_syntax:clause([Var], LGuard, LBody),

            RBody = [tree_lookup_body(Right, Var, RVal)],
            RClause = erl_syntax:clause([erl_syntax:underscore()], none, RBody),

            erl_syntax:case_expr(Var, [LClause, RClause]);
        % complete leaf
        [{LBound, LAddr}, {RBound, MAddr}] ->
            {_, RAddr} = RVal,
            LGuard = erl_syntax:infix_expr(Var, erl_syntax:operator('<'),
                erl_syntax:integer(LBound)),
            LBody = [erl_syntax:tuple([erl_syntax:atom(ok),
                erl_syntax:atom(marina_pool:node_id(LAddr))])],
            LClause = erl_syntax:clause([Var], LGuard, LBody),

            MGuard = erl_syntax:infix_expr(Var, erl_syntax:operator('<'),
                erl_syntax:integer(RBound)),
            MBody = [erl_syntax:tuple([erl_syntax:atom(ok),
                erl_syntax:atom(marina_pool:node_id(MAddr))])],
            MClause = erl_syntax:clause([Var], MGuard, MBody),

            RBody = [erl_syntax:tuple([erl_syntax:atom(ok),
                erl_syntax:atom(marina_pool:node_id(RAddr))])],
            RClause = erl_syntax:clause([erl_syntax:underscore()], none, RBody),

            erl_syntax:case_expr(Var, [LClause, MClause, RClause]);
        % incomplete leaf
        [{LBound, LAddr}] ->
            {_, RAddr} = RVal,
            LGuard = erl_syntax:infix_expr(Var, erl_syntax:operator('<'),
                erl_syntax:integer(LBound)),
            LBody = [erl_syntax:tuple([erl_syntax:atom(ok),
                erl_syntax:atom(marina_pool:node_id(LAddr))])],
            LClause = erl_syntax:clause([Var], LGuard, LBody),

            RBody = [erl_syntax:tuple([erl_syntax:atom(ok),
                erl_syntax:atom(marina_pool:node_id(RAddr))])],
            RClause = erl_syntax:clause([erl_syntax:underscore()], none, RBody),

            erl_syntax:case_expr(Var, [LClause, RClause])
    end.
