-module(jack_parse).

-export([scan_and_parse/1]).

-export_type([class/0]).

scan_and_parse(SourceFile) ->
    Toks = tokenize_file(SourceFile),
    parse(Toks).

tokenize_file(SourceFile) ->
    {ok, Data} = file:read_file(SourceFile),
    Code = string:concat(binary_to_list(Data), "\n"),
    {ok, Toks_with_comment, _} = jack_tok:string(Code),
    [Tok || Tok <- Toks_with_comment, Tok /= {comment}].

parse(Toks) ->
    {Class, []} = parse_class(Toks),
    Class.

%%
parse_multi(Toks, Fun, Delim) ->
    parse_multi(Toks, Fun, Delim, []).

parse_multi(Toks, Fun, Delim, Acc) ->
    try Fun(Toks) of
        {A, R} ->
            case Delim of
                nil ->
                    parse_multi(R, Fun, Delim, [A|Acc]);
                _ ->
                    case R of
                        [Delim|R1] ->
                            parse_multi(R1, Fun, Delim, [A|Acc]);
                        _ ->
                            {lists:reverse([A|Acc]), R}
                    end
            end
    catch
        _:_ ->
            {lists:reverse(Acc), Toks}
    end.

%%
parse_class(Toks) ->
    [{keyword, class}, {identifier, ClassName}, {symbol, '{'}|R1] = Toks,
    {ClassVarDecs, R2} = parse_classvar_decs(R1),
    {Subroutines, [{symbol, '}'}|R3]} = parse_subroutines(R2),
    {build_class(ClassName, ClassVarDecs, Subroutines), R3}.

parse_classvar_decs(Toks) ->    
    parse_multi(Toks, fun parse_classvar_dec/1, {symbol, ';'}).

parse_classvar_dec([{keyword, Qualifier}|Toks]) when Qualifier =:= static;
                                                     Qualifier =:= field ->
    {Type, R0} = parse_type(Toks),
    {Ids, R} = parse_multi(R0, fun parse_id/1, {symbol, ','}),
    {build_classvar_dec(Qualifier, Type, Ids), R}.

parse_subroutines(Toks) ->
    parse_multi(Toks, fun parse_subroutine/1, nil).

parse_subroutine([SubRoutineType, ReturnType, {identifier, SubRoutineName}, {symbol, '('}|Toks]) ->
    SRT = parse_subroutine_type(SubRoutineType),
    RT = parse_return_type(ReturnType),
    {Args, [{symbol, ')'}, {symbol, '{'}|R1]} = parse_multi(Toks, fun parse_arg/1, {symbol, ','}),
    {LocalVarDecs, R2} = parse_multi(R1, fun parse_local_var_dec/1, {symbol, ';'}),
    {Statements, [{symbol, '}'}|R3]} = parse_multi(R2, fun parse_statement/1, nil),
    {build_subroutine(SRT, RT, SubRoutineName, Args, LocalVarDecs, Statements), R3}.

parse_id([{identifier, Id}|RemToks]) -> {Id, RemToks}.

parse_arg(Toks) ->
    {Type, [{identifier, Arg}|R]} = parse_type(Toks),
    {build_arg(Type, Arg), R}.

parse_type([{keyword, void}|R]) -> {{void_type}, R};
parse_type([{keyword, Tp}|R]) ->
    Types = [int, char, boolean],
    case lists:member(Tp, Types) of
        true -> {{base_type, Tp}, R};
        false -> erlang:error({"want a type of {int, boolean, char} but found", Tp})
    end;
parse_type([{identifier, Tp}|R]) -> {{class_type, Tp}, R}.

parse_local_var_dec([{keyword, var}|Toks]) ->
    {Type, R0} = parse_type(Toks),
    {Vars, R} = parse_multi(R0, fun parse_id/1, {symbol, ','}),
    {build_local_var_dec(Type, Vars), R}.

parse_statement(Toks=[{keyword, 'if'}|_]) ->
    parse_if_statement(Toks);
parse_statement(Toks=[{keyword, 'do'}|_]) ->
    parse_do_statement(Toks);
parse_statement(Toks=[{keyword, 'let'}|_]) ->
    parse_let_statement(Toks);
parse_statement(Toks=[{keyword, 'while'}|_]) ->
    parse_while_statement(Toks);
parse_statement(Toks=[{keyword, 'return'}|_]) ->
    parse_return_statement(Toks).

parse_if_statement([{keyword, 'if'}, {symbol, '('}|R]) ->
    {Condition, [{symbol, ')'}, {symbol, '{'}|R1]} = parse_exp(R),
    {ThenBody, [{symbol, '}'}|R2]} = parse_multi(R1, fun parse_statement/1, nil),
    case R2 of
        [{keyword, 'else'}, {symbol, '{'}|R3] ->
            {ElseBody, [{symbol, '}'}|R4]} = parse_multi(R3, fun parse_statement/1, nil),
            {build_if_statement(Condition, ThenBody, ElseBody), R4};
        _ ->
            {build_if_statement(Condition, ThenBody), R2}
    end.

-type do_statement() :: {do_statement, subroutinecall()}.
parse_do_statement([{keyword, 'do'}|Toks]) ->
    {SubRoutineCall, [{symbol, ';'}|R]} = parse_subroutinecall(Toks),
    {{do_statement, SubRoutineCall}, R}.

parse_let_statement([{keyword, 'let'}|Toks]) ->
    [{identifier, Var}|R] = Toks,
    case R of
        [{symbol, '='}|R1] ->
            {Exp, [{symbol, ';'}|R2]} = parse_exp(R1),
            {build_let_statement(Var, Exp), R2};
        [{symbol, '['}|R1] ->
            {IdxExp, [{symbol, ']'}, {symbol, '='}|R2]} = parse_exp(R1),
            {Exp, [{symbol, ';'}|R3]} = parse_exp(R2),
            {build_let_statement(Var, IdxExp, Exp), R3}
    end.

parse_while_statement([{keyword, 'while'}, {symbol, '('}|Toks]) ->
    {StopCondition, [{symbol, ')'}, {symbol, '{'}|R]} = parse_exp(Toks),
    {WhileBody, [{symbol, '}'}|R1]} = parse_multi(R, fun parse_statement/1, nil),
    {build_while_statement(StopCondition, WhileBody), R1}.

parse_return_statement([{keyword, 'return'}|Toks]) ->
    case Toks of
        [{symbol, ';'}|R] ->
            {build_return_statement(), R};
        _ ->
            {Exp, [{symbol, ';'}|R]} = parse_exp(Toks),
            {build_return_statement(Exp), R}
    end.

parse_exp(Toks) ->
    parse_exp(Toks, [], []).

parse_exp(Toks, Terms, Ops) ->
    {Term, R} = parse_term(Toks),
    case R of
        [{symbol, Op}|R1] when Op =:= '+';
                               Op =:= '-';
                               Op =:= '*';
                               Op =:= '/';
                               Op =:= '<';
                               Op =:= '>';
                               Op =:= '=';
                               Op =:= '|';
                               Op =:= '&'->
            parse_exp(R1, [Term|Terms], [Op|Ops]);
        _ ->
            {build_exp(lists:reverse([Term|Terms]), lists:reverse(Ops)), R}
    end.

parse_term([{symbol, '('}|RemToks]) ->
    {Exp, [{symbol, ')'}|R]} = parse_exp(RemToks),
    {build_term_from_exp(Exp), R};
parse_term([Term={integerConstant, _}|RemToks]) -> {Term, RemToks};
parse_term([Term={stringConstant, _}|RemToks]) -> {Term, RemToks};
parse_term([{keyword, true}|RemToks]) -> {{const, true}, RemToks};
parse_term([{keyword, false}|RemToks]) -> {{const, false}, RemToks};
parse_term([{keyword, null}|RemToks]) -> {{const, null}, RemToks};
parse_term([{keyword, this}|RemToks]) -> {{const, this}, RemToks};
parse_term([{identifier, Var}, {symbol, '['}|RemToks]) ->
    {IdxExp, [{symbol, ']'}|R]} = parse_exp(RemToks),
    {build_array_ref_term(Var, IdxExp), R};
parse_term(Toks=[{identifier, _Var}, {symbol, '.'}|_]) ->
    parse_subroutinecall(Toks);
parse_term(Toks=[{identifier, _Var}, {symbol, '('}|_]) ->
    parse_subroutinecall(Toks);
parse_term([{identifier, Var}|RemToks]) ->
    {build_var_term(Var), RemToks};
parse_term([{symbol, Op}|RemToks]) when Op =:= '-';
                                        Op =:= '~' ->
    {Term, R} = parse_term(RemToks),
    {build_unary_term(Op, Term), R};
parse_term(Toks) ->
    parse_subroutinecall(Toks).

parse_subroutine_type({keyword, Type}) when Type =:= constructor;
                                            Type =:= method;
                                            Type =:= function ->
    {subroutine_type, Type}.

parse_return_type({keyword, void}) ->
    {subroutine_return_type, {void_type}};
parse_return_type({keyword, Tp}) ->
    Types = [int, char, boolean],
    case lists:member(Tp, Types) of
        true -> {subroutine_return_type, {base_type, Tp}};
        false -> erlang:error({"want a type of {int, boolean, char} but found", Tp})
    end;
parse_return_type({identifier, Tp}) ->
    {subroutine_return_type, {class_type, Tp}}.

parse_subroutinecall([{identifier, Name1}, {symbol, '.'}, {identifier, Name2}, {symbol, '('}|R]) ->
    {Args, [{symbol, ')'}|R1]} = parse_multi(R, fun parse_exp/1, {symbol, ','}),
    {build_subroutinecall(Name1, Name2, Args), R1};
parse_subroutinecall([{identifier, Name}, {symbol, '('}|R]) ->
    {Args, [{symbol, ')'}|R1]} = parse_multi(R, fun parse_exp/1, {symbol, ','}),
    {build_subroutinecall(Name, Args), R1}.

%% constructors
-type class() :: {class,
                  ClassName::atom(),
                  ClassVarDecs::[classvar_dec()],
                  Subroutines::[subroutine()]}.
build_class(ClassName, ClassVarDecs, Subroutines) ->
    {class, ClassName, ClassVarDecs, Subroutines}.

-type classvar_dec() :: {classvar_dec,
                         qualifier(),
                         jack_type(),
                         [atom()]}.
-type qualifier() :: static | field.
-type jack_type() :: {void_type}
                   | {base_type, int}
                   | {base_type, char}
                   | {base_type, boolean}
                   | {class_type, atom()}.
build_classvar_dec(Qualifier, Type, Ids) ->
    {classvar_dec, Qualifier, Type, Ids}.

-type subroutine() :: {subroutine,
                       subroutine_type(),
                       subroutine_return_type(),
                       atom(),
                       [arg()],
                       [local_var_dec()],
                       [statement()]}.
-type subroutine_type() :: constructor | method | function.
-type subroutine_return_type() :: atom().
build_subroutine(SubRoutineType, ReturnType, Name, Args, LocalVarDecs, Body) ->
    {subroutine, SubRoutineType, ReturnType, Name, Args, LocalVarDecs, Body}.

-type arg() :: {arg, jack_type(), atom()}.
build_arg(Type, Arg) ->
    {arg, Type, Arg}.

-type local_var_dec() :: {local_var_dec, jack_type(), [atom()]}.
build_local_var_dec(Tp, Vars) ->
    {local_var_dec, Tp, Vars}.

-type statement() :: if_statement()
                   | let_statement()
                   | return_statement()
                   | while_statement()
                   | do_statement().

-type else_type() :: nil | [statement()].
-type if_statement() :: {if_statement,
                         Condition::exp(),
                         Then::[statement()],
                         Else::else_type()}.
build_if_statement(Condition, Then, Else) ->
    {if_statement, Condition, Then, Else}.
build_if_statement(Condition, Then) ->
    {if_statement, Condition, Then, nil}.

-type array_ref() :: {array_ref, atom(), exp()}.
build_array_ref(Array, IdxExp) ->
    {array_ref, Array, IdxExp}.

-type let_statement() :: {let_statement,
                          Left::left(),
                          Right::exp()}.
-type left() :: array_ref() | var().
build_let_statement(Var, IdxExp, Exp) ->
    ArrayRef = build_array_ref(Var, IdxExp),
    {let_statement, ArrayRef, Exp}.
-type var() :: {var, atom()}.
build_let_statement(Var, Exp) ->
    {let_statement, {var, Var}, Exp}.

-type while_statement() :: {while,
                             exp(),
                             [statement()]}.
build_while_statement(StopCondition, Body) ->
    {while_statement, StopCondition, Body}.

-type return_statement() :: {return, return_val()}.
-type return_val() :: nil | exp().
build_return_statement() ->
    {return, nil}.

build_return_statement(Exp) ->
    {return, Exp}.

-type op() :: '+' | '-' | '*' | '/' | '&' | '|' | '=' | '>' | '<'.
-type exp() :: {exp, Terms::[term()], Ops::[op()]}.
build_exp(Terms, Ops) ->
    {exp, Terms, Ops}.

-type jack_term() :: {integerConstant, integer()}
                   | {stringConstant, string()}
                   | {const, true}
                   | {const, false}
                   | {const, null}
                   | {const, this}
                   | array_ref()
                   | var()
                   | unary_term()
                   | subroutinecall().

build_array_ref_term(Var, IdxExp) ->
    build_array_ref(Var, IdxExp).

build_var_term(Var) ->
    {var, Var}.

build_term_from_exp(Exp) ->
    {exp_term, Exp}.

-type unary_term() :: {unary, unary_op(), jack_term()}.
-type unary_op() :: '~' | '-'.
build_unary_term(Op, Term) ->
    {unary, Op, Term}.

-type subroutinecall() :: {subroutinecall,
                           NameSpace::atom(),
                           FuncName::atom(),
                           Args::[exp()]}.
build_subroutinecall(Name1, Name2, Args) ->
    {subroutinecall, Name1, Name2, Args}.

build_subroutinecall(Name1, Args) ->
    {subroutinecall, nil, Name1, Args}.
