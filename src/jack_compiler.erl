-module(jack_compiler).

-export([code_gen/2]).

code_gen({}, {class, ClassName, ClassVarDecs, Subroutines}) ->
    ok = symbol_table:new(ClassName),
    ok = build_class_vars(ClassName, ClassVarDecs),
    flatten([code_gen({ClassName}, Subroutine)
             || Subroutine <- Subroutines]);
code_gen({ClassName},
         {subroutine, {subroutine_type, SRT}, _RT, Name, Args, LocalVarDecs, Body}) when SRT /= constructor ->
    ok = symbol_table:new(ClassName, Name),
    case SRT of
        method ->
            ok = symbol_table:insert({ClassName, Name},
                                     argument, {class_type, ClassName}, this);
        _ ->
            ok
    end,
    ok = build_function_args({ClassName, Name}, Args),
    ok = build_function_local_vars({ClassName, Name}, LocalVarDecs),
    DefineCode = [
                  string:join(["function",
                               build_func_name(ClassName, Name),
                               integer_to_list(count_local_vars(LocalVarDecs))],
                              " ")
                 ],
    Push_this_code = case SRT of
                         method ->
                             [
                              "push argument 0",
                              "pop pointer 0"
                             ];
                         _ ->
                             []
                     end,
    DefineCode ++
    Push_this_code ++
    flatten([code_gen({ClassName, Name, SRT}, Statement)
             || Statement <- Body]);
code_gen({ClassName},
         {subroutine, {subroutine_type, constructor}, _RT, Name, Args, LocalVarDecs, Body}) ->
    ok = symbol_table:new(ClassName, Name),
    ok = build_function_args({ClassName, Name}, Args),
    ok = build_function_local_vars({ClassName, Name}, LocalVarDecs),
    DefineCode = [
                  string:join(["function",
                               build_func_name(ClassName, Name),
                               integer_to_list(count_local_vars(LocalVarDecs))],
                              " ")
                 ],
    NumFieldVars = symbol_table:count_class_field_vars(ClassName),
    AllocCode = [
                 string:concat("push constant ", integer_to_list(NumFieldVars)),
                 "call Memory.alloc 1",
                 "pop pointer 0"
                ],
    BodyCode = flatten([code_gen({ClassName, Name, constructor}, Statement)
                        || Statement <- Body]),
    DefineCode ++ AllocCode ++ BodyCode;
code_gen({ClassName, Name, SRT},
         {do_statement, SubRoutineCall}) ->
    % Name1 can be nil, variable_name, class_name
    % Name1 is nil, we should push 'this' as the 1st arg
    % Name1 is variable_name, we should push it as the 1 as arg
    code_gen({ClassName, Name, SRT}, SubRoutineCall) ++
    ["pop temp 0"];
code_gen({ClassName, Name, SRT}, {let_statement, {var, Var}, Exp}) ->
    {Var, Kind, _Type, Number} = symbol_table:fetch({ClassName, Name}, Var),
    NewKind = case Kind of
                  field -> this;
                  _ -> Kind
              end,
    code_gen({ClassName, Name, SRT}, Exp) ++
    [
     string:join(["pop", atom_to_list(NewKind), integer_to_list(Number)], " ")
    ];
code_gen({ClassName, Name, SRT}, {let_statement, {array_ref, Array, IdxExp}, Exp}) ->
    {Array, Kind, _Type, Number} = symbol_table:fetch({ClassName, Name}, Array),
    NewKind = case Kind of
                  field -> this;
                  _ -> Kind
              end,
    code_gen({ClassName, Name, SRT}, IdxExp) ++
    [
     string:join(["push", atom_to_list(NewKind), integer_to_list(Number)], " ")
    ] ++
    [
     "add"
    ] ++
    code_gen({ClassName, Name, SRT}, Exp) ++
    [
     "pop temp 0",
     "pop pointer 1",
     "push temp 0",
     "pop that 0"
    ];
code_gen({_ClassName, _Name, _SRT}, {return, nil}) ->
    [
     "push constant 0",
     "return"
    ];
code_gen({ClassName, Name, SRT}, {return, Exp}) ->
    code_gen({ClassName, Name, SRT}, Exp) ++
    [
     "return"
    ];
code_gen({ClassName, Name, SRT}, {if_statement, Condition, Then, nil}) ->
    % condition
    % if-goto then
    % goto if-end
    % label then
    % code of then
    % goto if-end
    % label if-end
    ThenLabel = label:new('then'),
    IfEndLabel = label:new('ifend'),
    code_gen({ClassName, Name, SRT}, Condition) ++
    [
     string:concat("if-goto ", ThenLabel),
     string:concat("goto ", IfEndLabel),
     string:concat("label ", ThenLabel)
    ] ++
    flatten([code_gen({ClassName, Name, SRT}, S)
             || S <- Then]) ++
    [
     string:concat("label ", IfEndLabel)
    ];
code_gen({ClassName, Name, SRT}, {if_statement, Condition, Then, Else}) ->
    % condition code
    % if-goto then
    % goto else
    % label then
    % ...
    % goto ifend
    % label else
    % else code
    % ifend
    ThenLabel = label:new('then'),
    ElseLabel = label:new('else'),
    IfEndLabel = label:new(ifend),
    code_gen({ClassName, Name, SRT}, Condition) ++
    [
     string:concat("if-goto ", ThenLabel),
     string:concat("goto ", ElseLabel),
     string:concat("label ", ThenLabel)
    ] ++
    flatten([code_gen({ClassName, Name, SRT}, S)
             || S <- Then]) ++
    [
     string:concat("goto ", IfEndLabel),
     string:concat("label ", ElseLabel)
    ] ++
    flatten([code_gen({ClassName, Name, SRT}, S)
             || S <- Else]) ++
    [
     string:concat("label ", IfEndLabel)
    ];
code_gen({ClassName, Name, SRT}, {while_statement, StopCondition, Body}) ->
    % label while_stop_condition
    % stopcondition code
    % not
    % if-goto while-end
    % body code
    % goto while_stop_condition
    % while-end
    WhileStartLabel = label:new(while_start),
    WhileEndLabel = label:new(while_end),
    [
     string:concat("label ", WhileStartLabel)
    ] ++
    code_gen({ClassName, Name, SRT}, StopCondition) ++
    [
     "not",
     string:concat("if-goto ", WhileEndLabel)
    ] ++
    flatten([code_gen({ClassName, Name, SRT}, S)
             || S <- Body]) ++
    [
     string:concat("goto ", WhileStartLabel),
     string:concat("label ", WhileEndLabel)
    ];
%% code gen for exps
code_gen({ClassName, Name, SRT}, {exp, [Term], []}) ->
    code_gen({ClassName, Name, SRT}, Term);
code_gen({ClassName, Name, SRT}, {exp, [Term1, Term2|Terms], [Op|Ops]}) ->
    code_gen({ClassName, Name, SRT}, Term1) ++
    code_gen({ClassName, Name, SRT}, Term2) ++
    [
     get_instruction_from_op(Op)
    ] ++
    flatten([code_gen({ClassName, Name, SRT}, T) ++
             code_gen({ClassName, Name, SRT}, O)
             || {T, O} <- lists:zip(Terms, Ops)]);
%% code gen for terms
code_gen({ClassName, Name, SRT}, {exp_term, Exp}) ->
    code_gen({ClassName, Name, SRT}, Exp);
code_gen({_ClassName, _Name, _SRT}, {integerConstant, Int}) ->
    [
     string:concat("push constant ", integer_to_list(Int))
    ];
code_gen({_ClassName, _Name, _SRT}, {stringConstant, S}) ->
    StrLen = length(S),
    [
     string:concat("push constant ", integer_to_list(StrLen)),
     "call String.new 1"
    ] ++
    flatten([[
              string:concat("push constant ", integer_to_list(C)),
              "call String.appendChar 2"
             ]
             || C <- S]);
code_gen({_ClassName, _Name, _SRT}, {const, true}) ->
    [
     "push constant 0",
     "not"
    ];
code_gen({_ClassName, _Name, _SRT}, {const, false}) ->
    [
     "push constant 0"
    ];
code_gen({_ClassName, _Name, _SRT}, {const, null}) ->
    [
     "push constant 0"
    ];
code_gen({_ClassName, _Name, _SRT}, {const, this}) ->
    [
     "push pointer 0"
    ];
code_gen({ClassName, Name, _SRT}, {var, Var}) ->
    {Var, Kind, _Type, Number} = symbol_table:fetch({ClassName, Name}, Var),
    NewKind = case Kind of
                  field -> this;
                  _ -> Kind
              end,
    [
     string:join(["push", atom_to_list(NewKind), integer_to_list(Number)], " ")
    ];
code_gen({ClassName, Name, SRT}, {array_ref, Array, IdxExp}) ->
    code_gen({ClassName, Name, SRT}, IdxExp) ++
    code_gen({ClassName, Name, SRT}, {var, Array}) ++
    [
     "add",
     "pop pointer 1",
     "push that 0"
    ];
code_gen({ClassName, Name, SRT}, {subroutinecall, Name1, Name2, Args}) ->
    {ArgsCode, ExtraArg, CName} = case Name1 of
                                      nil ->
                                          true = lists:member(SRT, [method, constructor]),
                                          {["push pointer 0"] ++
                                               flatten([code_gen({ClassName, Name, SRT}, Arg)
                                                        || Arg <- Args]),
                                           1,
                                           nil};
                                      _ ->
                                          case symbol_table:fetch({ClassName, Name}, Name1) of
                                              {Name1, _Kind, {class_type, CN}, _Number} ->
                                                  Arg0 = {exp, [{var, Name1}], []},
                                                  {flatten([code_gen({ClassName, Name, SRT}, Arg)
                                                            || Arg <- [Arg0|Args]]),
                                                   1,
                                                   CN};
                                              not_found ->
                                                  {flatten([code_gen({ClassName, Name, SRT}, Arg)
                                                            || Arg <- Args]),
                                                   0,
                                                   Name1}
                                          end
                                  end,
    FuncName = build_func_name(ClassName, CName, Name2),
    ArgsCode ++
    [
     string:join(["call", FuncName,
                  integer_to_list(length(Args) + ExtraArg)],
                 " ")
    ];
code_gen({ClassName, Name, SRT}, {unary, Op, Term}) ->
    code_gen({ClassName, Name, SRT}, Term) ++
    [
     case Op of
         '-' -> "neg";
         '~' -> "not"
     end
    ].

%% internal helpers
build_class_vars(_ClassName, []) -> ok;
build_class_vars(ClassName, [ClassVarDec|ClassVarDecs]) -> 
    ok = build_class_var(ClassName, ClassVarDec),
    ok = build_class_vars(ClassName, ClassVarDecs).

build_class_var(ClassName, {classvar_dec, Qualifier, Type, Ids}) ->
    ok = symbol_table:insert({ClassName}, Qualifier, Type, Ids).

build_function_args({ClassName, Name}, Args) ->
    ok = lists:foreach(fun ({arg, Type, Arg}) ->
                               ok = symbol_table:insert({ClassName, Name},
                                                        argument, Type, Arg)
                       end, Args).

build_function_local_vars({_ClassName, _Name}, []) -> ok;
build_function_local_vars({ClassName, Name}, [LocalVarDec|LocalVarDecs]) ->
    ok = build_function_local_var({ClassName, Name}, LocalVarDec),
    ok = build_function_local_vars({ClassName, Name}, LocalVarDecs).

build_function_local_var({ClassName, Name}, {local_var_dec, Type, Vars}) ->
    ok = symbol_table:insert({ClassName, Name}, local, Type, Vars).

count_local_vars([]) -> 0;
count_local_vars([{_, _, Vars}|LocalVarDecs]) -> 
    length(Vars) + count_local_vars(LocalVarDecs).

flatten(Ls) ->
    flatten(Ls, []).

flatten([], Acc) -> Acc;
flatten([L|Ls], Acc) ->
    flatten(Ls, Acc ++ L).
    

build_func_name(ClassName, nil, Name2) ->
    build_func_name(ClassName, Name2);
build_func_name(_ClassName, Name1, Name2) ->
    build_func_name(Name1, Name2).

build_func_name(Name1, Name2) ->
    S1 = atom_to_list(Name1),
    S2 = atom_to_list(Name2),
    string:join([S1, S2], ".").

get_instruction_from_op(Op) ->
    Alist = [
             {'+', "add"},
             {'-', "sub"},
             {'*', "call Math.multiply 2"},
             {'/', "call Math.divide 2"},
             {'&', "and"},
             {'|', "or"},
             {'=', "eq"},
             {'<', "lt"},
             {'>', "gt"}
            ],
    {_, Code} = lists:keyfind(Op, 1, Alist),
    Code.
