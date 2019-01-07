-module(symbol_table).

-behaviour(gen_server).

-export([new/1, new/2, insert/4, fetch/2, count_class_field_vars/1]).

-export([init/1, handle_call/3, handle_cast/2]).

%% APIs
new(ClassName) ->
    {ok, _} = gen_server:start_link({local, ClassName}, ?MODULE, [], []),
    ok.

new(ClassName, SubroutineName) ->
    ProcName = list_to_atom(string:join([atom_to_list(ClassName),
                                         atom_to_list(SubroutineName)],
                                        ".")),
    {ok, _} = gen_server:start_link({local, ProcName}, ?MODULE, [], []),
    ok.

insert({ClassName}, Kind, Type, Names) ->
    ok = gen_server:call(ClassName, {insert, Kind, Type, Names});
insert({ClassName, Name}, Kind, Type, Names) ->
    ProcName = list_to_atom(string:join([atom_to_list(ClassName),
                                         atom_to_list(Name)],
                                        ".")),
    ok = gen_server:call(ProcName, {insert, Kind, Type, Names}).

fetch({ClassName, Name}, VarName) ->
    ProcName = list_to_atom(string:join([atom_to_list(ClassName),
                                         atom_to_list(Name)],
                                        ".")),
    case gen_server:call(ProcName, {fetch, VarName}) of
        not_found ->
            gen_server:call(ClassName, {fetch, VarName});
        V -> V
    end.

count_class_field_vars(ClassName) ->
    gen_server:call(ClassName, {count_field_vars}).
                                                             
%% callbacks
init([]) ->
    {ok, []}.

handle_call({insert, Kind, Type, Names}, _From, State) when is_list(Names) ->
    NewState = insert_names(State, [{Kind, Type, Name} || Name <- Names]),
    {reply, ok, NewState};
handle_call({insert, Kind, Type, Name}, _From, State) when is_atom(Name) ->
    NewState = insert_name(State, {Kind, Type, Name}),
    {reply, ok, NewState};
handle_call({fetch, VarName}, _From, State) ->
    case lists:keysearch(VarName, 1, State) of
        {value, Tuple} -> {reply, Tuple, State};
        false -> {reply, not_found, State}
    end;
handle_call({count_field_vars}, _From, State) ->
    N = count_list(fun ({_, K, _, _}) -> K =:= field end, State),
    {reply, N, State}.

handle_cast(_R, S) ->
    {noreply, S}.
             
%% internal functions
insert_names(State, []) -> State;
insert_names(State, [Name|Names]) ->
    NewState = insert_name(State, Name),
    insert_names(NewState, Names).

insert_name(State, {Kind, Type, Name}) ->
    Number = count_list(fun ({_, K, _, _}) -> K =:= Kind end, State),
    [{Name, Kind, Type, Number}|State].

count_list(Func, List) ->
    lists:sum([1 || E <- List, Func(E) =:= true]).
