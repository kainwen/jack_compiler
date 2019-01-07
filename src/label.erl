-module(label).

-behaviour(gen_server).

-export([start/0, new/1]).

-export([init/1, handle_call/3, handle_cast/2]).

% APIs
start() ->
    {ok, _} = gen_server:start_link({local, label_server}, ?MODULE, [], []),
    ok.

new(Prefix) ->
    gen_server:call(label_server, {new, Prefix}).

% callbacks
init([]) ->
    {ok, 0}.

handle_call({new, Prefix}, _From, State) ->
    Label = string:join([atom_to_list(Prefix), integer_to_list(State)], "_"),
    {reply, Label, State+1}.

handle_cast(_R, S) ->
    {noreply, S}.
