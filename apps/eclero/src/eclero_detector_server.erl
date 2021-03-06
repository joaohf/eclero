-module(eclero_detector_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).

-record(state, {nodes = #{} :: map(),
                detector_module = eclero_detector :: atom()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),

    Nodes0 = application:get_env(eclero, nodes, []),
    Module = application:get_env(eclero, detector_module, eclero_detector),

    Nodes1 = merge_nodes(Nodes0),

    {ok, #state{detector_module = Module}, {continue, {register, Nodes1}}}.

handle_continue({register, RNodes}, #state{detector_module = DM,
                                           nodes = Nodes} = State) ->
    {ok, INodes} = register_interest(DM, RNodes, Nodes),

    {noreply, State#state{nodes = INodes}, {continue, up}};

handle_continue(up, #state{detector_module = DM, nodes = Nodes} = State) ->
    Node = eclero_detector:node(DM),
    Event = up,
    Nodes0 = update_interest(Nodes, Node, Event),

    ok = eclero_decision_server:node_status(Node, Event),

    {noreply, State#state{nodes = Nodes0}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({node_event, Node, Event}, #state{nodes = Nodes} = State) ->
    Nodes0 = update_interest(Nodes, Node, Event),

    ok = eclero_decision_server:node_status(Node, Event),

    {noreply, State#state{nodes = Nodes0}}.

terminate(_Reason, #state{detector_module = DM, nodes = Nodes}) ->
    deregister_interest(DM, Nodes),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

merge_nodes(Nodes) ->
    Node = erlang:node(),
    lists:umerge(Nodes, [Node]).


register_interest(_DetectorModule, [], INodes) ->
    {ok, INodes};

register_interest(DetectorModule, [Node | Nodes], INodes) ->
    {ok, Status} = eclero_detector:register(DetectorModule, Node),

    INodes0 = maps:put(Node, Status, INodes),

    register_interest(DetectorModule, Nodes, INodes0).


deregister_interest(DetectorModule, INodes) when is_map(INodes) ->
    Fun = fun(Node, _Status) -> deregister_interest(DetectorModule, Node) end,
    maps:map(Fun, INodes);

deregister_interest(DetectorModule, Node) when is_atom(Node) ->
    ok = eclero_detector:unregister(DetectorModule, Node),
    undefiend.


update_interest(Nodes, Node, Event) ->
    Fun = fun(_V) -> Event end,
    maps:update_with(Node, Fun, Nodes).
