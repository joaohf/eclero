-module(eclero_decision_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0,
         nodes/0,
         node_status/2,
         is_health/0,
         is_health/1]).

-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).

-record(state, {nodes_up = 0 :: non_neg_integer(),
                nodes_down = 0 :: non_neg_integer(),
                nodes = sets:new() :: sets:set(atom())}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

nodes() ->
    gen_server:call(?MODULE, {nodes}).

node_status(Node, Status) ->
    gen_server:cast(?MODULE, {node_status, Node, Status}).

is_health() ->
    gen_server:call(?MODULE, {is_health}).

is_health(Nodes) ->
    gen_server:multi_call(Nodes, ?MODULE, {is_health}).

init(_Args) ->
    {ok, #state{}}.

handle_continue(_Continue, State) ->

    eclero_metric:node(1, 0),

    {noreply, State}.

handle_call({nodes}, _From, #state{nodes = Nodes} = State) ->
    NNodes = sets:to_list(Nodes),
    {reply, {ok, NNodes}, State};
handle_call({is_health}, _From, #state{nodes_up = Up} = State) when Up > 0 ->
    {reply, true, State};
handle_call({is_health}, _From, #state{nodes_up = 0} = State) ->
    {reply, false, State}.

handle_cast({node_status, Node, down}, #state{nodes_up = Up,
                                              nodes_down = Down,
                                              nodes = Nodes} = State) ->
    NDown = Down + 1,
    NUp = decrease(Up),

    eclero_metric:node(NUp, NDown),

    {noreply, State#state{nodes_up = NUp,
                          nodes_down = NDown,
                          nodes = sets:del_element(Node, Nodes)}};

handle_cast({node_status, Node, up}, #state{nodes_up = Up,
                                            nodes_down = Down,
                                            nodes = Nodes} = State) ->
    NDown = decrease(Down),
    NUp = Up + 1,

    eclero_metric:node(NUp, NDown),

    {noreply, State#state{nodes_up = NUp,
                          nodes_down = NDown,
                          nodes = sets:add_element(Node, Nodes)}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

decrease(0) ->
    0;
decrease(Value) ->
    Value -1.
