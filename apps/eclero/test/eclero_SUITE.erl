-module(eclero_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

all() ->
    [
        {group, system},
        {group, decision},
        {group, detector}
    ].

start_stop() ->
    [start_and_stop].

missing_nodes() ->
    [missing_one_node].

dist_tests() ->
    [ping_and_check].

decision_tests() ->
    [decision].

detector_tests() ->
    [detector].

groups() ->
    [
        {system, [], [
                      {start_stop, [sequence], start_stop()},
                      {missing_nodes, [sequence], missing_nodes()},
                      {normal, [sequence], dist_tests()}
                     ]},
        {decision, [sequence], decision_tests()},
        {detector, [sequence], detector_tests()}
    ].

init_per_group(system, Config) ->
    Config;

init_per_group(start_stop, Config) ->
    S1 = make_node_name(s1),
    S2 = make_node_name(s2),
    S3 = make_node_name(s3),

    P1 = make_node_http_port(1),
    P2 = make_node_http_port(2),
    P3 = make_node_http_port(3),

    Nodes = #{S1 => [{port, P1}],
              S2 => [{port, P2}],
              S3 => [{port, P3}]},

    Sf = {startup_functions, [{?MODULE, start_eclero, [Nodes]}]},

    ok = start_slave(S1, [Sf]),
    ok = start_slave(S2, [Sf]),
    ok = start_slave(S3, [Sf]),

    [{nodes, Nodes} | Config];

init_per_group(missing_nodes, Config) ->
    S1 = make_node_name(s1),
    S2 = make_node_name(s2),
    S3 = make_node_name(s3),

    P1 = make_node_http_port(1),
    P2 = make_node_http_port(2),
    P3 = make_node_http_port(3),

    NodesCfg = #{S1 => [{port, P1}],
              S2 => [{port, P2}],
              S3 => [{port, P3}]},

    Sf = {startup_functions, [{?MODULE, start_eclero, [NodesCfg]}]},

    ok = start_slave(S1, [Sf]),
    ok = start_slave(S2, [Sf]),
    ok = start_slave(S3, [Sf]),

    Nodes = maps:keys(NodesCfg),

    NodeToStop = lists:nth(rand:uniform(length(Nodes)), Nodes),

    % The default aten failure detector timeout is 500 ms
    % wait 600 ms after stopping node
    ct:sleep(600),

    {ok, _} = ct_slave:stop(NodeToStop),

    NewNodesCfg = maps:remove(NodeToStop, NodesCfg),

    [{nodes, NewNodesCfg} | Config];

init_per_group(normal, Config) ->
    S1 = make_node_name(s1),
    S2 = make_node_name(s2),
    S3 = make_node_name(s3),

    P1 = make_node_http_port(1),
    P2 = make_node_http_port(2),
    P3 = make_node_http_port(3),

    Nodes = #{S1 => [{port, P1}],
              S2 => [{port, P2}],
              S3 => [{port, P3}]},

    Sf = {startup_functions, [{?MODULE, start_eclero, [Nodes]}]},

    ok = start_slave(S1, [Sf]),
    ok = start_slave(S2, [Sf]),
    ok = start_slave(S3, [Sf]),

    [{nodes, Nodes} | Config];

init_per_group(decision, Config) ->
    [{server, decision} | Config];

init_per_group(detector, Config) ->
    Nodes = [n1, n2, n3],

    ok = application:load(eclero),
    ok = application:set_env(eclero, nodes, Nodes),
    ok = application:set_env(eclero, detector_module, ?MODULE),

    [{server, detector}, {nodes, Nodes} | Config].


end_per_group(start_stop, Config) ->
    Nodes = ?config(nodes, Config),
    CleroNodes = maps:keys(Nodes),

    [] = [{N, E} || N <- CleroNodes, {error, _, _} = E <- [ct_slave:stop(N)]],

    ok;

end_per_group(missing_nodes, Config) ->
    Nodes = ?config(nodes, Config),
    CleroNodes = maps:keys(Nodes),

    [] = [{N, E} || N <- CleroNodes, {error, _, _} = E <- [ct_slave:stop(N)]],

    ok;

end_per_group(normal, Config) ->
    Nodes = ?config(nodes, Config),
    CleroNodes = maps:keys(Nodes),

    [] = [{N, E} || N <- CleroNodes, {error, _, _} = E <- [ct_slave:stop(N)]],

    ok;

end_per_group(detector, _Config) ->
    ok = application:unload(eclero),
    ok;

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Server = ?config(server, Config),

    case Server of
        decision ->
            {ok, Pid} = eclero_decision_server:start_link(),
            [{pid_decision, Pid} | Config];
        detector ->
            {ok, PidDetector} = eclero_detector_server:start_link(),
            {ok, PidDecision} = eclero_decision_server:start_link(),
            [{pid_detector, PidDetector},
             {pid_decision, PidDecision} | Config];
        _ ->
            Config
    end.


end_per_testcase(_TestCase, Config) ->
    Server = ?config(server, Config),

    case Server of
        decision ->
            Pid = ?config(pid_decision, Config),
            ok = gen_server:stop(Pid);
        detector ->
            PidDetector = ?config(pid_detector, Config),
            PidDecision = ?config(pid_decision, Config),
            ok = gen_server:stop(PidDetector),
            ok = gen_server:stop(PidDecision);
        _ ->
            ok
    end.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_and_stop() ->
    [{doc, "Start and stop eclero in all nodes"}].

start_and_stop(Config) ->
    NodesCfg = ?config(nodes, Config),
    Nodes = maps:keys(NodesCfg),

    [] = [RC || N <- Nodes, RC <- [ct_rpc:call(N, application, stop, [eclero])], RC /= ok],

    ok.

ping_and_check() ->
    [{doc, "Ping and check if all nodes are up and running"}].

ping_and_check(Config) ->
    NodesCfg = ?config(nodes, Config),
    Nodes = maps:keys(NodesCfg),

    [] = [RC || N <- Nodes, RC <- [ping(N, NodesCfg)], RC /= ok],

    Fun = fun () -> [RC || N <- Nodes, RC <- [check(N, NodesCfg)], RC /= ok] end,

    true = wait_expect(Fun, [], 3),

    ok.

missing_one_node() ->
    [{doc, "Check returns 'service unavailable' when one node is missing"}].

missing_one_node(Config) ->
    NodesCfg = ?config(nodes, Config),
    Nodes = maps:keys(NodesCfg),

    [] = [RC || N <- Nodes, RC <- [ping(N, NodesCfg)], RC /= ok],

    ct:sleep(1000),

    Fun = fun () -> [RC || N <- Nodes, RC <- [check_service_unavailable(N, NodesCfg)], RC /= ok] end,

    true = wait_expect(Fun, [], 3),

    ok.

decision() ->
    [{doc, "Changing node status changes the health decision"}].

decision(_Config) ->

    false = eclero_decision_server:is_health(),

    ok = eclero_decision_server:node_status(n1, up),

    true = eclero_decision_server:is_health(),

    ok = eclero_decision_server:node_status(n2, up),
    ok = eclero_decision_server:node_status(n3, up),

    true = eclero_decision_server:is_health(),

    {ok, [n1, n2, n3]} = eclero_decision_server:nodes(),

    ok = eclero_decision_server:node_status(n1, down),
    ok = eclero_decision_server:node_status(n2, down),
    ok =  eclero_decision_server:node_status(n3, down),

    false = eclero_decision_server:is_health(),

    {ok, []} = eclero_decision_server:nodes(),

    ok =  eclero_decision_server:node_status(n3, up),

    true = eclero_decision_server:is_health(),

    ok.

detector() ->
    [{doc, "Check if detector can propagate changes to decision"}].

detector(Config) ->
    Nodes = ?config(nodes, Config),

    PidDetector = ?config(pid_detector, Config),

    PidDetector ! {node_event, n1, up},

    ct:sleep(100),

    true = eclero_decision_server:is_health(),

    {ok, [n1]} = eclero_decision_server:nodes(),

    PidDetector ! {node_event, n2, up},

    PidDetector ! {node_event, n3, up},

    ct:sleep(100),

    true = eclero_decision_server:is_health(),

    {ok, [n1, n2, n3]} = eclero_decision_server:nodes(),

    [ PidDetector ! {node_event, Node, down} || Node <- Nodes],

    ct:sleep(100),

    {ok, []} = eclero_decision_server:nodes(),

    false = eclero_decision_server:is_health(),

    ok.


%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

start_eclero(Nodes) ->
    N = erlang:node(),
    Node = maps:get(N, Nodes),
    HttpPort = proplists:get_value(port, Node),
    CleroNodes = maps:keys(Nodes),

    ok = application:load(eclero),
    ok = application:load(aten),

    ok = application:set_env(eclero, http, HttpPort),
    ok = application:set_env(eclero, nodes, CleroNodes),
    ok = application:set_env(aten, poll_interval, 500),

    {ok, _Started} = application:ensure_all_started(eclero),

    ok.

get_current_host() ->
    N = atom_to_list(erlang:node()),
    {ok, list_to_atom(after_char($@, N))}.

get_current_host(Node) when is_atom(Node) ->
    N = atom_to_list(Node),
    {ok, after_char($@, N)}.

make_node_name(Name) ->
    {ok, Host} = get_current_host(),
    list_to_atom(lists:flatten(io_lib:format("~s@~s", [Name, Host]))).

make_node_http_port(Port) ->
    Port + 4000.

search_paths() ->
    Ld = code:lib_dir(),
    lists:filter(fun (P) -> string:prefix(P, Ld) =:= nomatch end, code:get_path()).

start_slave(N, StartOpts) ->
    ErlFlags = string:join(["-pa" | search_paths()], " "),

    DefaultOpts = [{kill_if_fail, true},
                   {monitor_master, true},
                   {erl_flags, ErlFlags}],

    Timeouts = [{T, 15} || T <- [boot_timeout, init_timeout, start_timeout]],

    Opts = lists:merge([DefaultOpts, Timeouts, StartOpts]),

    {ok, Host} = get_current_host(),

    {ok, N} = ct_slave:start(Host, N, Opts),

    ok.

after_char(_, []) -> [];
after_char(Char, [Char|Rest]) -> Rest;
after_char(Char, [_|Rest]) -> after_char(Char, Rest).

remove_node(Node, Config) ->
    Nodes = ?config(nodes, Config),
    Config0 = proplists:delete(nodes, Config),

    Nodes0 = maps:remove(Node, Nodes),

ct:pal("~p~n", [Nodes0]),

    [{nodes, Nodes0} | Config0].


% wait N seconds until check the expected result
wait_expect(_Fun, _Expect, 0) ->
    ct:fail({error, wait_for_check, false});

wait_expect(Fun, Expect, N) ->
    ct:sleep(1000),
    case Fun() of
        Expect ->
            true;
        _ ->
            wait_expect(Fun, Expect, N-1)
    end.

%% http client functions

make_url(Host, Port) ->
    io_lib:format("http://~s:~w", [Host, Port]).

make_url(Host, Port, Service) ->
    io_lib:format("http://~s:~w/~s", [Host, Port, Service]).

ping(Node, Nodes) ->
    Cfg = maps:get(Node, Nodes),
    Port = proplists:get_value(port, Cfg),
    {ok, Host} = get_current_host(Node),

    Url = make_url(Host, Port),

    Response = httpc:request(head, {Url, []}, [], []),

    ct:pal(info, ?LOW_IMPORTANCE, "~s: node ~s response ~p", [?FUNCTION_NAME, Node, Response]),

    ok.

check(Node, Nodes) ->
    check(Node, Nodes, 200).

check_service_unavailable(Node, Nodes) ->
    check(Node, Nodes, 503).

check(Node, Nodes, ExpectStatus) ->
    Cfg = maps:get(Node, Nodes),
    Port = proplists:get_value(port, Cfg),
    {ok, Host} = get_current_host(Node),

    Url = make_url(Host, Port, "check"),

    Result = httpc:request(get, {Url, []}, [], []),

    case Result of
        {ok, {{_Version, ExpectStatus, _}, _Headers, _Body}} ->
            ok;
        _Else ->
            ct:fail({badresult, {Node, Result}})
    end,

    ct:pal(info, ?LOW_IMPORTANCE, "~s: node ~s response ~p", [?FUNCTION_NAME, Node, Result]),

    ok.

node() ->
    n1.

register(_Node) ->
    {ok, up}.

unregister(_Node) ->
    ok.
