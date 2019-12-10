-module(eclero_health).

-export([get/0]).

get() ->
    {ok, Nodes} = eclero_decision_server:nodes(),

    {Replies, _BadNodes} = eclero_decision_server:is_health(Nodes),

    {ok, Replies}.
