-module(eclero_detector).

-export([node/0, register/1, unregister/1]).

node() ->
    erlang:node().

register(Node) ->
    case erlang:node() of
        Node ->
            {ok, up};
        _ ->
            ok = aten:register(Node),
            {ok, undefiend}
    end.

unregister(Node) ->
    aten:unregister(Node).
