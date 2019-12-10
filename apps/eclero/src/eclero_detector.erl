-module(eclero_detector).

-export([node/0, register/1, unregister/1]).

-export([node/1, register/2, unregister/2]).

-callback node() -> atom().
-callback register(atom()) -> {ok, up | undefiend}.
-callback unregister(atom()) -> ok.

node(Module) ->
    Module:node().

register(Module, Node) ->
    Module:register(Node).

unregister(Module, Node) ->
    Module:unregister(Node).

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
