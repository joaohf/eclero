-module(fake_detector).

-behaviour(eclero_detector).

-export([node/0,
         register/1,
         unregister/1]).

node() ->
    n1.

register(_Node) ->
    {ok, up}.

unregister(_Node) ->
    ok.
