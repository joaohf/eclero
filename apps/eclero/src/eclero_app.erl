%%%-------------------------------------------------------------------
%% @doc eclero public API
%% @end
%%%-------------------------------------------------------------------

-module(eclero_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    Port = application:get_env(eclero, http, 8000),

    ok = eclero_http:start(Port),
    eclero_sup:start_link().

stop(_State) ->
    ok = eclero_http:stop(),
    ok.

%% internal functions
