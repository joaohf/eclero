-module(eclero_http).

-export([start/1, stop/0]).

-export([init/2,
         service_available/2,
         content_types_provided/2,
         to_text/2]).

start(Port) ->
    TransOpt = [{port, Port}],
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/check", ?MODULE, []}
        ]}
    ]),
    ProtoOpt = #{env => #{dispatch => Dispatch}},
    {ok, _} = cowboy:start_clear(http, TransOpt, ProtoOpt),
    ok.

stop() ->
    cowboy:stop_listener(http).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

service_available(Req, State) ->
    %Res = eclero_decision_server:is_health(),
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, to_text}
    ], Req, State}.

to_text(Req, State) ->
    Body = to_text(eclero_health:get()),

    eclero_metric:request_check(),

    {Body, Req, State}.

%% internal functions

to_text({ok, Results}) ->
    Fun = fun({Node, Value}, AccIn) ->
        L = io_lib:format("~s:~w", [Node, Value]),
        [L | AccIn]
    end,
    L0 = lists:foldl(Fun, [], Results),
    L1 = lists:join(",", L0),
    list_to_binary(L1).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_text_test() ->
    L = to_text({ok, [{a, true}, {b, false}]}),
    ?assertEqual(<<"b:false,a:true">>, L),
    ok.

-endif.
