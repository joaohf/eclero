-module(eclero_metric).

-export([options/0]).

-export([request_check/0,
         node/2]).

options() ->
    Client = 'Elixir.TelemetryMetricsRiemann.Client.Katja',
    Metrics = metrics(),

    [{metrics, Metrics},
     {prefix, eclero},
     {client, Client}].


metrics() ->
    [
        'Elixir.Telemetry.Metrics':last_value(
            [decision, server, nodes, up],
            [{description, <<"Number of nodes online">>}]),
        'Elixir.Telemetry.Metrics':last_value(
            [decision, server, nodes, down],
            [{description, <<"Number of nodes offline">>}]),
        'Elixir.Telemetry.Metrics':counter(
            [http, request, check, done],
            [{description, <<"Number of received check requests">>}])
    ].


request_check() ->
    telemetry:execute(
        [http, request, check],
        #{value => 1},
        #{status_code => 200}).


node(Up, Down) ->
    telemetry:execute(
        [decision, server, nodes],
        #{up => Up, down => Down},
        #{}).
