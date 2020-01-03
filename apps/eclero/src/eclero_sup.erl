%%%-------------------------------------------------------------------
%% @doc eclero top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eclero_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    MetricArgs = eclero_metric:options(),

    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    Decision = #{id => eclero_decision_server,
                 start => {eclero_decision_server, start_link, []},
                 shutdown => 5000},
    Detector = #{id => eclero_detector_server,
                 start => {eclero_detector_server, start_link, []},
                 shutdown => 5000},
    Metric = 'Elixir.TelemetryMetricsRiemann':child_spec(MetricArgs),

    ChildSpecs = [Metric, Decision, Detector],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
