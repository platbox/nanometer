-module(nanometer_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("priv.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  nanometer_compat:create_ets_table(?COUNTER_TABLE, ordered_set, [write_concurrency]),
  nanometer_compat:create_ets_table(?GAUGE_TABLE, ordered_set, [write_concurrency]),
  nanometer_compat:create_ets_table(?METER_TABLE, ordered_set, [write_concurrency]),

  nanometer_compat:create_ets_table(?COUNTER_TX_TABLE, set, [write_concurrency]),
  nanometer_compat:create_ets_table(?GAUGE_TX_TABLE, set, [write_concurrency]),

  nanometer_compat:create_ets_table(?HISTOGRAM_TABLE, set, []),
  nanometer_compat:create_ets_table(?EXTERNAL_TABLE, set, []),

  nanometer_external:create([nanometer_stats], [{provider, fun stats_provider/1}, {initial_state, ok}]),
  nanometer_vmstats:setup(),

  ReporterOpts = application:get_env(nanometer, reporter, undefined),
  {ok, {
    #{strategy => one_for_all},
    [#{id => histogram_sup,
        start => {nanometer_histogram_sup, start_link, []},
        type => supervisor,
        restart => permanent},
     #{id => backend,
        start => {nanometer_report_graphite, start_link, [ReporterOpts]},
        type => worker,
        restart => permanent,
        shutdown => 5000}]
  }}.

%%====================================================================
%% Internal functions
%%====================================================================

stats_provider(ok) ->
  {nanometer:stats(), ok}.
