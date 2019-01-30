-module(nanometer_histogram_sup).
-behaviour(supervisor).

-include("priv.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?HISTOGRAM_SUPERVISOR}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  %% create a table
  {ok, {
    #{strategy => simple_one_for_one, intensity => 1, period => 5},
    [
      #{id => histogram_srv,
        start => {nanometer_histogram_srv, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker}
    ]
  }}.

