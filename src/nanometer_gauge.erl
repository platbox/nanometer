%% "Транзакционные" шкалы. Возвращают последнее (читай: случайное) значение, которое было
%% присвоено им с момента последнего сброса.

-module(nanometer_gauge).
-behaviour(nanometer).

%% API
-export([acquire/1, notify/2, reset/1, release/1, create/2, exists/1, list/0, count/0]).
-include("priv.hrl").

-import(nanometer_compat, [monotonic_ms/0, schedulers/0, scheduler/0]).

create(Name, []) ->
  Cutoff = monotonic_ms(),
  _Ignore = ets:insert_new(?GAUGE_TX_TABLE, [{Name, Cutoff}]),
  true.


exists(Name) ->
  case ets:lookup(?GAUGE_TX_TABLE, Name) of
    [] -> false;
    [_] -> true
  end.


list() ->
  ets:select(?GAUGE_TX_TABLE, [{{'$1', '_'}, [], ['$1']}, {{'$1', '_', '_'}, [], ['$1']}]).


count() ->
  ets:info(?GAUGE_TX_TABLE, size).


%% here we just don't care. Gauges don't accumulate stuff at all, so we just select stuff between last
%% transaction start that ended in reset, and 'now'.
acquire(Name) ->
  Cutoff = monotonic_ms(),
  LastReset = ets:lookup_element(?GAUGE_TX_TABLE, Name, 2),
  %% The intervals overlap, but duping a value is much less bad than outright losing it.
  Selected = ets:select(?GAUGE_TABLE,
    [{{{Name, '_'}, {'$1', '$2'}},
       [{'<', '$1', Cutoff}, {'>=', '$1', LastReset}],
       [{{'$1', '$2'}}]}]),
  ets:insert(?GAUGE_TX_TABLE, {Name, LastReset, Cutoff}),
  case Selected of
    [] ->
      [{ms_since_reset, Cutoff - LastReset}];
    _ ->
      {_, Value} = lists:max(Selected),
      [{ms_since_reset, Cutoff - LastReset}, {value, Value}]
  end.


notify(Name, Value) ->
  ets:insert(?GAUGE_TABLE, {{Name, scheduler()}, {monotonic_ms(), Value}}).


release(Name) ->
  LastReset = ets:lookup_element(?GAUGE_TX_TABLE, Name, 2),
  ets:insert(?GAUGE_TX_TABLE, {Name, LastReset}).


reset(Name) ->
  NewReset = ets:lookup_element(?GAUGE_TX_TABLE, Name, 3),
  ets:insert(?GAUGE_TX_TABLE, {Name, NewReset}).
