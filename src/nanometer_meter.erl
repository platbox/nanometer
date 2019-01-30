%% "Транзакционные" уровни. Уровни - они примерно как счётчики, только никогда не сбрасываются.

-module(nanometer_meter).
-behaviour(nanometer).

%% API
-export([acquire/1, notify/2, reset/1, release/1, create/2, exists/1, list/0, count/0]).
-include("priv.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(nanometer_compat, [monotonic_ms/0, schedulers/0, scheduler/0]).

create(Name, []) ->
  _Ignore = ets:insert_new(?METER_TABLE, [{{Name, I}, 0} || I <- lists:seq(1, schedulers())]).


exists(Name) ->
  case ets:lookup(?METER_TABLE, {Name, 1}) of
    [] -> false;
    [_] -> true
  end.


list() ->
  ets:select(?METER_TABLE, [{{{'$1', 1}, '_'}, [], ['$1']}]).


count() ->
  ets:info(?METER_TABLE, size) div schedulers().

%% meters don't have transactions, they never really reset

acquire(Name) ->
  Selected = ets:select(?METER_TABLE,
                        [{{{Name, '_'}, '$1'},
                          [],
                          ['$1']}]),
  Sum = lists:sum(Selected),
  [{value, Sum}].


notify(Name, Value) ->
  ets:update_counter(?METER_TABLE, {Name, scheduler()}, {2, +Value}).


reset(_Name) ->
  ok.


release(_Name) ->
  ok.
