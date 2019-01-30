%% Транзакционные счётчики. Сочетание acquire/release эквивалентно отсутствию операции,
%% сочетание acquire/reset - сбросу на ноль в момент выполнения acquire.
%%
%% Счётчики реализованы как отдельная строка в таблице для каждого планировщика плюс "строка
%% транзакции", которая хранит метаинформацию о времени начала транзакции и величине счётчика на
%% тот момент. Успешное завершение (reset) модифицирует счётчик на -X, где X - значение, которое
%% вернуло acquire.

-module(nanometer_counter).
-behaviour(nanometer).

%% API
-export([acquire/1, notify/2, reset/1, release/1, create/2, exists/1, list/0, count/0]).
-include("priv.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(nanometer_compat, [monotonic_ms/0, schedulers/0, scheduler/0]).

create(Name, []) ->
  _Ignore1 = ets:insert_new(?COUNTER_TABLE, [{{Name, I}, 0} || I <- lists:seq(1, schedulers())]),
  _Ignore2 = ets:insert_new(?COUNTER_TX_TABLE, {Name, monotonic_ms()}).


exists(Name) ->
  case ets:lookup(?COUNTER_TX_TABLE, Name) of
    [] -> false;
    [_] -> true
  end.


list() ->
  ets:select(?COUNTER_TABLE, [{{{'$1', 1}, '_'}, [], ['$1']}]).


count() ->
  ets:info(?COUNTER_TX_TABLE, size).

%% total amount is always "supposed to be" sum(counters[Name]) - transaction[Name]
%% resetting always sets transaction to zero.
%% acquiring a new transaction automatically releases (doesn't reset!) the previous transaction
%% so if we have {Sum, TxDelta}, after acquire-release we should have Sum + whatever else comes,
%% and after reset just "whatever else comes", thus it is necessary that TxDelta1 = Sum


acquire(Name) ->
  Now = monotonic_ms(),
  Selected = ets:select(?COUNTER_TABLE,
                        [{{{Name, '_'}, '$1'},
                          [],
                          ['$1']}]),
  Sum = lists:sum(Selected),
  LastReset = ets:lookup_element(?COUNTER_TX_TABLE, Name, 2),
  Delta = Now - LastReset,
  ets:insert(?COUNTER_TX_TABLE, {Name, LastReset, Now, Sum}),
  Rate = if
    Delta > 100 ->
      [{rate, 1000 * Sum / Delta}];
    true ->
      []
  end,
  [{ms_since_reset, Delta}, {count, Sum}, {one, Sum} | Rate].


notify(Name, Value) ->
  ets:update_counter(?COUNTER_TABLE, {Name, scheduler()}, {2, + Value}).


reset(Name) ->
  [{_, _, TxStart, TxDelta}] = ets:lookup(?COUNTER_TX_TABLE, Name),
  ets:update_counter(?COUNTER_TABLE, {Name, scheduler()}, {2, -TxDelta}),
  ets:insert(?COUNTER_TX_TABLE, {Name, TxStart}).


release(Name) ->
  LastReset = ets:lookup_element(?COUNTER_TX_TABLE, Name, 2),
  ets:insert(?COUNTER_TX_TABLE, {Name, LastReset}).

