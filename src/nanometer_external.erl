%% Интерфейс для взаимодействия с внешними провайдерами данных. На данный момент частота выборки
%% совпадает с частотой запроса данных.
%%
%% Для работы необходимо предоставить функцию вида (State) -> (Value, State1). При успешном
%% завершении транзакции State1 будет использоваться при следующем вызове функции. При
%% неуспешном будет использоваться старое State.

-module(nanometer_external).
-behaviour(nanometer).

%% API
-export([create/2, acquire/1, notify/2, reset/1, release/1, list/0, count/0, exists/1]).
-import(nanometer_compat, [monotonic_ms/0]).
-include("priv.hrl").

% {Name, Fun, LastState[, TxState]}
create(Name, Options) ->
  [{initial_state, State0}, {provider, Fun}] = lists:keysort(1, Options),
  _ = ets:insert_new(?EXTERNAL_TABLE, {Name, Fun, State0}).



exists(Name) ->
  case ets:lookup(?EXTERNAL_TABLE, Name) of
    [] -> false;
    [_] -> true
  end.


list() ->
  ets:select(?EXTERNAL_TABLE, [{{'$1', '_', '_'}, [], ['$1']}, {{'$1', '_', '_', '_'}, [], ['$1']}]).


count() ->
  ets:info(?EXTERNAL_TABLE, size).


acquire(Name) ->
  {Fun, State} = case ets:lookup(?EXTERNAL_TABLE, Name) of
    [{_, Fun1, State1}] ->
      {Fun1, State1};
    [{_, Fun1, State1, _}] ->
      {Fun1, State1}
  end,
  {Value, TxState} = Fun(State),
  ets:insert(?EXTERNAL_TABLE, {Name, Fun, State, TxState}),
  Value.


notify(_Name, _Value) ->
  error(undef).


reset(Name) ->
  [{_, Fun, _, TxState}] = ets:lookup(?EXTERNAL_TABLE, Name),
  ets:insert(?EXTERNAL_TABLE, {Name, Fun, TxState}).


release(Name) ->
  [{_, Fun, State, _}] = ets:lookup(?EXTERNAL_TABLE, Name),
  ets:insert(?EXTERNAL_TABLE, {Name, Fun, State}).
