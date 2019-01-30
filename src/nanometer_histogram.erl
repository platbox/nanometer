%% Гистограммы реализованы как gen_server, поскольку являют собой достаточно сложную структуру
%% данных, и совместный доступ к ним приблизительно невозможен без сериализации запросов.
%%
%% Гистограмма основана на алгоритме Гринвальда-Канна (nanometer_gk),
%% который используется для учёта входящих данных. Кроме того, каждая гистограмма имеет понятие
%% "минимального времени жизни" T: вызов reset() сбрасывает только те данные, которые поступили
%% не менее чем за T миллисекунд до соответствующего acquire().

-module(nanometer_histogram).
-behaviour(nanometer).

%% API
-export([acquire/1, notify/2, reset/1, release/1, create/2, exists/1, list/0, count/0]).

-include("priv.hrl").

-spec create(nanometer:name(), nanometer:options()) -> _.
create(Name, Options) ->
  case exists(Name) of
    false ->
      {ok, _} = supervisor:start_child(?HISTOGRAM_SUPERVISOR, [Name, Options]);
    _ ->
      ok
  end.

-spec exists(nanometer:name()) -> boolean().
exists(Name) ->
  case ets:lookup(?HISTOGRAM_TABLE, Name) of
    [] -> false;
    [{_, Pid}] -> is_process_alive(Pid)
  end.


-spec list() -> [nanometer:name()].
list() ->
  ets:select(?HISTOGRAM_TABLE, [{{'$1', '_'}, [], ['$1']}]).


-spec count() -> non_neg_integer().
count() ->
  ets:info(?HISTOGRAM_TABLE, size).


-spec acquire(nanometer:name()) -> nanometer:values().
acquire(Name) ->
  call(Name, acquire, []).


-spec notify(nanometer:name(), number()) -> _.
notify(Name, Value) ->
  cast(Name, {notify, Value}).


-spec reset(nanometer:name()) -> _.
reset(Name) ->
  cast(Name, reset).


-spec release(nanometer:name()) -> _.
release(Name) ->
  cast(Name, release).



call(Name, What, Default) ->
  Pid = ets:lookup_element(?HISTOGRAM_TABLE, Name, 2),
  try
      gen_server:call(Pid, What)
  catch
      exit:_Reason ->
        %% I mean...
        Default
  end.

cast(Name, What) ->
  Pid = ets:lookup_element(?HISTOGRAM_TABLE, Name, 2),
  gen_server:cast(Pid, What).
