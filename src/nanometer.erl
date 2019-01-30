-module(nanometer).

-type piece() :: atom() | integer() | binary().
-type name() :: [piece()].
-type options() :: proplists:proplist().
-type values() :: [{piece(), number()}].
-type type() :: counter | meter | gauge | histogram | external.
-type error() :: {error, any()}.

-callback create(name(), options()) -> any().
-callback notify(name(), number()) -> any().
-callback acquire(name()) -> values().
-callback release(name()) -> any().
-callback reset(name()) -> any().
-callback exists(name()) -> boolean().
-callback list() -> [name()].
-callback count() -> non_neg_integer().


%% low-level API
-export([create/3, notify/3, acquire/2, release/2, reset/2, list/1, types/0, callback_module/1, stats/0]).

%% convenience API
-export([count/2, meter/2, gauge/2, histogram/2, histogram/3, time/2, time/3]).

-export_type([name/0, error/0, options/0, values/0, type/0]).

-type result() :: ok | not_started | {error, any()}.
%% don't die!
-define(SAVE_OUR_SOULS(X),
  case catch(begin X end) of
    {'EXIT', {badarg, [{ets, _, _, _} | _]}} ->
      not_started;
    {'EXIT', {Error, _}} ->
      {error, Error};
    _ ->
      ok
  end).

-spec create(type(), name(), options()) -> result().
create(Type, Name, Options) ->
  Mod = callback_module(Type),
  ?SAVE_OUR_SOULS(Mod:create(Name, Options)).

-spec notify(type(), name(), number()) -> result().
notify(Type, Name, Number) ->
  Mod = callback_module(Type),
  ?SAVE_OUR_SOULS(Mod:notify(Name, Number)).

-spec acquire(type(), name()) -> values().
acquire(Type, Name) ->
  Mod = callback_module(Type),
  Mod:acquire(Name).

-spec release(type(), name()) -> ok.
release(Type, Name) ->
  Mod = callback_module(Type),
  Mod:release(Name),
  ok.

-spec reset(type(), name()) -> ok.
reset(Type, Name) ->
  Mod = callback_module(Type),
  Mod:reset(Name),
  ok.

-spec list(type()) -> [name()].
list(Type) ->
  Mod = callback_module(Type),
  Mod:list().

-spec types() -> [type()].
types() -> [counter, meter, gauge, histogram, external].

-spec callback_module(type()) -> module().
callback_module(counter) ->
  nanometer_counter;
callback_module(meter) ->
  nanometer_meter;
callback_module(gauge) ->
  nanometer_gauge;
callback_module(histogram) ->
  nanometer_histogram;
callback_module(external) ->
  nanometer_external.

-spec stats() -> values().
stats() ->
  lists:map(
    fun(Type) ->
      Mod = callback_module(Type),
      {Type, Mod:count()}
    end, types()).


-spec count(name(), integer()) -> result().
count(Name, Delta) when is_integer(Delta) ->
  ?SAVE_OUR_SOULS(
  begin
    nanometer_counter:create(Name, []),
    nanometer_counter:notify(Name, Delta)
  end
  ).

-spec meter(name(), integer()) -> result().
meter(Name, Delta) when is_integer(Delta) ->
  ?SAVE_OUR_SOULS(
  begin
    nanometer_meter:create(Name, []),
    nanometer_meter:notify(Name, Delta)
  end
  ).

-spec gauge(name(), number()) -> result().
gauge(Name, Value) ->
  ?SAVE_OUR_SOULS(
  begin
    nanometer_gauge:create(Name, []),
    nanometer_gauge:notify(Name, Value)
  end
  ).


-spec histogram(name(), number()) -> result().
histogram(Name, Value) ->
  histogram(Name, Value, []).

-spec histogram(name(), number(), options()) -> result().
histogram(Name, Value, Options) ->
  ?SAVE_OUR_SOULS(
  begin
    nanometer_histogram:create(Name, Options),
    nanometer_histogram:notify(Name, Value)
  end
  ).

-spec time(name(), fun(() -> X)) -> X when X :: any().
time(Name, Fun) ->
  time(Name, Fun, []).

-spec time(name(), fun(() -> X), options()) -> X when X :: any().
time(Name, Fun, Options) ->
  Time0 = nanometer_compat:monotonic_us(),
  Result = Fun(),
  Time1 = nanometer_compat:monotonic_us(),
  histogram(Name, Time1 - Time0, Options),
  Result.
