-module(nanometer_histogram_srv).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(nanometer_compat, [monotonic_ms/0]).

-include("priv.hrl").

-type histogram() :: nanometer_gk:gk().
-type ms() :: integer().
-type entry() :: {ms() | infinity, histogram()}.

%% @type state().
%% Состояньице. History содержит, собственно, все гистограммы с момента какого-то acquire,
%% не считая данных с момента последнего acquire (которые находятся в transaction). Empty -
%% просто образец пустого дерева. Percentiles - перцентили, которые нас интересуют. Min_Lifetime
%% - минимальное время жизни в миллисекундах. Debug контролирует, отправлять ли внутреннюю
%% статистику гистограммы в качестве метрик.
-record(state, {
  history = [] :: [entry()],
  transaction = undefined :: undefined | entry(),
  empty :: histogram(),
  percentiles = [50, 75, 99] :: [0..100 | min | max | median, ...],
  min_lifetime = 0 :: infinity | non_neg_integer(),
  debug = false :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Opts) ->
  case gen_server:start_link(?MODULE, Opts, []) of
    {ok, Pid} ->
      ets:insert(?HISTOGRAM_TABLE, {Name, Pid}),
      {ok, Pid};
    Err ->
      Err
  end.


-define(DEFAULT_HISTOGRAM_OPTIONS,
  application:get_env(nanometer, histogram_options, [])).

init(Opts0) ->
  Opts = ?DEFAULT_HISTOGRAM_OPTIONS ++ Opts0,
  Acc = proplists:get_value(accuracy, Opts, 1),
  Epsilon = Acc / 100,
  Empty = nanometer_gk:new(Epsilon),
  State = lists:foldl(
    fun({accuracy, _Handled}, State1) when is_number(Acc) -> State1;
       ({percentiles, P}, State1) when is_list(P) -> State1#state{percentiles = P};
       ({min_lifetime, L}, State1) when is_integer(L), L >= 0; L == infinity -> State1#state{min_lifetime = L};
       ({debug, Dbg}, State1) when Dbg == true; Dbg == false -> State1#state{debug = Dbg}
    end, #state{empty = Empty}, Opts),
  {ok, State#state{transaction = {monotonic_ms(), Empty}}}.

-spec handle_call(acquire, _, #state{}) -> {reply, nanometer:values(), #state{}}.
handle_call(acquire, _From, State = #state{empty = Empty, min_lifetime = MinL, history = History, transaction = Tx, percentiles = Perc}) ->
  Now = monotonic_ms(),
  [{LastReset, Top} | _] = History1 = push_tx(History, Tx, MinL),
  Values0 = [{P, nanometer_gk:get_quantile(translate_percentile(P), Top)} || P <- Perc],
  Values = [{P, V} || {P, V} <- Values0, V /= undefined],
  Stats = case State#state.debug of
    true -> nanometer_gk:get_stats(Top);
    false -> []
  end,
  {reply, [{ms_since_reset, Now - LastReset} | Stats ++ Values],
   State#state{transaction = {Now, Empty}, history = History1}}.

translate_percentile(min) -> 0;
translate_percentile(max) -> 1;
translate_percentile(median) -> 0.5;
translate_percentile(P) ->
  P / 100.


-spec handle_cast({notify, number()} | reset | release, #state{}) -> {noreply, #state{}}.
handle_cast({notify, Value}, State = #state{transaction = {Time, Histo}}) ->
  Histo1 = nanometer_gk:insert(Value, Histo),
  {noreply, State#state{transaction = {Time, Histo1}}};

handle_cast(reset, State = #state{transaction = {StartTime, _}, history = History, min_lifetime = MinL}) ->
  History1 = filter_expired(History, StartTime, MinL),
  %% we don't have to push anything, we are happy as is (in "released state" now)
  {noreply, State#state{history = History1}};
handle_cast(release, State) ->
  %% we are happy here.
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


push_tx([{Time, OldTx}], {_, NewTx}, infinity) ->
  [{Time, nanometer_gk:merge_left(OldTx, NewTx)}];
push_tx(History, Tx, _) ->
  push_tx(History, Tx).

push_tx([], {Time, NewTx}) ->
  [{Time, NewTx}];
push_tx([{Time, OldTx}], {Time, NewTx}) ->
  [{Time, nanometer_gk:merge_left(OldTx, NewTx)}];
push_tx([{Time1, OldTx} | Rest], {Time, NewTx}) ->
  [{Time1, nanometer_gk:merge_left(OldTx, NewTx)} | push_tx(Rest, {Time, NewTx})].


filter_expired(History, _Time, infinity) ->
  History;
filter_expired([{Start, _Tx} | Rest], Time, Duration) when Time - Start >= Duration ->
  filter_expired(Rest, Time, Duration);
filter_expired(NonExpired, _Time, _Duration) ->
  NonExpired.
