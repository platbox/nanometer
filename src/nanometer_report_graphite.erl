-module(nanometer_report_graphite).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(nanometer_compat, [romanize/1, monotonic_ms/0, universal_ms/0]).
-import(erlang, [send_after/3, send_after/4, cancel_timer/1]).

-define(SERVER, ?MODULE).
-define(IP_HEADER_SIZE, 32).
-define(TCP_HEADER_SIZE, 192).

-define(RETRIES, 5).

-type name() :: {module() | stats, nanometer:name()}.
-record(state, {
  sock = undefined :: undefined | gen_tcp:socket(),
  address :: {inet:hostname(), inet:port_number()},
  tcp_opts = [] :: [gen_tcp:option()],
  period = 5000 :: timeout(),
  connect_timeout = infinity :: timeout(),
  packet_size = 1432 :: pos_integer(),
  prefix = [] :: iodata(),
  timer :: reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(undefined) ->
  ignore;
start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

init(Params) ->
  {address, Address} = lists:keyfind(address, 1, Params),
  State0 = #state{timer = send_after(0, self(), report), address = Address},
  State = lists:foldl(
    fun({period, Period}, State1) -> State1#state{period = Period};
       ({address, _AlreadyHandled}, State1) -> State1;
       ({tcp_options, Opts}, State1) -> State1#state{tcp_opts = Opts};
       ({packet_size, Size}, State1) -> State1#state{packet_size = Size};
       ({connect_timeout, Timeout}, State1) -> State1#state{connect_timeout = Timeout};
       ({prefix, Prefix}, State1) -> State1#state{prefix = [Prefix, $.]}
    end, State0, Params),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(report, State = #state{timer = Ref, period = Period}) ->
  cancel_timer(Ref),
  StartTime = monotonic_ms(),
  SystemTime = universal_ms(),
  State1 = maybe_open_socket(State),
  Result = case State1#state.sock of
    undefined ->
      State1;
    Sock ->
      PacksToSend = collect_packs(State1, SystemTime),
      case
        nanometer:time([nanometer_stats, reporter_send_time], fun() ->
          send_packs_retry(Sock, PacksToSend, ?RETRIES) end, [{min_lifetime, Period * 10}]) of
        ok ->
          State1;
        emsgsize ->
          State1#state{packet_size = State1#state.packet_size div 2};
        closed ->
          State1#state{sock = undefined};
        timeout ->
            catch gen_tcp:close(Sock),
          State1#state{sock = undefined}
      end
  end,
  %% handle graphite quantization
  SendNextAt = next_interval(StartTime, monotonic_ms(), SystemTime - StartTime, Period),
  {noreply, Result#state{timer = send_after(SendNextAt, self(), report, [{abs, true}])}};
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

render(Key, Name, Values, TimestampS) ->
  NamePrefix = render_prefix(Name),
  [[Key, romanize(NamePrefix), stringify(Metric), $\s, stringify(Value), $\s, TimestampS, $\n] ||
    {Metric, Value} <- Values].

render_batch(Key, Metrics, TimestampS) ->
  [{Type, Name, render(Key, Name, Values, TimestampS)} || {Type, Name, Values} <- Metrics].

pack(Rendered, PacketSize) ->
  pack(Rendered, [], [], [], 0, PacketSize - ?IP_HEADER_SIZE - ?TCP_HEADER_SIZE).

pack([], Packs, Names, Acc, _Size, _MaxSize) ->
  [{Names, Acc} | Packs];
pack([{Mod, Name, Metrics} | Rest], Packs, Names, Acc, Size, MaxSize) ->
  MetricsSize = iolist_size(Metrics),
  if
    MetricsSize + Size =< MaxSize ->
      pack(Rest, Packs, [{Mod, Name} | Names], [Metrics | Acc], MetricsSize + Size, MaxSize);
    true ->
      pack(Rest, [{Names, Acc} | Packs], [{Mod, Name}], [Metrics], MetricsSize, MaxSize)
  end.

-spec send_packs(gen_tcp:socket(), Packs :: [{[name()], iodata()}]) ->
  {ok | Error, Accepted, Rejected} when Accepted :: [name()], Rejected :: [{[name()], iodata()}],
  Error :: closed | emsgsize | timeout.
send_packs(Sock, Packs) ->
  send_packs(Sock, Packs, [], []).

send_packs(_Sock, [], Accepted, Rejected) ->
  {ok, Accepted, Rejected};
send_packs(Sock, [NamesPack = {Names, Pack} | Rest], Accepted, Rejected) ->
  case gen_tcp:send(Sock, Pack) of
    ok ->
      send_packs(Sock, Rest, Names ++ Accepted, Rejected);
    {error, Error} when Error =:= closed; Error =:= emsgsize; Error =:= timeout ->
      error_logger:warning_report(["unrecoverable send error", {error, Error}]),
      {Error, Accepted, [NamesPack | Rejected]};
    {error, _Err} ->
      error_logger:info_report(["recoverable send error", {error, _Err}]),
      send_packs(Sock, Rest, Accepted, [NamesPack | Rejected])
  end.

send_packs_retry(_Sock, [], _Retry) ->
  ok;
send_packs_retry(_Sock, Packs, 0) ->
  lists:foreach(
    fun({Names, _Pack}) -> release_many(Names) end, Packs
  ),
  ok;
send_packs_retry(Sock, Packs, Retry) ->
  {Tag, Accepted, Rejected} = send_packs(Sock, Packs),
  reset_many(Accepted),
  case Tag of
    ok -> send_packs_retry(Sock, Rejected, Retry - 1);
    Error ->
      lists:foreach(
        fun({Names, _Pack}) -> release_many(Names) end, Packs
      ),
      Error
  end.


render_prefix(Name) ->
  lists:foldr(fun(Item, Acc) -> [stringify(Item), $. | Acc] end, [], Name).

stringify(Piece) when is_atom(Piece) ->
  atom_to_binary(Piece, unicode);
stringify(Piece) when is_integer(Piece) ->
  integer_to_binary(Piece);
stringify(Piece) when is_float(Piece) ->
  float_to_binary(Piece, [{decimals, 17}]);
stringify(Piece) ->
  Piece.

maybe_open_socket(State = #state{
  sock            = undefined,
  tcp_opts        = TcpOptions,
  address         = {Host, Port},
  packet_size     = PacketSize,
  connect_timeout = Timeout}) ->
  case gen_tcp:connect(Host, Port, [{nodelay, true}, {delay_send, false}, {active, false}] ++ TcpOptions, Timeout) of
    {ok, Sock} ->
      gen_tcp:shutdown(Sock, read),
      {ok, List} = inet:getopts(Sock, [buffer, sndbuf]),
      #{buffer := BufSize, sndbuf := SndBufSize} = maps:from_list(List),
      inet:setopts(Sock, [{buffer, max(PacketSize, BufSize)}, {sndbuf, max(PacketSize, SndBufSize)}]),
      State#state{sock = Sock};
    {error, _Err} ->
      error_logger:warning_report(["failed to open the socket", {error, _Err}]),
      State#state{sock = undefined}
  end;
maybe_open_socket(State) ->
  State.

acquire_many([], Acc) ->
  Acc;
acquire_many([{Module, Name} | Rest], Acc) ->
  acquire_many(Rest, [{Module, Name, Module:acquire(Name)} | Acc]).

release_many([]) ->
  ok;
release_many([{Module, Name} | Rest]) ->
  Module:release(Name),
  release_many(Rest).

reset_many([]) ->
  ok;
reset_many([{Module, Name} | Rest]) ->
  Module:reset(Name),
  reset_many(Rest).

collect_packs(#state{packet_size = PacketSize, prefix = Prefix}, Timestamp) ->
  Modules = [nanometer:callback_module(Type) || Type <- nanometer:types()],
  MNames = lists:flatmap(
    fun(Module) -> [{Module, Name} || Name <- Module:list()] end, Modules),
  Values = acquire_many(MNames, []),
  TimestampS = integer_to_binary(Timestamp div 1000),
  Rendered = render_batch(Prefix, Values, TimestampS),
  pack(Rendered, PacketSize).

next_interval(StartTime, CurrentTime, Offset, Period) ->
  NextTime = (StartTime - (StartTime + Offset) rem Period) + Period,
  if
    NextTime >= CurrentTime -> NextTime;
    true -> (CurrentTime - (CurrentTime + Offset) rem Period) + Period
  end.
