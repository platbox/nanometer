-module(nanometer_vmstats).

%% API
-export([setup/0]).
-import(erlang, [system_info/1]).
-import(nanometer_compat, [monotonic_ms/0]).

-record(state, {
  reductions = 0,
  io = {0, 0},
  gc = {0, 0},
  sched = sched_stats(),
  ms = monotonic_ms()
}).

stats(#state{
  reductions = RedOld,
  io         = {BytesInOld, BytesOutOld},
  gc         = {PrevCalls, PrevWords},
  sched      = SchedList,
  ms         = PrevMs}) ->
  Time = monotonic_ms(),
  Rate = Time - PrevMs,
  TotalMessages = lists:foldl(
    fun(Pid, Acc) ->
      case process_info(Pid, message_queue_len) of
        undefined -> Acc;
        {message_queue_len, Count} -> Count + Acc
      end
    end,
    0,
    processes()
  ),
  ErrLogLen = case whereis(error_logger) of
    undefined ->
      [];
    ErrLoggerPid ->
      case process_info(ErrLoggerPid, message_queue_len) of
        undefined ->
          [];
        {message_queue_len, Count} ->
          {error_logger_queue_len, Count}
      end
  end,
  {Reductions, _} = statistics(reductions),
  {{input, BytesIn}, {output, BytesOut}} = statistics(io),
  {Collections, Words, _} = statistics(garbage_collection),

  {SchedList1, SchedStats} = collect_scheduler(SchedList, Rate),

  Result = [
    {ms_since_reset, Rate},
    {proc_count, system_info(process_count)},
    {proc_limit, system_info(process_limit)},
    {messages_in_queues, TotalMessages},
    {run_queue, statistics(run_queue)},
    {modules, length(code:all_loaded())},
    rate(reductions, Reductions - RedOld, Rate),
    rate('io.bytes_in', BytesIn - BytesInOld, Rate),
    rate('io.bytes_out', BytesOut - BytesOutOld, Rate),
    rate('gc.collections', Collections - PrevCalls, Rate),
    rate('gc.words_reclaimed', Words - PrevWords, Rate),
    SchedStats,
    ErrLogLen
    ] ++ [{concat(memory, X), erlang:memory(X)} || X <- [total, processes_used, atom_used, binary, ets]],

  {lists:flatten(Result),
    #state{reductions = Reductions, io = {BytesIn, BytesOut}, gc = {Collections, Words}, sched = SchedList1, ms = Time}}.

rate(Name, Value, Rate) when Rate > 100 ->
  [{concat(Name, count), Value}, {concat(Name, rate), 1000 * Value / Rate}];
rate(Name, Value, _) ->
  {concat(Name, count), Value}.

concat(A, B) ->
  list_to_binary(io_lib:format("~s.~s", [A, B])).

collect_scheduler(SchedList, _Rate) ->
  SchedList1 = sched_stats(),
  Stats = lists:zipwith(
    fun({I, ActiveLast, TotalLast}, {I, Active, Total}) when TotalLast /= Total ->
      Tag = io_lib:format("scheduler_wall_time.~B", [I]),
      [{concat(Tag, 'active.count'), Active - ActiveLast},
       {concat(Tag, 'total.count'), Total - TotalLast}];
       (_, _) -> [] end,
    SchedList, SchedList1),
  {SchedList1, Stats}.

sched_stats() ->
  lists:keysort(1, erlang:statistics(scheduler_wall_time)).

setup() ->
  erlang:system_flag(scheduler_wall_time, true),
  nanometer_external:create([vmstats], [{provider, fun stats/1}, {initial_state, #state{}}]).
