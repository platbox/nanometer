-module(nanometer_compat).

%% API
-export([monotonic_ms/0, monotonic_us/0, universal_ms/0, schedulers/0, scheduler/0,
         create_ets_table/3, romanize/1]).

-spec monotonic_ms() -> integer().
monotonic_ms() ->
  erlang:monotonic_time(milli_seconds).

-spec monotonic_us() -> integer().
monotonic_us() ->
  erlang:monotonic_time(micro_seconds).

-spec universal_ms() -> integer().
universal_ms() ->
  os:system_time(milli_seconds).

-define(MAX, 16).
schedulers() ->
  max(?MAX, erlang:system_info(schedulers)).

scheduler() ->
  Sched = erlang:system_info(scheduler_id),
  1 + (Sched - 1) rem ?MAX.

create_ets_table(TableName, TableType, Concurrency) ->
  try
    ets:new(TableName, [named_table, TableType, public] ++ [{Flag, true} || Flag <- Concurrency])
  catch
    error:badarg ->
      %% check if the table already exists
      TableType = ets:info(TableName, type)
  end,
  ok.

-include("gost2006_generate.hrl").

%% @doc
%% "Романизует" произвольные данные, заменяя символы символами из набора ASCII.
%% Транслитерирует русскую кириллицу согласно ГОСТ Р 52535.1-2006, остальные заменяет на
%% подчёркивание.
-spec romanize(iodata() | unicode:charlist()) -> iodata().
romanize(SomeThing) ->
  RegularExpression = "[^[:word:]_.]",
  try
    re:run(SomeThing, RegularExpression, [unicode, global, {capture, first, index}])
  of
    Result -> handle_rerun_result(unicode:characters_to_binary(SomeThing), Result)
  catch
    error:badarg ->
      ResultLatin = re:run(SomeThing, RegularExpression, [global, {capture, first, index}]),
      handle_rerun_result(iolist_to_binary(SomeThing), ResultLatin)
  end.

handle_rerun_result(BaseBinary, nomatch) ->
  BaseBinary;
handle_rerun_result(BaseBinary, {match, Matches}) ->
  TotalSize = byte_size(BaseBinary),
  {FinalList, LastIndex} = lists:mapfoldl(
    fun([{Offset, Length}], Offset) ->
      {romanize_letter(binary_part(BaseBinary, Offset, Length)),
       Offset + Length};

       ([{Offset, Length}], StartOffset) ->
         {[binary_part(BaseBinary, StartOffset, Offset - StartOffset),
           romanize_letter(binary_part(BaseBinary, Offset, Length))],
          Offset + Length}
    end, 0, Matches
  ),
  LastPiece = if
    TotalSize > LastIndex -> [binary_part(BaseBinary, LastIndex, TotalSize - LastIndex)];
    TotalSize == LastIndex -> []
  end,
  [FinalList | LastPiece].
