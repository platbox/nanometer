%% Реализует приближённый метод Гринвальда-Канна.
%%
%% Суть метода заключается в том, что мы храним не все элементы, а лишь несколько, поддерживая
%% инвариант, что ранги всех элементов известны с точностью ± εN, где ε - заданная заранее
%% константа, а N - количество виденных измерений.
%%
%% Для каждого элемента, означающего измерение, мы поддерживаем три значения - собственно
%% значение сохранённого измерения, g_i - разница между его минимальным рангом и минимальным
%% рангом предыдущего элемента, и ∆_i - разница между его минимальным и максимальным рангом.
%% Таким образом, минимальный ранг элемента может быть вычислен как r-min(i) = ∑_{j≤i} g_i,
%% а максимальный - как r-max(j) = r-min(i) + ∆_i.
%%
%% Заметим, что если r-min(i) ≤ r-min(j) ≤ r-max(j) ≤ r-min(i) + 2 εN, то j может
%% использоваться как приближение для i с точностью ± εN. В связи с этим вводится понятие
%% ёмкости: ёмкость элемента равна cap(i) = 2 εN - (g_i + ∆_i). Несложно видеть, что если
%% ёмкость элемента больше нуля, то он может приблизить элемент i-1.
%%
%% Покажем, что если ёмкость всех элементов ≥ 0, то для заданного ранга X мы можем найти
%% элемент с рангом X ± εN. Найдём элемент i такой, что r-max(i) ≤ X + εN, но r-max(i+1) > X + εN.
%% Если такой элемент удалось найти, то r-min(i) = r-max(i+1) - (∆_i + g_i) =
%% r-max(i+1) + cap(i) - 2 εN > X - εN + cap(i) > X - εN. Таким образом, value_i имеет ранг
%% X ± εN. Если такой элемент не удалось найти, то максимальный ранг любого элемента не более X
%% + εN, в частности, r-max(Last) ≤ X + εN, r-min(Last) = r-max(Last) ({@link insert/2}),
%% и value_Last является приближением.
%%
%% Для поддержания компактности тройки организуются в декартово дерево с ключом порядка, равным
%% value_i, и неявным ключом высоты, равным band(2 εN, ∆_i). Для определения band см. исходную
%% статью или {@link band_number/2}. band тем больше, чем меньше ∆_i, и сохраняет порядок
%% при увеличении N. Всего высот около log_2(2 εN).
%%
%% (В оригинальной статье вместо декартова дерево используется дерево, заданное свойством
%% "родитель - первый элемент справа, который выше", однако оно изоморфно декартову при помощи
%% стандартного изоморфизма "левый ребёнок, правый собрат").
%%
%% При сжатии, которое случается, когда увеличивается 2 εN или размер дерева превышает прежний
%% максимум, мы пытаемся уничтожить левое поддерево (оставляя отца) или слить правое поддерево с
%% отцом, если левое поддерево правого поддерева уже пусто. При этом мы следим за тем, чтобы
%% новая наполненность вершин оставалась строго больше нуля. (От автора: в принципе, уничтожать
%% правое поддерево/левого ребёнка было бы ничем не хуже, просто алгоритм предпочитает завышать,
%% а не занижать).
%%
%% В то время как корректность выдаваемых ответов является легко видимой характеристикой структуры,
%% ограничения её размера - вещь нетривиальная, и интересующихся я отсылаю к оригинальной статье:
%% [http://infolab.stanford.edu/~datar/courses/cs361a/papers/quantiles.pdf].

-module(nanometer_gk).
-include_lib("eunit/include/eunit.hrl").

%% The weight-key for finger tree is g_i of the original
%% algorithm.

-record(element, {
  min_rank = 1 :: pos_integer(),
  delta = 0 :: non_neg_integer(),
  value :: number()
}).

-record(measure, {
  total_min_rank = 0 :: non_neg_integer(),
  size = 0 :: non_neg_integer(),
  max_delta = 0 :: non_neg_integer(),
  right :: undefined | #element{}
}).

-define(NULL_MEASURE, #measure{}).

-record(gk, {
  tuples :: ftree(),
  % size is stored in the tree,
  epsilon :: number(), %% desired accuracy of percentiles
  last_maxerr = 0 :: non_neg_integer(), % last floor(2 epsilon n) for which the compression occured
  compressions = 0 :: non_neg_integer(), % number of compressions occurred
  capacity :: non_neg_integer() % historically largest size
}).

%% API
-export([new/1, insert/2, get_quantile/2, get_stats/1, merge_left/2]).

-opaque gk() :: #gk{}.
-export_type([gk/0]).


%% Создаёт новое дерево и инициализирует его. Начальная ёмкость установлена в trunc(1/2ε), потому
%% что сжатия для меньшего размера дерева произойти не может.
%% Каждый раз после полного сжатия ёмкость увеличивается до размера дерева + 1; каждый раз, когда
%% при вставке размер дерева превысил бы старую ёмкость, производится полное сжатие.]
%%
%% В качестве основы для алгоритма используется
%% [http://www.staff.city.ac.uk/~ross/papers/FingerTree.html]. Это очень мощная структура данных,
%% позволяющая легко поддерживать сразу множество моноидальных метрик.

-spec new(Eps :: number()) -> gk().
new(Epsilon) ->
  #gk{tuples  = new_tree(),
      epsilon = Epsilon,
      capacity = trunc(1 / (2 * Epsilon))}.


%% Вставляет элемент в дерево. Если элемент попадает в начало или конец, его погрешность ранга
%% присваивается равной нулю (инвариант: при сжатии минимум и максимум всегда остаются в дереве).
%% В противном случае r-min(New) ≥ r-min(i) + 1, r-max(New) ≤ r-max(i + 1), откуда ∆_New ≤ 2 εN - 1.

-spec insert(number(), gk()) -> gk().
insert(Value, Gk) when is_number(Value) ->
  Gk1 = #gk{tuples = Tuples} = compress(Gk),
  {Left, Right} = split_tree_left(
    fun(#measure{right = #element{value = Value1}}) when Value1 > Value -> true;
       (_) -> false end,
    Tuples),
  NewTuples = case
    is_empty(Left) orelse is_empty(Right)
  of
    true ->
      join_trees(
           Left, {
          #element{value = Value}
        }, Right);
    _ ->
      join_trees(
           Left, {
          #element{value = Value, delta = get_maxerr(Gk1) - 1}
        }, Right)
  end,
  ?assertEqual(get_size(Left) + 1 + get_size(Right), get_size(NewTuples)),
  ?assertEqual(get_size(Tuples) + 1, get_size(NewTuples)),
  bump_tuples(Gk1, NewTuples).


%% Получает элемент с нужной квантилью (от 0 - минимум до 1 - максимум), точнее, с переданной
%% квантилью плюс-минус эпсилон. См. документацию модуля для описания алгоритма.

-spec get_quantile(number(), gk()) -> number() | undefined.
get_quantile(Quantile, Gk = #gk{tuples = Tuples}) ->
  case get_count(Tuples) of
    0 ->
      undefined;
    1 ->
      #element{value = V} = peek_left(Tuples),
      V;
    Count ->
      MaxErrors = get_maxerr(Gk),
      Rank = round(1 + (Count - 1) * Quantile),
      RankMax = Rank + MaxErrors / 2,
      {Lesser, _Greater} = split_tree_left(
        fun(#measure{total_min_rank = MinRank,
                     right = #element{delta = Delta}}) when MinRank + Delta > RankMax -> true;
           (_) -> false end, Tuples),
      %% the answer may not be unique here, but we are guaranteed that a) Lesser contains an item,
      %% b) Lesser's rightmost item's rank + delta =< MaxRank, and, by construction of MaxRank,
      %% c) Lesser's rank >= Rank - MaxErrors / 2
      #element{value = V} = peek_right(Lesser),
      V
  end.


%% Получает статистику самого дерева: размер, ёмкость, количество представленных элементов и
%% количество произошедших сжатий.

-spec get_stats(gk()) -> nanometer:values().
get_stats(#gk{tuples = Tuples, capacity = Cap, compressions = Cpr}) ->
  [{size, get_size(Tuples)},
   {capacity, Cap},
   {count, get_count(Tuples)},
   {compressions, Cpr}].


%% Сливает два дерева в одно. Использует более или менее стандартный merge-sort.

-spec merge_left(Dest :: gk(), Src :: gk()) -> gk().
merge_left(Gk1 = #gk{tuples = Tuples1, epsilon = Epsilon1, capacity = Cap1, last_maxerr = Last1},
           _Gk2 = #gk{tuples = Tuples2, epsilon = Epsilon2, capacity = Cap2, last_maxerr = Last2}) ->
  TotalCount = get_count(Tuples1) + get_count(Tuples2),
  Epsilon = max(Epsilon1, Epsilon2),
  Delta = get_maxerr(TotalCount, Epsilon),
  MergedTuples = merge_tuples(new_tree(), Tuples1, Tuples2, Delta),
  compress(Gk1#gk{epsilon = Epsilon, tuples = MergedTuples, capacity = max(Cap1, Cap2), last_maxerr = min(Last1, Last2)}).

%% fallback for one empty
merge_tuples({}, Left, {}, _) ->
  Left;
merge_tuples({}, {}, Right, _) ->
  Right;
%% last element has rank-delta of 0
merge_tuples(Acc, {}, {E}, _) ->
  merge_tuples_step(Acc, E, 0);
merge_tuples(Acc, {E}, {}, _) ->
  merge_tuples_step(Acc, E, 0);
%% consume all!
merge_tuples(Acc, Left, {}, Delta) ->
  merge_tuples_batch(Acc, Left, Delta);
merge_tuples(Acc, {}, Right, Delta) ->
  merge_tuples_batch(Acc, Right, Delta);
merge_tuples(Acc, Left, Right, Delta) ->
  #element{value = VL} = peek_left(Left),
  #element{value = VR} = peek_left(Right),
  if
    VR < VL ->
      {ER, Right1} = pop_left(Right),
      merge_tuples(merge_tuples_step(Acc, ER, Delta), Left, Right1, Delta);
    true ->
      {EL, Left1} = pop_left(Left),
      merge_tuples(merge_tuples_step(Acc, EL, Delta), Left1, Right, Delta)
  end.

merge_tuples_step({}, E = #element{delta = 0}, _) ->
  {E};  % first element has rank-delta of 0
merge_tuples_step(Acc, E, Delta) ->
  push_right(Acc, E#element{delta = Delta}).

merge_tuples_batch(Acc, Tree, Delta) ->
  {TreeExceptLast, Last} = pop_right(Tree),
  Transformed = tree_map(fun(E) -> E#element{delta = Delta} end, TreeExceptLast),
  push_right(join_trees(Acc, Transformed), Last).



%% helpers
band_number(Delta, MaxErrors) ->
  band_number(Delta, MaxErrors, 0).

band_number(Delta, MinBound, Acc) ->
  if
    MinBound =< Delta ->
      Acc;
    true ->
      band_number((Delta + 1) bsr 1, MinBound bsr 1, Acc + 1)
  end.

compress(Gk = #gk{tuples = Tuples}) ->
  Size = get_size(Tuples),
  MaxErrors = get_maxerr(Gk),
  if
    MaxErrors > Gk#gk.last_maxerr, Size > 1 ->
      compress(Gk, MaxErrors);
    Size > Gk#gk.capacity - 1 ->
      compress(Gk, MaxErrors);
    true ->
      Gk
  end.

compress(Gk = #gk{tuples = Tuples, compressions = Cpr}, MaxErrors) ->
  {Tuples1, First} = pop_right(Tuples),
  Acc = new_tree(),
  TuplesFin = compress_loop(MaxErrors, Tuples1, First, Acc),
  Gk#gk{tuples = TuplesFin, last_maxerr = MaxErrors, compressions = Cpr + 1}.

compress_loop(_, {}, First, Acc) ->
  push_left(First, Acc);
compress_loop(MaxErrors, Rest0, First = #element{min_rank = G1, delta = D1}, Acc) ->
  {Rest, Second} = pop_right(Rest0),
  #element{min_rank = G2, delta = D2} = Second,
  BandFirst = band_number(D1, MaxErrors),
  BandSecond = band_number(D2, MaxErrors),
  if
    BandFirst >= BandSecond ->
      {Rest1, ToDelete} = take_children(BandSecond, MaxErrors, Rest),
      #measure{total_min_rank = GStar} = tree_measure(ToDelete),
      if
        G1 + G2 + GStar < MaxErrors ->
          % indeed let the merging occur
          First1 = First#element{min_rank = G1 + G2 + GStar},
          compress_loop(MaxErrors, Rest1, First1, Acc);
        true ->
          % meh
          compress_loop(MaxErrors, Rest, Second,
            push_left(First, Acc))
      end;
    true ->
      compress_loop(MaxErrors, Rest, Second,
                    push_left(First, Acc))
  end.

take_children(Band, MaxErrors, Rest) ->
  split_tree_right(
    fun(#measure{max_delta = Delta}) -> band_number(Delta, MaxErrors) >= Band end, Rest).

get_size(Tuples) ->
  #measure{size = Size} = tree_measure(Tuples),
  Size.

get_count(Tuples) ->
  #measure{total_min_rank = Count} = tree_measure(Tuples),
  Count.

get_maxerr(#gk{epsilon = Epsilon, tuples = Tuples}) ->
  get_maxerr(get_count(Tuples), Epsilon).

get_maxerr(Count, Epsilon) ->
  max(1, trunc((Count - 1) * 2 * Epsilon)).

bump_tuples(Gk = #gk{capacity = Cap}, Tuples) ->
  Size = get_size(Tuples),
  NewCap = max(Cap, Size + 1),
  Gk#gk{tuples = Tuples, capacity = NewCap}.




%% FingerTree implementation.

-type element() :: #element{}.
-type measure() :: #measure{}.
-type node(X) ::
{measure(), X, X} |
{measure(), X, X, X}.
-type digit(X) ::
{measure(), X} |
{measure(), X, X} |
{measure(), X, X, X} |
{measure(), X, X, X, X}.
-type tree(X) ::
{} |
{X} |
{measure(), digit(X), tree(node(X)), digit(X)}.
-type some(X) :: {} | {X} | {X, X} | {X, X, X} | {X, X, X, X}.

-type desc() :: element() | node(desc()).

-type ftree() :: tree(element()).


%% Constructors.
-spec tree(Prefix :: digit(X), Middle :: tree(node(X)), Suffix :: digit(X)) -> tree(X) when X :: desc().
tree(Prefix, Middle, Suffix) ->
  {measure_combine(digit_measure(Prefix), tree_measure(Middle), digit_measure(Suffix)),
   Prefix, Middle, Suffix}.


-compile({inline, [node/2, node/3]}).

-spec node(X, X) -> node(X) when X :: desc().
node(Left, Right) ->
  {measure_combine(
    desc_measure(Left), desc_measure(Right)
  ), Left, Right}.
-spec node(X, X, X) -> node(X) when X :: desc().
node(Left, Middle, Right) ->
  {measure_combine(
    desc_measure(Left), desc_measure(Middle), desc_measure(Right)
  ), Left, Middle, Right}.


-compile({inline, [digit/1, digit/2, digit/3, digit/4]}).
-spec digit(X) -> digit(X) when X :: desc().
digit(X) ->
  {desc_measure(X), X}.
-spec digit(X, X) -> digit(X) when X :: desc().
digit(X, Y) ->
  {measure_combine(
    desc_measure(X),
    desc_measure(Y)
  ), X, Y}.
-spec digit(X, X, X) -> digit(X) when X :: desc().
digit(X, Y, Z) ->
  {measure_combine(
    desc_measure(X),
    desc_measure(Y),
    desc_measure(Z)
  ), X, Y, Z}.
-spec digit(X, X, X, X) -> digit(X) when X :: desc().
digit(X, Y, Z, W) ->
  {measure_combine(
    desc_measure(X),
    desc_measure(Y),
    desc_measure(Z),
    desc_measure(W)
  ), X, Y, Z, W}.


%% Measures.
-compile({inline, [desc_measure/1, digit_measure/1, tree_measure/1]}).
desc_measure(Element = #element{min_rank = MinRank, delta = Delta}) ->
  #measure{right = Element, total_min_rank = MinRank, max_delta = Delta, size = 1};
desc_measure(_Node3 = {Measure, _, _, _}) ->
  Measure;
desc_measure(_Node2 = {Measure, _, _}) ->
  Measure.

digit_measure(Digit) ->
  element(1, Digit).

tree_measure({}) ->
  ?NULL_MEASURE;
tree_measure({X}) ->
  desc_measure(X);
tree_measure({Measure, _, _, _}) ->
  Measure.


%% Assuming sorted input.
-compile({inline, [measure_combine/2, measure_combine/3, measure_combine/4]}).

measure_combine(Measure, #measure{right = undefined}) ->
  Measure;
measure_combine(#measure{total_min_rank = G1, max_delta = D1, right = _V1, size = C1},
                #measure{total_min_rank = G2, max_delta = D2, right = V2, size = C2}) ->
  #measure{total_min_rank = G1 + G2, max_delta = max(D1, D2), right = V2, size = C1 + C2}.

measure_combine(M1, M2, M3) ->
  measure_combine(M1, measure_combine(M2, M3)).

measure_combine(M1, M2, M3, M4) ->
  measure_combine(M1, measure_combine(M2, M3, M4)).

%% conversions
-compile({inline, [node_to_digit/1]}).
-spec digit_to_tree(digit(X)) -> tree(X) when X :: desc().
digit_to_tree({_Measure, X}) ->
  {X};
digit_to_tree({Measure, X, Y}) ->
  {Measure, digit(X), {}, digit(Y)};
digit_to_tree({Measure, X, Y, Z}) ->
  {Measure, digit(X, Y), {}, digit(Z)};
digit_to_tree({Measure, X, Y, Z, W}) ->
  {Measure, digit(X, Y), {}, digit(Z, W)}.

-spec node_to_digit(node(X)) -> digit(X) when X :: desc().
node_to_digit(X) ->
  X.

-spec some_to_digit(some(X)) -> digit(X) when X :: desc().
some_to_digit({X}) ->
  digit(X);
some_to_digit({X, Y}) ->
  digit(X, Y);
some_to_digit({X, Y, Z}) ->
  digit(X, Y, Z);
some_to_digit({X, Y, Z, W}) ->
  digit(X, Y, Z, W).

some_to_tree({}) ->
  {};
some_to_tree({X}) ->
  {X};
some_to_tree(X) ->
  digit_to_tree(some_to_digit(X)).

%% deque operations

-spec tree_left(some(X), tree(node(X)), digit(X)) -> tree(X) when X :: desc().
tree_left({}, {}, Suffix) ->
  digit_to_tree(Suffix);
tree_left({}, Middle, Suffix) ->
  {Node, Middle1} = pop_left(Middle),
  tree(node_to_digit(Node), Middle1, Suffix);
tree_left(Some, Middle, Suffix) ->
  tree(some_to_digit(Some), Middle, Suffix).

-spec pop_left(tree(X)) -> {X, tree(X)} when X :: desc().
pop_left({}) ->
  error(badarg);
pop_left({X}) ->
  {X, {}};
pop_left({_, Prefix, Middle, Suffix}) ->
  case Prefix of
    {_, X} ->
      {X, tree_left({}, Middle, Suffix)};
    {_, X, Y} ->
      {X, tree(digit(Y), Middle, Suffix)};
    {_, X, Y, Z} ->
      {X, tree(digit(Y, Z), Middle, Suffix)};
    {_, X, Y, Z, W} ->
      {X, tree(digit(Y, Z, W), Middle, Suffix)}
  end.

-spec tree_right(digit(X), tree(node(X)), some(X)) -> tree(X) when X :: desc().
tree_right(Prefix, {}, {}) ->
  digit_to_tree(Prefix);
tree_right(Prefix, Middle, {}) ->
  {Middle1, Node} = pop_right(Middle),
  tree(Prefix, Middle1, node_to_digit(Node));
tree_right(Prefix, Middle, Some) ->
  tree(Prefix, Middle, some_to_digit(Some)).

-spec pop_right(tree(X)) -> {tree(X), X} when X :: desc().
pop_right({}) ->
  error(badarg);
pop_right({X}) ->
  {{}, X};
pop_right({_, Prefix, Middle, Suffix}) ->
  case Suffix of
    {_, X} ->
      {tree_right(Prefix, Middle, {}), X};
    {_, Y, X} ->
      {tree(Prefix, Middle, digit(Y)), X};
    {_, Z, Y, X} ->
      {tree(Prefix, Middle, digit(Z, Y)), X};
    {_, W, Z, Y, X} ->
      {tree(Prefix, Middle, digit(W, Z, Y)), X}
  end.

-spec push_left(X, tree(X)) -> tree(X) when X :: desc().
push_left(X, {}) ->
  {X};
push_left(X, {Y}) ->
  tree(digit(X), {}, digit(Y));
push_left(X, {_, Prefix, Middle, Suffix}) ->
  case Prefix of
    {_, Y} ->
      tree(digit(X, Y), Middle, Suffix);
    {_, Y, Z} ->
      tree(digit(X, Y, Z), Middle, Suffix);
    {_, Y, Z, W} ->
      tree(digit(X, Y, Z, W), Middle, Suffix);
    {_, Y, Z, W, TooMuch} ->
      Middle1 = push_left(node(Z, W, TooMuch), Middle),
      tree(digit(X, Y), Middle1, Suffix)
  end.

-spec push_right(tree(X), X) -> tree(X) when X :: desc().
push_right({}, X) ->
  {X};
push_right({Y}, X) ->
  tree(digit(Y), {}, digit(X));
push_right({_, Prefix, Middle, Suffix}, X) ->
  case Suffix of
    {_, Y} ->
      tree(Prefix, Middle, digit(Y, X));
    {_, Z, Y} ->
      tree(Prefix, Middle, digit(Z, Y, X));
    {_, W, Z, Y} ->
      tree(Prefix, Middle, digit(W, Z, Y, X));
    {_, TooMuch, W, Z, Y} ->
      Middle1 = push_right(Middle, node(TooMuch, W, Z)),
      tree(Prefix, Middle1, digit(Y, X))
  end.

%% Merging.
-spec push_left_some(some(X), tree(X)) -> tree(X) when X :: desc().
push_left_some({}, Tree) ->
  Tree;
push_left_some({X}, Tree) ->
  push_left(X, Tree);
push_left_some({X, Y}, Tree) ->
  push_left(X, push_left(Y, Tree));
push_left_some({X, Y, Z}, Tree) ->
  push_left(X, push_left(Y, push_left(Z, Tree)));
push_left_some({X, Y, Z, W}, Tree) ->
  push_left(X, push_left(Y, push_left(Z, push_left(W, Tree)))).

-spec push_right_some(tree(X), some(X)) -> tree(X) when X :: desc().
push_right_some(Tree, {}) ->
  Tree;
push_right_some(Tree, {X}) ->
  push_right(Tree, X);
push_right_some(Tree, {Y, X}) ->
  push_right(push_right(Tree, Y), X);
push_right_some(Tree, {Z, Y, X}) ->
  push_right(push_right(push_right(Tree, Z), Y), X);
push_right_some(Tree, {W, Z, Y, X}) ->
  push_right(push_right(push_right(push_right(Tree, W), Z), Y), X).

-include("ftree_generate.hrl").

-spec join_trees(tree(X), tree(X)) -> tree(X) when X :: desc().
join_trees(Left, Right) ->
  join_trees(Left, {}, Right).
-spec join_trees(tree(X), some(X), tree(X)) -> tree(X) when X :: desc().
join_trees({}, Some, TreeR) ->
  push_left_some(Some, TreeR);
join_trees(TreeL, Some, {}) ->
  push_right_some(TreeL, Some);
join_trees({X}, Some, TreeR) ->
  push_left(X, push_left_some(Some, TreeR));
join_trees(TreeL, Some, {X}) ->
  push_right(push_right_some(TreeL, Some), X);
join_trees({_, Left, MiddleLeft, InnerLeft}, Some, {_, InnerRight, MiddleRight, Right}) ->
  SomeDeep = join_digits(InnerLeft, Some, InnerRight),
  tree(Left, join_trees(MiddleLeft, SomeDeep, MiddleRight), Right).

%% Splitting on a predicate, it may return true | false.

-type predicate() :: fun((measure()) -> boolean()).
-type split(C) :: {C, C}.

-spec split_digit_left(predicate(), measure(), digit(X)) -> split(some(X)) when X :: desc().
split_digit_left(_Pred, _P0, {_, X}) ->
  {{}, {X}};
split_digit_left(Pred, P0, {_, X, Y}) ->
  Xm = measure_combine(P0, desc_measure(X)),
  case Pred(Xm) of
    true ->
      {{}, {X, Y}};
    _ ->
      {{X}, {Y}}
  end;
split_digit_left(Pred, P0, {_, X, Y, Z}) ->
  Xm = measure_combine(P0, desc_measure(X)),
  XYm = measure_combine(Xm, desc_measure(Y)),
  case {Pred(Xm), Pred(XYm)} of
    {true, _} ->
      {{}, {X, Y, Z}};
    {_, true} ->
      {{X}, {Y, Z}};
    _ ->
      {{X, Y}, {Z}}
  end;
split_digit_left(Pred, P0, {_, X, Y, Z, W}) ->
  Xm = measure_combine(P0, desc_measure(X)),
  XYm = measure_combine(Xm, desc_measure(Y)),
  XYZm = measure_combine(XYm, desc_measure(Z)),
  case {Pred(Xm), Pred(XYm), Pred(XYZm)} of
    {true, _, _} ->
      {{}, {X, Y, Z, W}};
    {_, true, _} ->
      {{X}, {Y, Z, W}};
    {_, _, true} ->
      {{X, Y}, {Z, W}};
    _ ->
      {{X, Y, Z}, {W}}
  end.

-spec split_digit_right(predicate(), digit(X), measure()) -> split(some(X)) when X :: desc().
split_digit_right(_Pred, {_, X}, _P0) ->
  {{X}, {}};
split_digit_right(Pred, {_, Y, X}, P0) ->
  Xm = measure_combine(desc_measure(X), P0),
  case Pred(Xm) of
    true ->
      {{Y, X}, {}};
    _ ->
      {{Y}, {X}}
  end;
split_digit_right(Pred, {_, Z, Y, X}, P0) ->
  Xm = measure_combine(desc_measure(X), P0),
  YXm = measure_combine(desc_measure(Y), Xm),
  case {Pred(Xm), Pred(YXm)} of
    {true, _} ->
      {{Z, Y, X}, {}};
    {_, true} ->
      {{Z, Y}, {X}};
    _ ->
      {{Z}, {Y, X}}
  end;
split_digit_right(Pred, {_, W, Z, Y, X}, P0) ->
  Xm = measure_combine(desc_measure(X), P0),
  YXm = measure_combine(desc_measure(Y), Xm),
  ZYXm = measure_combine(desc_measure(Z), YXm),
  case {Pred(Xm), Pred(YXm), Pred(ZYXm)} of
    {true, _, _} ->
      {{W, Z, Y, X}, {}};
    {_, true, _} ->
      {{W, Z, Y}, {X}};
    {_, _, true} ->
      {{W, Z}, {Y, X}};
    _ ->
      {{W}, {Z, Y, X}}
  end.


-spec split_tree_left(predicate(), ftree()) -> split(ftree()).
split_tree_left(Pred, Tree) ->
  case Pred(tree_measure(Tree)) of
    false ->
      {Tree, {}};
    _ ->
      split_tree_left(Pred, ?NULL_MEASURE, Tree)
  end.

-spec split_tree_left(predicate(), measure(), tree(X)) -> split(tree(X)) when X :: desc().
split_tree_left(_, _, {}) ->
  {{}, {}};
split_tree_left(_, _, {X}) ->
  %% on the whole tree, Pred(P0 + |X|) is already deemed true
  {{}, {X}};
split_tree_left(Pred, P0, {_, Left, Middle, Right}) ->
  Pm = measure_combine(P0, digit_measure(Left)),
  PMm = measure_combine(Pm, tree_measure(Middle)),
  case {Pred(Pm), Pred(PMm)} of
    {true, _} ->
      {SomeLeft, SomeRight} = split_digit_left(Pred, P0, Left),
      {some_to_tree(SomeLeft), tree_left(SomeRight, Middle, Right)};
    {_, true} ->
      {LeftSub, RightSub} = split_tree_left(Pred, Pm, Middle),
      {tree_right(Left, LeftSub, {}), tree_left({}, RightSub, Right)};
    _ ->
      {SomeLeft, SomeRight} = split_digit_left(Pred, PMm, Right),
      {tree_right(Left, Middle, SomeLeft), some_to_tree(SomeRight)}
  end.

-spec split_tree_right(predicate(), ftree()) -> split(ftree()).
split_tree_right(Pred, Tree) ->
  case Pred(tree_measure(Tree)) of
    false ->
      {{}, Tree};
    _ ->
      split_tree_right(Pred, Tree, ?NULL_MEASURE)
  end.

-spec split_tree_right(predicate(), tree(X), measure()) -> split(tree(X)) when X :: desc().
split_tree_right(_, {}, _) ->
  {{}, {}};
split_tree_right(_, {X}, _) ->
  {{X}, {}};
split_tree_right(Pred, {_, Left, Middle, Right}, P0) ->
  Pm = measure_combine(digit_measure(Right), P0),
  PMm = measure_combine(tree_measure(Middle), Pm),
  case {Pred(Pm), Pred(PMm)} of
    {true, _} ->
      {SomeLeft, SomeRight} = split_digit_right(Pred, Right, PMm),
      {tree_right(Left, Middle, SomeLeft), some_to_tree(SomeRight)};
    {_, true} ->
      {LeftSub, RightSub} = split_tree_right(Pred, Middle, Pm),
      {tree_right(Left, LeftSub, {}), tree_left({}, RightSub, Right)};
    _ ->
      {SomeLeft, SomeRight} = split_digit_right(Pred, Left, P0),
      {some_to_tree(SomeLeft), tree_left(SomeRight, Middle, Right)}
  end.


%% API
-spec new_tree() -> ftree().
new_tree() ->
  {}.

-spec is_empty(ftree()) -> boolean().
is_empty({}) ->
  true;
is_empty(_) ->
  false.

-spec peek_left(ftree()) -> element().
peek_left({X}) ->
  X;
peek_left({_, {_, X}, _, _}) ->
  X;
peek_left({_, {_, X, _}, _, _}) ->
  X;
peek_left({_, {_, X, _, _}, _, _}) ->
  X;
peek_left({_, {_, X, _, _, _}, _, _}) ->
  X.


-spec peek_right(ftree()) -> element().
peek_right({X}) ->
  X;
peek_right({_, _, _, {_, X}}) ->
  X;
peek_right({_, _, _, {_, _, X}}) ->
  X;
peek_right({_, _, _, {_, _, _, X}}) ->
  X;
peek_right({_, _, _, {_, _, _, _, X}}) ->
  X.

-spec tree_map(fun((element()) -> element()), tree(X)) -> tree(X) when X :: desc().
tree_map(_, {}) ->
  {};
tree_map(Fun, {X}) ->
  {desc_map(Fun, X)};
tree_map(Fun, {_, Left, Mid, Right}) ->
  tree(digit_map(Fun, Left), tree_map(Fun, Mid), digit_map(Fun, Right)).

desc_map(Fun, X) when is_record(X, element) ->
  Fun(X);
desc_map(Fun, {_, X, Y}) ->
  node(desc_map(Fun, X), desc_map(Fun, Y));
desc_map(Fun, {_, X, Y, Z}) ->
  node(desc_map(Fun, X), desc_map(Fun, Y), desc_map(Fun, Z)).

digit_map(Fun, {_, X}) ->
  digit(desc_map(Fun, X));
digit_map(Fun, {_, X, Y}) ->
  digit(desc_map(Fun, X), desc_map(Fun, Y));
digit_map(Fun, {_, X, Y, Z}) ->
  digit(desc_map(Fun, X), desc_map(Fun, Y), desc_map(Fun, Z));
digit_map(Fun, {_, X, Y, Z, W}) ->
  digit(desc_map(Fun, X), desc_map(Fun, Y), desc_map(Fun, Z), desc_map(Fun, W)).
