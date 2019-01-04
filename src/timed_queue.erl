-module(timed_queue).

-export([ new/0
        , new/1
        , insert/2
        , reserve/2
        , prolongate/3
        , release/2
        , delete/2
        ]).

-include("timed_queue.hrl").

-type queue() :: #timed_queue{}.

-type key() :: pos_integer().
-type value() :: any().
-type reservation_time() :: pos_integer().

-record(entry, { value :: value()
               , reservations = 0 :: non_neg_integer()
               }).
-type entry() :: #entry{}.

-type config() :: #{ unique => boolean()
                   , max_reservations => pos_integer()
                   }.

-type key_error() :: key_not_exists | reservation_expired.

-export_type([ queue/0
             , key/0
             , value/0
             , key_error/0
             ]).

%% API functions

-spec new() -> queue() | no_return().
new() ->
  new(#{}).

-spec new(config()) -> queue() | no_return().
new(Config) ->
  Queue0 = #timed_queue{tree = gb_trees:empty()},
  Queue1 = set_unique(Queue0, maps:get(unique, Config, false)),
  set_max_reservations(Queue1, maps:get(max_reservations, Config, unlimited)).

-spec insert(value(), queue()) -> queue().
insert(Value, Queue = #timed_queue{unique = true, set = ValuesSet}) ->
  case sets:is_element(Value, ValuesSet) of
    false ->
      Key = os:system_time(nanosecond),
      Entry = #entry{value = Value, reservations = 0},
      {_RealKey, NewQueue} = insert(Key, Entry, Queue),
      NewValuesSet = sets:add_element(Value, ValuesSet),
      NewQueue#timed_queue{set = NewValuesSet};
    true  ->
      Queue
  end;
insert(Value, Queue) ->
  Key = os:system_time(nanosecond),
  Entry = #entry{value = Value, reservations = 0},
  {_RealKey, NewQueue} = insert(Key, Entry, Queue),
  NewQueue.

-spec reserve(reservation_time(), queue()) -> {key(), value(), queue()} |
                                              {queue_empty, queue()}.
reserve(Millis, Queue = #timed_queue{tree = Tree}) ->
  case gb_trees:is_empty(Tree) of
    true ->
      {queue_empty, Queue};
    false ->
      {Key, Entry} = gb_trees:smallest(Tree),
      NowNanos = os:system_time(nanosecond),
      case Key > NowNanos of
        true ->
          {queue_empty, Queue};
        false ->
          do_reserve(Key, Entry, Millis, NowNanos, Queue)
      end
  end.

-spec prolongate(key(), reservation_time(), queue())
                -> {key(), value(), queue()} | {key_error(), queue()}.
prolongate(Key, Millis, Queue = #timed_queue{tree = Tree}) ->
  case {gb_trees:is_defined(Key, Tree), Key > os:system_time(nanosecond)} of
    {true, true} ->
      {Entry, NewTree} = gb_trees:take(Key, Tree),
      NewKey = Key + Millis * 1000000,
      {RealKey, NewQueue} =
        insert(NewKey, Entry, Queue#timed_queue{tree = NewTree}),
      {RealKey, Entry#entry.value, NewQueue};
    {true, false} ->
      {reservation_expired, Queue};
    {false, _} ->
      {key_not_exists, Queue}
  end.

-spec release(key(), queue()) -> queue() | {key_error(), queue()}.
release(Key, Queue = #timed_queue{tree = Tree}) ->
  case gb_trees:is_defined(Key, Tree) of
    true ->
      NowNanos = os:system_time(nanosecond),
      case Key > NowNanos of
        true ->
          {Entry, NewTree} = gb_trees:take(Key, Tree),
          {_NewKey, NewQueue} =
            insert(NowNanos, Entry, Queue#timed_queue{tree = NewTree}),
          {Key, Entry#entry.value, NewQueue};
        false ->
          {reservation_expired, Queue}
      end;
    false ->
      {key_not_exists, Queue}
  end.

-spec delete(key(), queue()) -> queue().
delete(Key, Queue = #timed_queue{tree = Tree}) ->
  case gb_trees:take_any(Key, Tree) of
    error ->
      Queue;
    {Entry, NewTree} ->
      case Queue#timed_queue.unique of
        true ->
          NewVSet = sets:del_element(Entry#entry.value, Queue#timed_queue.set),
          Queue#timed_queue{tree = NewTree, set = NewVSet};
        false ->
          Queue#timed_queue{tree = NewTree}
      end
  end.

%% Internal functions

-spec set_unique(queue(), any()) -> queue() | no_return().
set_unique(Queue, true) ->
  Queue#timed_queue{set = sets:new(), unique = true};
set_unique(Queue, false) ->
  Queue;
set_unique(_Queue, _Unique) ->
  error(bad_arg, {unique, must_be_boolean}).

-spec set_max_reservations(queue(), any()) -> queue() | no_return().
set_max_reservations(Queue, MaxReservations)
  when is_integer(MaxReservations) andalso MaxReservations > 0 ->
  Queue#timed_queue{max_reservations = MaxReservations};
set_max_reservations(Queue, unlimited) ->
  Queue;
set_max_reservations(_Queue, _MaxReservations) ->
  error(bad_arg, {max_reservations, must_be_positive_integer}).

-spec insert(key(), entry(), queue()) -> {key(), queue()}.
insert(Key, Entry, Queue = #timed_queue{tree = Tree}) ->
  case gb_trees:is_defined(Key, Tree) of
    true ->
      insert(Key + 1, Entry, Queue); % find available key
    false ->
      NewTree = gb_trees:insert(Key, Entry, Tree),
      {Key, Queue#timed_queue{tree = NewTree}}
  end.

-spec do_reserve(key(), entry(), reservation_time(), pos_integer(), queue())
                -> {key(), value(), queue()} | {queue_empty, queue()}.
do_reserve(Key,
           Entry = #entry{reservations = Reservs},
           Millis,
           NowNanos,
           Queue = #timed_queue{tree = Tree, max_reservations = MaxReservs})
  when MaxReservs == unlimited orelse Reservs < MaxReservs ->
  NewKey = NowNanos + Millis * 1000000,
  NewTree = gb_trees:delete(Key, Tree),
  NewEntry = Entry#entry{reservations = Reservs + 1},
  {RealKey, NewQueue} =
    insert(NewKey, NewEntry, Queue#timed_queue{tree = NewTree}),
  {RealKey, Entry#entry.value, NewQueue};
do_reserve(Key, _Entry, Millis, _NowNanos, Queue) ->
  NewQueue = delete(Key, Queue),
  reserve(Millis, NewQueue).
