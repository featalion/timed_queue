-module(timed_queue_tests).

-include_lib("eunit/include/eunit.hrl").
-include("timed_queue.hrl").

new_test() ->
  Q1 = timed_queue:new(),
  ?assertMatch(#timed_queue{ set = undefined
                           , unique = false
                           , max_reservations = unlimited
                           },
               Q1),
  Q2 = timed_queue:new(#{unique => true}),
  ASet = sets:new(),
  ?assertMatch(#timed_queue{ set = ASet
                           , unique = true
                           , max_reservations = unlimited
                           },
               Q2),
  Q3 = timed_queue:new(#{max_reservations => 3}),
  ?assertMatch(#timed_queue{ set = undefined
                           , unique = false
                           , max_reservations = 3
                           },
               Q3),
  Q4 = timed_queue:new(#{unique => true, max_reservations => 3}),
  ?assertMatch(#timed_queue{ set = ASet
                           , unique = true
                           , max_reservations = 3
                           },
               Q4),
  ?assertException(error, bad_arg,
                   timed_queue:new(#{unique => very_unique})),
  ?assertException(error, bad_arg,
                   timed_queue:new(#{max_reservations => -20})),
  ?assertException(error, bad_arg,
                   timed_queue:new(#{max_reservations => a_lot})).

insert_test() ->
  Q0 = timed_queue:new(),
  Q1 = timed_queue:insert(the_value, Q0),
  ?assertEqual(1, gb_trees:size(Q1#timed_queue.tree)).

insert_unique_test() ->
  Q0 = timed_queue:new(#{unique => true}),
  V1 = the_value,
  Q1 = timed_queue:insert(V1, Q0),
  Q2 = timed_queue:insert(V1, Q1),
  ?assertEqual(1, gb_trees:size(Q2#timed_queue.tree)),
  V2 = new_value,
  Q3 = timed_queue:insert(V2, Q2),
  Q4 = timed_queue:insert(V2, Q3),
  ?assertEqual(2, gb_trees:size(Q4#timed_queue.tree)).

reserve_test() ->
  Q0 = timed_queue:new(),
  Result1 = timed_queue:reserve(1000, Q0),
  ?assertEqual(2, size(Result1)),
  {queue_empty, Q1} = Result1,
  Value = the_value,
  Q2 = timed_queue:insert(Value, Q1),
  Result2 = timed_queue:reserve(1000, Q2),
  ?assertEqual(3, size(Result2)),
  {K, V, Q3} = Result2,
  ?assert(is_integer(K) andalso K > 0),
  ?assertEqual(Value, V),
  ?assertMatch(#timed_queue{}, Q3).

delete_test() ->
  Q0 = timed_queue:new(),
  Value = the_value,
  Q1 = timed_queue:insert(Value, Q0),
  {K, V, Q2} = timed_queue:reserve(1000, Q1),
  ?assertEqual(Value, V),
  Q3 = timed_queue:delete(K, Q2),
  ?assertMatch(#timed_queue{}, Q3),
  ?assertEqual(0, gb_trees:size(Q3#timed_queue.tree)).

prolongate_test() ->
  Q0 = timed_queue:new(),
  Value = the_value,
  Q1 = timed_queue:insert(Value, Q0),
  {K1, V1, Q2} = timed_queue:reserve(1000, Q1),
  ?assertEqual(Value, V1),
  {queue_empty, Q3} = timed_queue:reserve(1000, Q2),
  Result1 = timed_queue:prolongate(K1, 1000, Q3),
  ?assertEqual(3, size(Result1)),
  {K2, V2, Q4} = Result1,
  ?assert(K1 < K2),
  ?assertEqual(Value, V2),
  Q5 = timed_queue:delete(K2, Q4),
  ?assertMatch(#timed_queue{}, Q5),
  ?assertEqual(0, gb_trees:size(Q5#timed_queue.tree)),
  Q6 = timed_queue:insert(Value, Q5),
  Result2 = timed_queue:reserve(1, Q6),
  ?assertEqual(3, size(Result2)),
  {K3, V3, Q7} = Result2,
  ?assertEqual(Value, V3),
  timer:sleep(1),
  Result3 = timed_queue:prolongate(K3, 1000, Q7),
  ?assertEqual(2, size(Result3)),
  ?assertMatch({reservation_expired, #timed_queue{}}, Result3),
  {_, Q8} = Result3,
  RandomKey = 1234,
  Result4 = timed_queue:prolongate(RandomKey, 1000, Q8),
  ?assertEqual(2, size(Result4)),
  ?assertMatch({key_not_exists, #timed_queue{}}, Result4).

max_reservations_test() ->
  MaxReservs = 2,
  Q0 = timed_queue:new(#{max_reservations => MaxReservs}),
  Value = the_value,
  Q1 = timed_queue:insert(Value, Q0),
  Timeout = 10,
  Q2 = reserve_and_wait_expiration(Q1, Timeout, Value, MaxReservs),
  Result = timed_queue:reserve(Timeout, Q2),
  ?assertMatch({queue_empty, #timed_queue{}}, Result),
  {_, Q3} = Result,
  ?assertEqual(0, gb_trees:size(Q3#timed_queue.tree)).

reserve_and_wait_expiration(Queue, Timeout, ExpValue, Times) ->
  Fn = fun(_, Q0) ->
           Result = timed_queue:reserve(Timeout, Q0),
           ?assertMatch({_, ExpValue, #timed_queue{}}, Result),
           {_, _, Q} = Result,
           timer:sleep(Timeout),
           Q
       end,
  lists:foldl(Fn, Queue, lists:seq(1, Times)).
