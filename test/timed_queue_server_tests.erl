-module(timed_queue_server_tests).

-export([test_sync_fn/0]).

-include_lib("eunit/include/eunit.hrl").

-define(default_queue_name, test_queue_one).
-define(second_queue_name, test_queue_two).
-define(default_reservation_time, 1000).
-define(default_server_config,
        #{ queue_name => ?default_queue_name
         , reservation_time => ?default_reservation_time
         }).
-define(test_value_1, value_1).
-define(test_value_2, value_2).

start_stop_test() ->
  ensure_start_server(),
  ?assertEqual(ok, stop_server(?default_queue_name)).

cannot_start_twice_test() ->
  {ok, Srv1Pid} = ensure_start_server(),
  Srv2 = start_server_default(),
  ?assertMatch({error, {already_started, _}}, Srv2),
  {_, {_, Srv2Pid}} = Srv2,
  ?assertEqual(Srv1Pid, Srv2Pid),
  ?assertEqual(ok, stop_server(?default_queue_name)).

can_start_two_different_queue_servers_test() ->
  {ok, Srv1Pid} = ensure_start_server(),
  {ok, Srv2Pid} =
    ensure_start_server(#{ queue_name => ?second_queue_name
                         , reservation_time => ?default_reservation_time
                         }),
  ?assertNotEqual(Srv1Pid, Srv2Pid),
  ?assertEqual(ok, stop_server(?default_queue_name)),
  ?assertEqual(ok, stop_server(?second_queue_name)).

simple_reserve_test() ->
  ensure_start_server_with_values([?test_value_1]),
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  ReserveResult2 = timed_queue_server:reserve(?default_queue_name),
  ?assertEqual(queue_empty, ReserveResult2),
  stop_server(?default_queue_name).

custom_reserve_test() ->
  ensure_start_server_with_values([?test_value_1]),
  CustTimeout = 10,
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name, CustTimeout),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  timer:sleep(CustTimeout),
  ReserveResult2 = timed_queue_server:reserve(?default_queue_name, CustTimeout),
  ?assertMatch({_, ?test_value_1}, ReserveResult2),
  stop_server(?default_queue_name).

delete_test() ->
  ensure_start_server_with_values([?test_value_1]),
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  {Key1, _} = ReserveResult1,
  ok = timed_queue_server:delete(?default_queue_name, Key1),
  ReserveResult2 = timed_queue_server:reserve(?default_queue_name),
  ?assertEqual(queue_empty, ReserveResult2),
  timed_queue_server:insert(?default_queue_name, [?test_value_2]),
  ReserveResult3 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_2}, ReserveResult3),
  {Key2, _} = ReserveResult3,
  ok = timed_queue_server:delete(?default_queue_name, Key2),
  ReserveResult4 = timed_queue_server:reserve(?default_queue_name),
  ?assertEqual(queue_empty, ReserveResult4),
  stop_server(?default_queue_name).

cannot_delete_expired_test() ->
  ensure_start_server_with_values([?test_value_1]),
  CustTimeout = 10,
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name, CustTimeout),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  {Key1, _} = ReserveResult1,
  timer:sleep(CustTimeout),
  ok = timed_queue_server:delete(?default_queue_name, Key1),
  ReserveResult2 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult2),
  stop_server(?default_queue_name).

prolongate_test() ->
  ensure_start_server_with_values([?test_value_1]),
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  {Key1, _} = ReserveResult1,
  ProlongResult1 = timed_queue_server:prolongate(?default_queue_name, Key1),
  ?assertMatch({ok, _}, ProlongResult1),
  stop_server(?default_queue_name).

cannot_prolongate_expired_test() ->
  ensure_start_server_with_values([?test_value_1]),
  CustTimeout = 10,
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name, CustTimeout),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  {Key1, _} = ReserveResult1,
  timer:sleep(CustTimeout),
  ProlongResult1 = timed_queue_server:prolongate(?default_queue_name, Key1),
  ?assertEqual({error, reservation_expired}, ProlongResult1),
  stop_server(?default_queue_name).

cannot_prolongate_unknown_test() ->
  ensure_start_server_with_values([?test_value_1]),
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  RandKey = 1,
  ProlongResult1 = timed_queue_server:prolongate(?default_queue_name, RandKey),
  ?assertEqual({error, key_not_exists}, ProlongResult1),
  stop_server(?default_queue_name).

release_test() ->
  ensure_start_server_with_values([?test_value_1]),
  ReserveResult1 = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult1),
  {Key1, _} = ReserveResult1,
  ReleaseResult1 = timed_queue_server:release(?default_queue_name, Key1),
  ?assertEqual(ok, ReleaseResult1),
  %% Unknown reservation key
  RandKey = 1,
  ReleaseResult2 = timed_queue_server:release(?default_queue_name, RandKey),
  ?assertEqual({error, key_not_exists}, ReleaseResult2),
  %% Reservation expired
  CustTimeout = 10,
  ReserveResult2 = timed_queue_server:reserve(?default_queue_name, CustTimeout),
  ?assertMatch({_, ?test_value_1}, ReserveResult2),
  {Key2, _} = ReserveResult2,
  timer:sleep(CustTimeout),
  ReleaseResult3 = timed_queue_server:release(?default_queue_name, Key2),
  ?assertEqual({error, reservation_expired}, ReleaseResult3),
  stop_server(?default_queue_name).

sync_test() ->
  Config = maps:merge(?default_server_config,
                      #{ sync_fn => {?MODULE, test_sync_fn, []}
                       , sync_interval => 0}), % trigger if a queue is empty
  ensure_start_server(Config),
  ?assertEqual(queue_empty, timed_queue_server:reserve(?default_queue_name)),
  %% the sync fn should be triggered
  ReserveResult = timed_queue_server:reserve(?default_queue_name),
  ?assertMatch({_, ?test_value_1}, ReserveResult),
  stop_server(?default_queue_name).

test_sync_fn() ->
  {ok, [?test_value_1]}.

start_server_default() ->
  start_server(?default_server_config).

start_server(Config) ->
  timed_queue_server:start_link(Config).

stop_server(QueueName) ->
  timed_queue_server:stop(QueueName).

ensure_start_server() ->
  ensure_start_server(?default_server_config).

ensure_start_server_with_values(Values) ->
  Config = maps:merge(?default_server_config, #{values => Values}),
  ensure_start_server(Config).

ensure_start_server(Config) ->
  %%stop_server(?default_queue_name),
  Srv = start_server(Config),
  ?assertMatch({ok, _}, Srv),
  {ok, SrvPid} = Srv,
  ?assert(is_pid(SrvPid)),
  Srv.
