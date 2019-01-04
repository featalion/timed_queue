-module(timed_queue_server).

-behaviour(gen_server).

-export([ start_link/1
        ]).

-export([ delete/2
        , insert/2
        , prolongate/2
        , prolongate/3
        , release/2
        , reserve/1
        , reserve/2
        , sync/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , terminate/2
        ]).

-type sync_fn() :: {module(), atom(), [any()]}.
-type config() :: #{ queue_name := atom()
                   , reservation_time := pos_integer()
                   , sync_fn => sync_fn()
                   , sync_interval => non_neg_integer()
                   , values => [any()]
                   }.
-type state() :: #{ queue_name := atom()
                  , queue := timed_queue:queue()
                  , reservation_time := pos_integer()
                  , last_synced := non_neg_integer()
                  , sync_fn => sync_fn()
                  , sync_interval => non_neg_integer()
                  }.
-type reservation() :: {timed_queue:key(), timed_queue:value()} | queue_empty.

-spec start_link(config()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Config = #{queue_name := QueueName}) ->
  gen_server:start_link({local, QueueName}, ?MODULE, Config, []).

%% API functions

-spec delete(atom(), integer()) -> ok.
delete(QueueName, ReservationKey) ->
  gen_server:cast(QueueName, {delete, ReservationKey}).

-spec insert(atom(), [any()]) -> ok.
insert(QueueName, Values) ->
  gen_server:cast(QueueName, {insert, Values}).

-spec prolongate(atom(), timed_queue:key())
                -> {ok, timed_queue:key()} | {error, timed_queue:key_error()}.
prolongate(QueueName, ReservationKey) ->
  gen_server:call(QueueName, {prolongate, ReservationKey}).

-spec prolongate(atom(), timed_queue:key(), pos_integer())
                -> {ok, timed_queue:key()} | {error, timed_queue:key_error()}.
prolongate(QueueName, ReservationKey, ProlongationTime) ->
  gen_server:call(QueueName, {prolongate, ReservationKey, ProlongationTime}).

-spec release(atom(), timed_queue:key())
             -> ok | {error, timed_queue:key_error()}.
release(QueueName, ReservationKey) ->
  gen_server:call(QueueName, {release, ReservationKey}).

-spec reserve(atom()) -> {integer(), any()} | queue_empty.
reserve(QueueName) ->
  gen_server:call(QueueName, reserve).

-spec reserve(atom(), integer()) -> {integer(), any()} | queue_empty.
reserve(QueueName, ReservationTime) ->
  gen_server:call(QueueName, {reserve, ReservationTime}).

-spec sync(atom()) -> ok.
sync(QueueName) ->
  gen_server:cast(QueueName, sync).

%% Behavour functions (gen_server)

-spec init(config()) -> {ok, state()}.
init(Config = #{values := Values, reservation_time := _}) ->
  Queue0 = timed_queue:new(),
  Queue = lists:foldl(fun timed_queue:insert/2, Queue0, Values),
  State = maps:without([values], Config),
  {ok, State#{queue => Queue}};
init(Config = #{reservation_time := _}) ->
  init(Config#{values => []}).

-spec handle_call(reserve | {reserve, pos_integer()}, {pid(), any()}, state())
                 -> {reply, reservation(), state()};
                 ( {prolongate, timed_queue:key()} |
                   {prolongate, timed_queue:key(), pos_integer()}
                 , {pid(), any()}
                 , state())
                 -> {reply
                    , reservation() | timed_queue:prolongation_error()
                    , state()};
                 ({release, timed_queue:key()}, {pid(), any()}, state())
                 -> {reply, ok | {error, timed_queue:key_error()}, state()}.
handle_call(reserve, _From, State = #{reservation_time := ReservationTime}) ->
  reserve_and_reply(ReservationTime, State);
handle_call({reserve, ReservationTime}, _From, State) ->
  reserve_and_reply(ReservationTime, State);
handle_call({prolongate, ReservationKey},
            _From,
            State = #{reservation_time := ReservationTime}) ->
  prolongate_and_reply(ReservationKey, ReservationTime, State);
handle_call({prolongate, ReservationKey, ReservationTime}, _From, State) ->
  prolongate_and_reply(ReservationKey, ReservationTime, State);
handle_call({release, ReservationKey}, _From, State = #{queue := Queue}) ->
  case timed_queue:release(ReservationKey, Queue) of
    {Error, NewQueue} ->
      {reply, {error, Error}, State#{queue => NewQueue}};
    NewQueue ->
      {reply, ok, State#{queue => NewQueue}}
  end.

-spec handle_cast({insert, [timed_queue:value()]}, state())
                 -> {noreply, state()};
                 ({delete, timed_queue:key()}, state())
                 -> {noreply, state()};
                 (sync, state())
                 -> {noreply, state()}.
handle_cast({insert, Values}, State) ->
  NewState = insert_values(Values, State),
  {noreply, NewState};
handle_cast({delete, ReservationKey}, State = #{queue := Queue}) ->
  NewQueue = timed_queue:delete(ReservationKey, Queue),
  {noreply, State#{queue => NewQueue}};
handle_cast(sync, State) ->
  NewState = case should_sync(State) of
               {false, _}          ->
                 State;
               {true, CurrentTime} ->
                 State1 = sync_and_handle_result(State),
                 State1#{last_synced => CurrentTime}
             end,
  {noreply, NewState}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%% Internal functions

-spec reserve_and_reply(pos_integer(), state())
                       -> {reply, reservation(), state()}.
reserve_and_reply(ReservationTime, State = #{queue := Queue}) ->
  case timed_queue:reserve(ReservationTime, Queue) of
    {ReservationKey, Value, NewQueue} ->
      {reply, {ReservationKey, Value}, State#{queue => NewQueue}};
    {queue_empty, _Queue} ->
      maybe_send_sync(State),
      {reply, queue_empty, State}
  end.

-spec maybe_send_sync(state()) -> ok.
maybe_send_sync(State = #{ queue_name := QueueName
                         , sync_fn := _
                         , sync_interval := _}) ->
  case should_sync(State) of
    {true, _} -> sync(QueueName);
    _         -> ok
  end;
maybe_send_sync(_State) ->
  ok.

-spec should_sync(state()) -> {true | false, non_neg_integer()}.
should_sync(State = #{ sync_fn := _
                     , sync_interval := SyncInterval}) ->
  CurrentTime = erlang:system_time(second),
  LastSynced = maps:get(last_synced, State, 0), % never synced by default
  SyncRequired = CurrentTime > LastSynced + SyncInterval,
  {SyncRequired, CurrentTime};
should_sync(_State) ->
  {false, erlang:system_time(second)}.

-spec insert_values(list(), state()) -> state().
insert_values(Values, State = #{queue := Queue}) ->
  NewQueue = lists:foldl(fun timed_queue:insert/2, Queue, Values),
  State#{queue => NewQueue}.

prolongate_and_reply(ReservationKey, ReservationTime, State = #{queue := Queue}) ->
  case timed_queue:prolongate(ReservationKey, ReservationTime, Queue) of
    {NewKey, Value, NewQueue} ->
      {reply, {NewKey, Value}, State#{queue => NewQueue}};
    Error ->
      {reply, Error, State}
  end.

-spec sync_and_handle_result(state()) -> state().
sync_and_handle_result(State = #{queue_name := QueueName, sync_fn := {M, F, Args}}) ->
  try
    case apply(M, F, Args) of
      {ok, Values} when is_list(Values) ->
        logger:debug("Queue ~p: got values from sync function, values=~p~n",
                     [QueueName, Values]),
        insert_values(Values, State);
      {error, Reason} ->
        logger:error("Queue ~p: cannot sync values, reason=~p~n", [QueueName, Reason]),
        State
    end
  catch
    C:R:S ->
      logger:critical(
        "Queue ~p: error occured when sync queue, error=~p, reason=~p, stacktrace=~p~n",
        [QueueName, C, R, S]),
      State
  end.
