# Erlang/OTP Timed Queue

The `timed_queue` is the OTP library that provides implementation of a timed/delayed queue.
A client can insert, reserve, prolongate, release, and delete messages. As soon as a message
is reserved for a supplied period of time it becomes unavailable for other clients. After the
timeout the message becomes available again unless its reservation is prolongated or the
message is deleted from the queue. Additional features are a message values uniqueness check
and a limitation of number of reservations.

## Overview

The library contains two modules:

* `timed_queue`, which implements the queue functionality
* `timed_queue_server`, that implements a `gen_server` behaviour and provides an API
to the queue functionality

### Features

* provides the queue functionality and a queue server implementation
* the implementation guarantees unique reservation keys
* supports a values uniqueness (using Erlang's `sets`) on demand
* supports a reservations count limit on demand
* nanosecond resolution (if supported by OS) of reservation time

### Quick example

```erl
1> Queue0 = timed_queue:new().
{timed_queue,{0,nil},undefined,false,unlimited}
2> Queue1 = timed_queue:insert(the_value, Queue0).
{timed_queue,{1,
              {1546881227360583000,{entry,the_value,0},nil,nil}},
             undefined,false,unlimited}
%% reserve a message for 10 seconds
3> {_ReservationKey, the_value, Queue2} = timed_queue:reserve(10000, Queue1).
{1546881538637022000,the_value,
 {timed_queue,{1,
               {1546881538637022000,{entry,the_value,1},nil,nil}},
              undefined,false,unlimited}}
4> {queue_empty, Queue3} = timed_queue:reserve(10000, Queue2).
{queue_empty,{timed_queue,{1,
                           {1546881538637022000,{entry,the_value,1},nil,nil}},
                          undefined,false,unlimited}}
%% Wait for a message to be released automatically
5> timer:sleep(10000).
ok
6> {ReservationKey, the_value, Queue4} = timed_queue:reserve(10000, Queue3).
{1546881607447263000,the_value,
 {timed_queue,{1,
               {1546881607447263000,{entry,the_value,2},nil,nil}},
              undefined,false,unlimited}}
%% Delete the message
7> Queue5 = timed_queue:delete(ReservationKey, Queue4).
{timed_queue,{0,nil},undefined,false,unlimited}
```

## The `timed_queue` module

This module implements a timed queue functionality. All functions, except `new/0` and
`new/1` accept a `timed_queue` record as a last parameter.

A queue can be configured to ensure that message values are unique and to limit a number
of times a message can be reserved. Configuration is defined as:

```erl
-type config() :: #{ unique => boolean()
                   , max_reservations => pos_integer()
                   }.
```

Options are:

* `unique` - boolean flag. If it is set to `true` then the queue implementation ensures
message values uniqueness across the queue. Defaults to `false`.
* `max_reservations` - positive integer. If the parameter is set then the queue
implementation ensures that a message will not be reserved more times than provided value.
If it is not set unlimited number of reservations is allowed. Not set by default.

The `timed_queue` module's functions works with a `timed_queue` record and return a new
record to a caller. The timed queue implementation, provided by the module, exports the
following functions:

```erl
-type queue() :: #timed_queue{}.

%% Create a new queue
-spec new() -> queue() | no_return().
-spec new(config()) -> queue() | no_return().

%% Insert a value into a queue
-spec insert(value(), queue()) -> queue().
%% Where value() is
-type value() :: any().

%% Reserve a value on a queue
-spec reserve(reservation_time(), queue()) -> {key(), value(), queue()} |
                                              {queue_empty, queue()}.
%% Where reservation_time() is a positive integer, timeout in milliseconds
-type reservation_time() :: pos_integer().

%% Prolongate a reservation unless it is expired already
-spec prolongate(key(), reservation_time(), queue())
                -> {key(), value(), queue()} | {key_error(), queue()}.
%% Where key() and key_errors() are
-type key() :: pos_integer().
-type key_error() :: key_not_exists | reservation_expired.

%% Release a reserved message
-spec release(key(), queue()) -> queue() | {key_error(), queue()}.

%% Delete a reserved message
-spec delete(key(), queue()) -> queue().
```

## The `timed_queue_server` module

This module implements `gen_server` behaviour and hides the timed queue implementation
from a client as well as provides more extensive configuration. Because it implements
the `gen_server` the module can be managed as a part of an OTP application's supervision
tree.

#### Server configuration

The configuration of the server is defined as following

```erl
-type config() :: #{ queue_name := atom()
                   , reservation_time := pos_integer()
                   , queue_config => timed_queue:config()
                   , sync_fn => sync_fn()
                   , sync_interval => non_neg_integer()
                   , values => [any()]
                   }.
-type sync_fn() :: {module(), atom(), [any()]}.
```

Options:

* `queue_name` - required, an atom that a new server will be registered with. Later
a client will use the atom as a name of a queue it wants to access
* `reservation_time` - required, default reservation time in milliseconds
* `queue_config` - optional, a `timed_queue:config()` map that will be passed to the
`timed_queue:new/1` if defined. Otherwise a queue will be created with the
`timed_queue:new/0` function
* `sync_fn` - optional, a 3-tuple with a module, a function name and a list of arguments
to be called if a queue is empty to populate it with new values. Does not defined by
default
* `sync_interval` - optional, but is required if `sync_fn` is set. Interval in
milliseconds between two sequential calls of `sync_fn`
* `values` - optional, a list of values to be inserted into a queue during a server
initialization process

The API functions are quite similar to `timed_queue`'s, but they always receive an
atom that represents queue name (the `queue_name` configuration parameter) as a
first argument, instead of a `timed_queue` record as a last. There are two additional
functions:

* `reserve/1`, which is shortcut to `reserve/2` with the default `reservation_time`
configuration parameter
* `prolongate/2`, which is shortcut to `prolongate/3` with the default `reservation_time`
configuration parameter used as a prolongation time

#### Start and stop a server

To start a server as a part of an application's supervision tree

```erl
-module(app_sup).

-behaviour(supervisor).

-export([ start_link/1
        , init/1
        ]).

start_link(Config) ->
  supervisor:start_link(app_sup, Config).

init(_Config = #{queue_name := QName, reservation_time := RTime}) ->
  Flags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs =
    [ #{ id => QName
       , start => { timed_queue_server
                  , start_link
                  , [#{ queue_name => QName
                      , reservation_time => RTime
                      }]
                  }
       , restart => permanent
       , shutdown => 10000
       , type => worker
       , modules => [timed_queue_server]
       }
    ],
  {ok, {Flags, ChildSpecs}}.
```

To start and stop a server "manually"

```erl
Config = #{queue_name => my_queue, reservation_time => 5000},
{ok, Pid} = timed_queue_server:start_link(Config).

do_the_work().

ok = timed_queue_server:stop(my_queue).
```

## Implementation details

To do.

## ToDo list

* Enable Trevi-CI
* Add an option to allow deletion of an expired reservation unless related message is
not reserved yet
* Describe implementation details
* Add more improvements to the documentation
* Add EDoc

