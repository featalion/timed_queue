-record(timed_queue,
        { tree :: gb_trees:tree()
        , set = undefined :: sets:set() | undefined
        , unique = false :: boolean()
        , max_reservations = unlimited :: pos_integer() | unlimited
        }).
