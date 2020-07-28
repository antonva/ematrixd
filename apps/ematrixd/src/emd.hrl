-record(device, {id,
                 user,
                 access_token}).

-record(user, {id,
               name,
               password}).

-record(room, {id,
               domain,
               version,
               event_dag,
               event_depth}).
