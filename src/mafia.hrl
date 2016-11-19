-define(kv_store, kv_store).
-define(DefThId, 1404320).
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-type thread_id() :: integer().
-type page_num() :: integer().
-type msg_id() :: integer().

-record(s,
        {page :: page_num(),  %% either page num to get and when got the actual page num
         is_last_page :: boolean(),
         page_num_last_read :: page_num(),
         page_total_last_read :: page_num(),
         thread_id :: thread_id(),
         url :: string(),
         body :: string()
        }).

-record(kv_store,
        {key,
         value
        }).

-record(page_rec,
        {key :: {thread_id(), page_num()},
         message_ids :: [msg_id()],
         thread_id :: thread_id(),
         complete = false :: boolean()
        }).

-record(message,
        {msg_id :: msg_id(),
         thread_id :: thread_id(),
         page_num :: page_num(),
         user_name,
         time,
         message
        }).
