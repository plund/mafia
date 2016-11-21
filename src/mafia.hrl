-define(kv_store, kv_store).
-define(day  , day__).
-define(night, night).
-define(verified, verified).
-define(unverified, unverified).

-define(GSECS_1970, 62167219200).
-define(GDAYS_1970, 719528).
%% calendar:date_to_gregorian_days({1970,1,1}) -> 719528.

-define(HourSecs, 3600).
-define(DaySecs, 24*3600).

-define(DefThId, 1404320).
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-define(Extra, ["End", "No-Lynch"]).
-define(M24_GMs, ["DemonRHK", "MoscowFleet"]).
-define(M24_players,
        ["Bo_sox48", "CaptainMeme", "Chaqa", "dargorygel", "Ezio", "Floodgates",
         "Ghug", "Glen_Alexander", "Goldfinger0303", "Guak", "Hellenic Riot",
         "Ikaneko", "Jamiet99uk", "Krellin", "Maniac", "Peterlund",
         "Rdrivera2005", "Teacon7", "VashtaNeurotic", "Vecna", "Xorxes",
         "Yoyoyozo", "Zorclex"
        ]).
-define(Aliases, [{"CaptainMeme", ["Meme"]},
                  {"Hellenic Riot", ["HR", "H.R."]},
                  {"Brainbomb", ["BB"]},
                  {"No-Lynch", ["No Lynch"]}
                 ]).

-type thread_id() :: integer().
-type page_num() :: integer().
-type msg_id() :: integer().
-type day_num() :: integer().
-type user() :: binary().
-type player() :: user().
-type seconds1970() :: integer().
-type message() :: binary().
-type day_night() :: ?day | ?night.
-type phase() :: {integer(), day_night()}.
-type deadline() :: {integer(), day_night(), seconds1970()}.

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
         complete :: boolean()
        }).

-record(message,
        {msg_id :: msg_id(),
         thread_id :: thread_id(),
         page_num :: page_num(),
         user_name :: user(),
         time :: seconds1970(),
         message :: message()
        }).

-record(vote,
        {time :: seconds1970(),
         id :: msg_id(),
         page :: page_num(),
         vote :: player(),
         raw :: binary(),
         valid :: boolean()
        }).

-record(mafia_day,
        {key :: {thread_id(), day_num()},
         thread_id :: thread_id(),
         day :: day_num(),
         votes :: [{player(), [#vote{}]}],
         complete :: boolean()
        }).

-record(mafia_game,
        {key :: thread_id(),
         name :: binary(),
         day_hours,    %% DayHours,   % 48
         night_hours,  %% NightHours, % 24
         time_zone,    %% (EST=-5, UK = 0, CET=1)
         day1_dl_time, %% D1DeadLine, % Day 1 deadline in local time
         is_init_dst,  %% IsInitDst,  % true = DST, false = normal time
         dst_changes,  %% DstChanges  % [{date_time(), IsDst::boolean()}]
         deadlines = [] :: [{integer(), day_night(), seconds1970()}],
         gms,          %% set_kv(mafia_GMs, ["DemonRHK", "MoscowFleet"]),
         players_orig :: [player()], %% set_kv(mafia_players, ?M24_players),
         players_rem  :: [player()],
         game_num = 24,
         players_dead :: [{player(), {day|night, integer()}}],
         page_to_read, %% set_kv(page_to_read, 1),
         complete :: boolean()
        }).

-record(user,
        {name_upper :: user(),
         name :: user(),
         verification_status :: ?verified | ?unverified
        }).
