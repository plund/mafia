-define(kv_store, kv_store).
-define(day  , day__).
-define(night, night).
-define(verified, verified).
-define(unverified, unverified).

-define(GSECS_1970, 62167219200).
-define(GDAYS_1970, 719528).
%% calendar:date_to_gregorian_days({1970,1,1}) -> 719528.

-define(MinuteSecs, 60).
-define(HourSecs, 3600).
-define(DaySecs, (24*3600)).

-define(M24ThId, 1404320).
-define(M25ThId, 1420289).
-define(DefThId, ?M25ThId).
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-define(Unvote, "Unvote").
-define(Extra, ["End", "No-Lynch"]).
-define(M24_GMs, ["DemonRHK", "MoscowFleet"]).
-define(M24_players,
        ["bo_sox48", "captainmeme", "Chaqa", "dargorygel", "Ezio", "Floodgates",
         "ghug", "Glen_Alexander", "goldfinger0303", "guak", "Hellenic Riot",
         "Ikaneko", "Jamiet99uk", "krellin", "Maniac", "peterlund",
         "rdrivera2005", "teacon7", "VashtaNeurotic", "Vecna", "xorxes",
         "Yoyoyozo", "zorclex"
        ]).

-define(M25_GMs, ["VashtaNeurotic", "captainmeme"]).
-define(M25_players,
        ["brainbomb", "DemonRHK", "ghug", "Ezio", "Floodgates", "MoscowFleet",
         "RagingIke297", "DemonOverlord", "guak", "krellin", "Kakarroto", "ND",
         "teacon7", "Chaqa", "WardenDresden", "xorxes", "Vecna", "rdrivera2005",
         "Glen_Alexander", "Jamiet99uk", "Maniac", "dargorygel", "zorclex"]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type time() :: {hour(), minute(), second()}.

-type thread_id() :: integer().
-type page_num() :: integer().
-type msg_id() :: integer().
-type day_num() :: integer().
-type alias() :: binary().
-type user() :: binary().
-type player() :: user().
-type seconds1970() :: integer().
-type message() :: binary().
-type day_night() :: ?day | ?night.
-type phase() :: {integer(), day_night()}.
-type deadline() :: {integer(), day_night(), seconds1970()}.

-record(s,
        {page :: page_num(),  %% either page num to get and when got the
         %%                      actual page num
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

-record(death,
        {player :: player(),
         is_end :: boolean(),
         phase :: phase(),
         comment :: binary(),
         msg_id :: msg_id(),
         time :: seconds1970(),
         is_deleted = false :: boolean()
        }).

-record(mafia_day,
        {key :: {thread_id(), day_num()},
         thread_id :: thread_id(),
         day :: day_num(),
         votes :: [{player(), [#vote{}]}],
         end_votes :: [player()],
         players_rem :: [player()],
         player_deaths :: [#death{}] %% Dead players in mid day
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
         deadlines = [] :: [deadline()],
         gms,          %% set_kv(mafia_GMs, ["DemonRHK", "MoscowFleet"]),
         players_orig :: [player()], %% set_kv(mafia_players, ?M24_players),
         players_rem  :: [player()],
         game_num = 24,
         player_deaths :: [#death{}],
         page_to_read, %% set_kv(page_to_read, 1),
         complete :: boolean()
        }).

-record(user,
        {name_upper :: user(),
         name :: user(),
         aliases :: [alias()],
         verification_status :: ?verified | ?unverified
        }).

-record(stat,
        {key :: {player(), ThId::integer()}
              | {player(), ThId::integer(), phase()},
         msg_ids :: [msg_id()],
         num_chars :: integer(),
         num_words :: integer(),
         num_postings :: integer()
        }).
