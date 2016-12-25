-define(true, true).
-define(false, false).
-define(undefined, undefined).

-define(kv_store, kv_store).
-define(day  , day__).
-define(night, night).
-define(phase, phase).
-define(game_ended, game_ended).
-define(total_stats, total_stats).

-define(verified, verified).
-define(unverified, unverified).

-define(dev, dev).
-define(utc, utc).
-define(gmt, gmt).
-define(zulu, zulu).
-define(user, user).
-define(game, game).
-define(time, time).
-define(mode, mode).
-define(period, period).
-define(html, html).
-define(text, text).

%% KV keys
-define(thread_id, thread_id).
-define(game_key, game_key).
-define(page_to_read, page_to_read).
-define(timezone_user, timezone_user).
-define(dst_user, dst_user).
-define(timezone_game, timezone_game).
-define(dst_game, dst_game).
-define(print_time, print_time).

-define(GSECS_1970, 62167219200).
-define(GDAYS_1970, 719528).
%% calendar:date_to_gregorian_days({1970,1,1}) -> 719528.

-define(MinuteSecs, 60).
-define(HourSecs, 3600).
-define(DaySecs, (24*3600)).

-define(MAX_GM_DL_MINS, 15).

-define(M24ThId, 1404320).
-define(M25ThId, 1420289).
%% -define(DefThId, ?M25ThId).
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-define(Unvote, "Unvote").
-define(END, "End").
-define(UNEND, "Unend").
-define(NoLynch, "No-Lynch").
-define(Extra, [?END, ?UNEND, ?NoLynch]).
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
-define(M26_GMs, ["Jamiet99uk", "Chaqa"]).
-define(M26_players,
        ["Ezio", "peterlund", "brainbomb", "bo_sox48", "DemonOverlord", "ND",
         "Tom Bombadil",
         %%"Tom Bomabadil",
         "Floodgates", "xorxes", "MoscowFleet",
         "Balki Bartokomous",
         %%"Balkibartonomous",
         "VashtaNeurotic", "Fluminator", "Ragingike297",
         "Maniac", "Kakarroto", "DeathLlama8", "rdrivera2005", "arj11", "ghug",
         "teacon7", "Hellenic Riot", "Glen_Alexander", "CaptainMeme",
         "Reedeer1", "Vecna"]).
-define(M26_Subs, ["WardenDresden", "Dargorygel", "Zorclex", "DemonRHK"]).

-type year()   :: non_neg_integer().
-type month()  :: 1..12.
-type day()    :: 1..31.
-type hour()   :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type millisecs() :: integer().

-type time() :: {hour(), minute(), second()}.
-type date() :: {year(), month(), day()}.
-type datetime() :: {date(), time()}.

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
-type phase() :: ?game_ended | {integer(), day_night()}.
-type deadline() :: {integer(), day_night(), seconds1970()}.

%% simple macros
-define(a2l(A), atom_to_list(A)).

-define(b2l(B), binary_to_list(B)).

-define(i2l(I), integer_to_list(I)).

-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2a(L), list_to_atom(L)).
-define(l2u(L), string:to_upper(L)).
-define(lrev(L), lists:reverse(L)).

%% combined macros
-define(l2ub(L), ?l2b(?l2u(L))).
-define(b2ul(B), ?l2u(?b2l(B))).
-define(b2ub(B), ?l2b(?b2ul(B))).

-define(dbg(Term),
        io:format("~s DBG ~999p\n",
                  [mafia_print:print_time(current_time, short),
                   Term])).
-define(dbg(Time, Term),
        io:format("~s DBG ~999p\n",
                  [mafia_print:print_time(Time, short),
                   Term])).
-define(dbg_str(Str),
        io:format("~s DBG ~s\n",
                  [mafia_print:print_time(current_time, short),
                   Str])).
-define(man(Time, Cmd),
        io:format("~s MANUAL ~999p\n",
                  [mafia_print:print_time(Time, short),
                   Cmd])).

-define(HTML_TAB_START(Title, TabAttrStr),
 "<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <title>" ++ Title ++ "</title>
  </head>
  <body bgcolor=\"#cfffaf\">
    <center>
      <h3>" ++ Title ++ "</h3>
      <table" ++ TabAttrStr ++ ">").

-define(HTML_TAB_END, "
      </table>
    </center>
  </body>
</html>").

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
         game_end :: ?undefined | {seconds1970(), msg_id()}
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

-record(cmd,
        { time :: seconds1970(),
          msg_id :: msg_id(),
          mfa :: mfa()
        }).
