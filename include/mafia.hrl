-define(true, true).
-define(false, false).
-define(undefined, undefined).
-define(ok, ok).
-define(error, error).
-define(none, none).
-define(dummy, dummy).
-define(new_page, new_page).
-define(add_id, add_id).
-define(unchanged, unchanged).

-define(sort, sort).
-define(normal, normal).
-define(words, words).
-define(words_per_post, words_per_post).

-define(dbuser_ok, dbuser_ok).
-define(dbuser_unver, dbuser_unver).
-define(dbuser_wrong_case, dbuser_wrong_case).
-define(dbuser_none, dbuser_none).

-define(kv_store, kv_store).
-define(day, day).
-define(night, night).
-define(phase, phase).
-define(game_ended, game_ended).
-define(current, current).
-define(total_stats, total_stats).
-define(global, global).
-define(bytes, bytes).

-define(verified, verified).
-define(unverified, unverified).

-define(dev, dev).
-define(utc, utc).
-define(gmt, gmt).
-define(zulu, zulu).
-define(user, user).
-define(game, game).
-define(mode, mode).
-define(period, period).
-define(html, html).
-define(text, text).
-define(t_mode, t_mode).
-define(file_suffix, file_suffix).
-define(short, short).
-define(long, long).
-define(extensive, extensive).
%% -define(time, time).
-define(use_time, use_time).
-define(time_zone, time_zone).
-define(dst, dst).

-define(sorted, sorted).
-define(seconds, seconds).
-define(earlier, earlier).
-define(later, later).

-define(gm, gm).
-define(player, player).
-define(dead_player, dead_player).
-define(other, other).
-define(console, console).

-define(start_polling, start_polling).
-define(stop_polling, stop_polling).
-define(stopped, stopped).
-define(deadline, deadline).

%% KV keys
-define(console_tz, console_tz).
-define(dst_game, dst_game).
-define(dst_user, dst_user).
-define(game_key, game_key).
-define(mod_msg, mod_msg).
-define(page_to_read, page_to_read).
-define(reg_threads, reg_threads).
-define(thread_id, thread_id).
-define(time_offset, time_offset). %% integer in seconds
-define(timer_minutes, timer_minutes).
-define(timezone_game, timezone_game).
-define(timezone_user, timezone_user).
%% positive offset means that simulated time is in the past.

-define(SERVER, ?MODULE).
-define(WEBPORT, 50666).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").

-define(GSECS_1970, 62167219200).
-define(GDAYS_1970, 719528).
%% calendar:date_to_gregorian_days({1970,1,1}) -> 719528.

-define(MinuteSecs, 60).
-define(HourSecs, 3600).
-define(DaySecs, (24*3600)).

-define(MAX_GM_DL_MINS, 20).

-define(BotUrl, "http://mafia.peterlund.se/").

-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-define(Unvote, "Unvote").
-define(END, "END").
-define(UNEND, "UNEND").
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
        ["arj11", "Balki Bartokomous",
         "bo_sox48", "brainbomb",
         "DeathLlama8", "DemonOverlord", "Ezio", "Floodgates", "DemonRHK",
         "ghug", "Glen_Alexander", "Hellenic Riot", "Kakarroto", "Maniac",
         "MoscowFleet", "ND", "peterlund", "RagingIke297",
         "rdrivera2005",
         "teacon7", "Tom Bombadil", "VashtaNeurotic", "Vecna",
         "dargorygel", "WardenDresden",
         "xorxes"]).
-define(M26_Subs, ["WardenDresden", "dargorygel", "zorclex", "DemonRHK"]).

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

-define(inc_cnt(CntName), mafia_lib:inc_cnt(CntName)).
-define(inc_cnt(CntName, Inc), mafia_lib:inc_cnt(CntName, Inc)).
-define(inc_cnt(CntName, Args, Inc), mafia_lib:inc_cnt(CntName, Args, Inc)).
-define(thid(Id), mafia_lib:thid(Id)).
-define(set(K, V), mafia_db:set(K, V)).
-define(getv(K), mafia_db:getv(K)).

%% -define(dwrite(Obj), mafia_lib:dwrite(gen, Obj)).
-define(dwrite_kv(Obj), mafia_lib:dwrite(kv, Obj)).
-define(dwrite_msg(Obj), mafia_lib:dwrite(msg, Obj)).
-define(dwrite_page(Obj), mafia_lib:dwrite(page, Obj)).
-define(dwrite_stat(Obj), mafia_lib:dwrite(stat, Obj)).
-define(dwrite_user(Obj), mafia_lib:dwrite(user, Obj)).
-define(dwrite_day(Obj), mafia_lib:dwrite(day, Obj)).
-define(dwrite_game(Obj), mafia_lib:dwrite(game, Obj)).

-define(rmess(MsgId), mafia_lib:rmess(MsgId)).
-define(rpage(ThId, Page), mafia_lib:rpage(ThId, Page)).
-define(rpage(Key), mafia_lib:rpage(Key)).
-define(ruser(MsgId), mafia_lib:ruser(MsgId)).
-define(ruserUB(MsgId), mafia_lib:ruserUB(MsgId)).

-define(rgame(ThId), mafia_lib:rgame(ThId)).
-define(rday(GK, Phase), mafia_lib:rday(GK, Phase)).

-define(dbg(Term),
        io:format("~s DBG ~p ~999p\n",
                  [mafia_print:print_time(?console),
                   ?MODULE, Term])).
-define(dbg(Time, Term),
        io:format("~s DBG ~p ~999p\n",
                  [mafia_print:print_time(?console, Time),
                   ?MODULE, Term])).
-define(dbg_str(Str),
        io:format("~s DBG ~s\n",
                  [mafia_print:print_time(?console),
                   Str])).
-define(man(Time, Cmd),
        io:format("~s MANUAL ~999p\n",
                  [mafia_print:print_time(?console, Time),
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
      <table" ++ TabAttrStr ++ ">
      <tr><td align=\"center\"><h2>" ++ Title ++ "</h2></td></tr>").

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
         %% is_deleted = false :: boolean()  %% Delete marking
        }).

-record(phase,
        {num :: integer(),
         don :: day_night() | ?game_ended
        }).

-record(dl,
        {phase :: #phase{},
         time :: seconds1970()
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
         phase :: #phase{},
         comment :: binary(),
         msg_id :: msg_id(),
         time :: seconds1970(),
         is_deleted = false :: boolean()
        }).

-record(replacement,
        {new_player :: player(),
         replaced_player :: player(),
         phase :: #phase{},
         msg_id :: msg_id(),
         time :: seconds1970()
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
         day_hours :: integer(),     %% Typically 48
         night_hours :: integer(),   %% Typically 24
         time_zone :: integer(),     %% (EST=-5, UK = 0, CET=1)
         day1_dl_time :: datetime(), %% Game TZ local time
         is_init_dst :: boolean(),   %% true = DST, false = normal time
         dst_changes :: [{datetime(), ToDst::boolean()}],
         deadlines = [] :: [#dl{}],
         gms :: [user()],
         players_orig  :: [player()],
         players_rem   :: [player()],
         game_num      :: integer(),
         player_deaths :: [#death{} | #replacement{}],
         page_to_read :: integer(), %% set_kv(page_to_read, 1),
         game_end :: ?undefined | {seconds1970(), msg_id()},
         last_msg_time
        }).

-record(user,
        {name_upper :: user(),
         name :: user(),
         aliases :: [alias()],
         verification_status :: ?verified | ?unverified
        }).

-record(stat,
        {key :: {player(), ThId::integer()}
              | {player(),
                 ThId::integer(),
                 ?game_ended | {integer(), day_night()}},
         msg_ids :: [msg_id()],
         num_chars :: integer(),
         num_words :: integer(),
         num_postings :: integer()
        }).

-record(prstat,
        {key :: {player(), ThId::integer()}
              | {player(),
                 ThId::integer(),
                 ?game_ended | {integer(), day_night()}},
         msg_ids :: [msg_id()],
         num_chars :: integer(),
         num_words :: integer(),
         num_postings :: integer(),
         words_per_post :: float()
        }).

-record(cnt,
        {key   :: {CounterName :: binary()} |
                  {CounterName :: binary(), Day1970 :: integer()},
         value :: integer()
        }).

-record(cmd,
        { time :: seconds1970(),
          msg_id :: msg_id(),
          mfa :: mfa()
        }).
