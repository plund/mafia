-include("mafia_atoms.hrl").

-define(SERVER, ?MODULE).
-define(WEBPORT, 50666).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").
-define(TURQUOISE_HEX, "DFFFDF").
-define(BG_TURQUOISE, "bgcolor=\"#" ++ ?TURQUOISE_HEX ++ "\"").

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
-type day_night() :: ?day | ?night | ?game_ended.
-type mfargs() :: {atom(), atom(), list()}.

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
        {key :: ?undefined | {thread_id(), page_num()},
         message_ids = [] :: [msg_id()],
         thread_id :: ?undefined | thread_id(),
         complete = false:: boolean()
        }).

-record(message,
        {msg_id :: ?undefined | msg_id(),
         thread_id :: ?undefined | thread_id(),
         page_num :: ?undefined | page_num(),
         user_name :: ?undefined | user(),
         time :: ?undefined | seconds1970(),
         message :: ?undefined | message()
         %% is_deleted = false :: boolean()  %% Delete marking
        }).

-record(phase,
        {num :: ?undefined | integer(),
         don :: ?undefined | day_night()
        }).

-record(dl,
        {phase :: ?undefined | #phase{},
         time :: ?undefined | seconds1970()
        }).

-record(vote,
        {time :: ?undefined | seconds1970(),
         id :: ?undefined | msg_id(),
         page :: ?undefined | page_num(),
         vote :: ?undefined | player(),
         raw :: ?undefined | binary(),
         valid :: ?undefined | boolean()
        }).

-record(death,
        {player :: ?undefined | player(),
         is_end :: ?undefined | boolean(),
         phase :: ?undefined | #phase{},
         comment :: ?undefined | binary(),
         msg_id :: ?undefined | msg_id(),
         time :: ?undefined | seconds1970(),
         is_deleted = false :: boolean()
        }).

-record(replacement,
        {new_player :: ?undefined | player(),
         replaced_player :: ?undefined | player(),
         phase :: ?undefined | #phase{},
         msg_id :: ?undefined | msg_id(),
         time :: ?undefined | seconds1970()
        }).

-record(mafia_day,
        {key :: ?undefined | {thread_id(), day_num()},
         thread_id :: ?undefined | thread_id(),
         day :: ?undefined | day_num(),
         votes = [] :: [{player(), [#vote{}]}],
         end_votes = [] :: [player()],
         players_rem = [] :: [player()],
         player_deaths = [] :: [#death{} | #replacement{}] %% Dead players in mid day
        }).

-record(mafia_game,
        {key :: ?undefined | thread_id(),
         name :: ?undefined | binary(),
         day_hours = 48 :: integer(),    %% Typically 48
         night_hours = 24 :: integer(),  %% Typically 24
         time_zone = 0 :: integer(),     %% (EST=-5, UK = 0, CET=1)
         day1_dl_time :: ?undefined | datetime(), %% Game TZ local time
         is_init_dst :: ?undefined | boolean(),   %% true = DST, false = normal time
         dst_changes = [] :: [{datetime(), ToDst::boolean()}],
         deadlines = [] :: [#dl{}],
         gms = [] :: [user()],
         players_orig = [] :: [player()],
         players_rem = [] :: [player()],
         game_num :: ?undefined | integer(),
         player_deaths = [] :: [#death{} | #replacement{}],
         page_to_read :: ?undefined | integer(), %% set_kv(page_to_read, 1),
         game_end :: ?undefined | {seconds1970(), msg_id()},
         last_msg_id :: ?undefined | msg_id(),
         last_msg_time :: ?undefined | seconds1970()
        }).

-record(user,
        {name_upper :: ?undefined | user(),
         name :: ?undefined | user(),
         aliases = [] :: [alias()],
         verification_status :: ?undefined | ?verified | ?unverified
        }).

-record(stat,
        {key :: {player(), ThId::integer()}
              | {player(),
                 ThId::integer(),
                 ?game_ended | {integer(), day_night()}}
              | term(),
         msg_ids :: '_' | [msg_id()],
         num_chars :: '_' | integer(),
         num_words :: '_' | integer(),
         num_postings :: '_' | integer()
        }).

-record(prstat,
        {key :: {player(), ThId::integer()}
              | {player(),
                 ThId::integer(),
                 ?game_ended | {integer(), day_night()}}
              | ?undefined,
         msg_ids :: [msg_id()],
         num_chars :: integer(),
         num_words :: integer(),
         num_postings :: integer(),
         words_per_post :: float()
        }).

-record(cnt,
        {key :: {CounterName :: binary()}
              | {CounterName :: binary(), Day1970 :: integer()}
              | term(),
         value :: '_' | integer()
        }).

-record(cmd,
        { time :: ?undefined | seconds1970(),
          msg_id :: ?undefined |  msg_id(),
          mfa :: ?undefined | mfargs()
        }).
