-include("mafia_atoms.hrl").
-include("mafia_macros.hrl").

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
-type dst_zone() :: ?eu | ?usa | ?australia | ?new_zeeland | ?none.
-type dst_direction() :: ?to_dst | ?to_normal.
-type dst_change() :: {datetime(), dst_direction()}.

-type game_num() :: integer().
-type thread_id() :: integer().
-type page_num() :: integer().
-type msg_id() :: integer().
-type day_num() :: integer().
-type alias() :: binary().
-type user() :: binary().
-type player() :: user().
-type seconds1970() :: integer().
-type greg_secs() :: integer().
-type message() :: binary().
-type day_night() :: ?day | ?night | ?game_start | ?game_ended.
-type mfargs() :: {atom(), atom(), list()}.

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
        {key :: ?undefined | {game_num(), day_num()},
         thread_id :: ?undefined | thread_id(),
         day :: ?undefined | day_num(),
         votes = [] :: [{player(), [#vote{}]}],
         end_votes = [] :: [player()],
         players_rem = [] :: [player()],
         %% Deaths and replacements in mid day
         player_deaths = [] :: [#death{} | #replacement{}]
        }).

-record(mafia_game,
        {game_num :: ?undefined | integer(),
         thread_id :: '$1' | ?undefined | thread_id(),
         name :: ?undefined | binary(),
         day_hours = 48 :: integer(),
         night_hours = 24 :: integer(),
         time_zone :: ?undefined | integer(), %% (EST=-5, UK = 0, CET=1)
         start_time :: ?undefined | datetime(),
         dst_zone :: ?undefined | dst_zone(),
         dst_changes = [] :: [dst_change()],
         deadlines = [] :: [#dl{}],
         gms = [] :: [user()],
         players_orig = [] :: [player()],
         players_rem = [] :: [player()],
         player_deaths = [] :: [#death{} | #replacement{}],
         page_to_read = 1 :: integer(),
         game_end :: ?undefined | {seconds1970(), msg_id()},
         last_msg_id :: ?undefined | msg_id(),
         last_msg_time :: ?undefined | seconds1970()
        }).

-record(user,
        {name_upper :: ?undefined | user(),
         name :: ?undefined | user(),
         aliases = [] :: [alias()],
         verification_status :: ?undefined | ?verified | ?unverified,
         pw_hash :: ?undefined | integer()
        }).

-record(stat,
        {key :: {player(), game_num()}
              | {player(), game_num(),
                 ?game_ended | {day_num(), day_night()}}
              | term(),
         msg_ids :: [msg_id()],
         num_chars :: integer(),
         num_words :: integer(),
         num_postings :: integer()
        }).

-record(prstat,
        {key :: {player(), game_num()}
              | {player(), game_num(),
                 ?game_ended | {day_num(), day_night()}}
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
         value :: integer()
        }).

-record(cmd,
        { time :: ?undefined | seconds1970(),
          msg_id :: ?undefined |  msg_id(),
          mfa :: ?undefined | mfargs()
        }).
