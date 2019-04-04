-module(mafia_lib).

-export([dwrite/2,

         rmess/1,
         rpage/1,
         rpage/3,
         pages_for_thread/1,
         all_page_keys/0,
         all_page_keys/1,
         all_day_keys/1,
         all_keys/1,

         set_current_game/0,
         is_ongoing_game/1,

         rday/2,
         rgame/1,
         ruser/2,
         ruserUB/1,
         ruserUB/2,
         rmessI/1,

         is_alpha_num/1,
         remove_blockquotes/1,

         get_unicode_msg/1,
         escapes_to_unicode/1,

         set_new_password/2,
         check_password/3,
         copy_password_between_users/4,

         prev_msg/1,

         get_path/1,
         get_arg/1,
         my_string_substr/3,
         alpha_sort/1,
         to_bin_sort/1,

         split_into_groups/2,

         is_url_char/1,
         get_url/1,
         split_on_url_boundary/1,
         strip_br_white_rev/1,
         strip_br_white_fwd/1,
         get_url_begin/1,

         bgcolor/1,
         split_on_first_char/2,
         replace_p/3,

         re_matches/2,
         merge_intervals/1,

         all_msgids/2,
         all_msgids/3,

         iterate_all_msg_ids/2,
         iterate_all_msg_ids/3,
         iter_msgids/4,

         inc_cnt/1,
         inc_cnt/2,
         inc_cnt/3,
         print_all_cnts/0,
         print_all_cnts/1,
         save_cnts_to_file/0,

         dbg/2,
         dbg/3,
         dbg_str/1,
         man/2,
         stacktrace/0
        ]).

-include("mafia.hrl").

dirty_write(Obj) ->
    dirty_update_counter(cnt, {dirty_write, ?global}, 1),
    mnesia:dirty_write(Obj).

dirty_update_counter(cnt, Key, Inc) ->
    mnesia:dirty_update_counter(cnt, {dirty_update_counter, ?global}, 1),
    mnesia:dirty_update_counter(cnt, Key, Inc).

dwrite(page, Obj) ->
    dirty_update_counter(cnt, {dirty_write, ?global, page}, 1),
    dwrite_page(Obj);
dwrite(day, Day) ->
    dirty_update_counter(cnt, {dirty_write, ?global, day}, 1),
    dirty_write(Day#mafia_day{players_rem = [], player_deaths = []});
dwrite(Tag, Obj) ->
    dirty_update_counter(cnt, {dirty_write, ?global, Tag}, 1),
    dirty_write(Obj).

rmess(MsgId = {_, _}) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rmessI(MsgId) of
        [] -> [];
        [M] when M#message.time > OffsetNow -> [];
        [M] -> [M]
    end.

rmessI(MsgId = {_, _}) ->
    mnesia:dirty_read(message, MsgId).

%% -----------------------------------------------------------------------------

rpage(Key = {_, _, Site}) ->
    case rpageI(Key) of
        [] -> [];
        [P] ->
            MsgIds2 = [MId || MId <- P#page_rec.message_ids,
                              [] /= ?rmess({MId, Site}) ],
            if MsgIds2 == [] -> [];
               true ->
                    NumMsgs = case Site of
                                  ?wd2 -> 20;
                                  _ -> 30
                              end,
                    Complete =
                        %% P#page_rec.complete andalso
                        %% MsgIds2 == P#page_rec.message_ids andalso
                        length(MsgIds2) >= NumMsgs,
                    [P#page_rec{message_ids = MsgIds2,
                                complete = Complete}]
            end
    end.

rpage(ThId, Page, Site) -> rpage({ThId, Page, Site}).

rpageI(Key = {_,_,_}) -> mnesia:dirty_read(page_rec, Key).

%% update page_rec only if more info is added
dwrite_page(P) ->
    case rpageI(P#page_rec.key) of
        [] ->
            dirty_write(P);
        [Db] ->
            NumDb = length(Db#page_rec.message_ids),
            NumNew = length(P#page_rec.message_ids),
            Db2 = if NumNew >= 30;
                     NumNew > NumDb ->
                          Db#page_rec{message_ids = P#page_rec.message_ids};
                     true -> Db
                  end,
            Db3 = if P#page_rec.complete, not Db2#page_rec.complete ->
                          Db2#page_rec{complete = true};
                     true -> Db2
                  end,
            dirty_write(Db3)
    end.

pages_for_thread(Key = {_ThId, _Site}) ->
    [P || {_, P, _} <- all_page_keys(Key)].

all_page_keys({ThId, Site}) ->
    [K || K = {N, _, S} <- all_page_keys(), N == ThId, S == Site].

all_page_keys() ->
    lists:sort([ K || K <- mnesia:dirty_all_keys(page_rec), [] /= rpage(K)]).

all_day_keys(GNum) -> [K || K = {GN, _} <- all_keys(mafia_day), GN == GNum].

all_keys(Tab = mafia_game) ->
    OrderF = fun(?undefined, ?undefined) -> ?true; % true is A =< B
                (?undefined, _) -> ?false;
                (_, ?undefined) -> ?true;
                (T1, T2) -> T1 =< T2
             end,
    SortF = fun(G1, G2) ->
                    OrderF(G1#mafia_game.start_time,
                           G2#mafia_game.start_time)
            end,
    [G#mafia_game.game_num || G <- lists:sort(SortF, ets:tab2list(Tab))];
all_keys(Tab) -> lists:sort(mnesia:dirty_all_keys(Tab)).


%% set default game for front page.
%% Order games this way:
%% 1. Oldest ongoing game, 2. oldest "upcoming" game. 3 last finished game
set_current_game() ->
    case sort_for_current(ets:tab2list(mafia_game)) of
        [CurGame | _] ->
            mafia_db:set(?game_key, CurGame#mafia_game.game_num);
        _ -> ok
    end.

is_ongoing_game(#mafia_game{thread_id = ?undefined}) -> ?false;
is_ongoing_game(#mafia_game{game_end = GE}) -> GE == ?undefined;
is_ongoing_game(GNum) when is_integer(GNum) ->
    is_ongoing_game(hd(?rgame(GNum))).

is_upcoming_game(#mafia_game{thread_id = Thid})
  when Thid /= ?undefined -> ?false;
is_upcoming_game(#mafia_game{game_end = GE})
  when GE /= ?undefined -> ?false;
is_upcoming_game(#mafia_game{}) -> ?true.

sort_for_current(Gs) ->
    Type =
        fun(G) ->
                case is_ongoing_game(G) of
                    ?true -> 1;
                    ?false ->
                        case is_upcoming_game(G) of
                            ?true -> 2;
                            ?false -> 3
                        end
                end
        end,
    TimeOrderF =
        fun(?undefined, ?undefined) -> ?true; % true is A =< B
           (?undefined, _) -> ?false;
           (_, ?undefined) -> ?true;
           (T1, T2) -> T1 =< T2
        end,
    SortF =
        fun(G1, G2) ->
                Type1 = Type(G1),
                Type2 = Type(G2),
                if Type1 < Type2 -> ?true;
                   Type1 > Type2 -> ?false;
                   Type1 < 3 ->
                        %% order on oldest start time
                        TimeOrderF(G1#mafia_game.start_time,
                                   G2#mafia_game.start_time);
                   Type1 == 3 ->
                        %% order on last finished game
                        element(1, G1#mafia_game.game_end)
                            >= element(1, G2#mafia_game.game_end)
                end
        end,
    lists:sort(SortF, Gs).

%% -----------------------------------------------------------------------------

%% -> #mafia_day{}
rday(GameNum, #phase{num = DayNum}) ->
    rday(GameNum, DayNum);
rday(GameNum, DayNum) when is_integer(GameNum) ->
    rday(?rgame(GameNum), DayNum);
rday([#mafia_game{} = G], DayNum) ->
    rday(G, DayNum);
rday(#mafia_game{} = G, DayNum) ->
    GameNum = G#mafia_game.game_num,
    ThId = G#mafia_game.thread_id,
    case mnesia:dirty_read(mafia_day, Key = {GameNum, DayNum}) of
        D when D == [];
               DayNum == ?undefined ->
            #mafia_day{key = Key,
                       thread_id = ThId,
                       day = DayNum,
                       votes = [],
                       end_votes = [],
                       players_rem = G#mafia_game.players_rem,
                       player_deaths = []
                      };
        [Day] ->
            Times = get_times(G, DayNum),
            Day#mafia_day{
              players_rem = get_day(players_rem, G, Times),
              player_deaths = get_day(players_deaths, G, Times)
             }
    end;
rday([], _) -> {?error, rday_no_such_game}.

get_times(G, DayNum) ->
    TimeNow = mafia_time:utc_secs1970(),
    DayPh = #phase{num = DayNum, ptype = ?day},
    NigPh = #phase{num = DayNum, ptype = ?night},
    TimeDLSta = mafia_time:get_time_for_prev_phase(G, DayPh),
    TimeDLEnd = case mafia_time:get_time_for_phase(G, NigPh) of
                    undefined ->
                        element(1, G#mafia_game.game_end);
                    Time when is_integer(Time) ->
                        Time
                end,
    DTimeSta = min(TimeDLSta + ?MAX_GM_DL_SECS, TimeNow),
    DTimeEnd = min(TimeDLEnd + ?MAX_GM_DL_SECS, TimeNow),
    RTimeSta = min(TimeDLSta, TimeNow),
    RTimeEnd = min(TimeDLEnd, TimeNow),
    {DTimeSta, DTimeEnd, RTimeSta, RTimeEnd}.

get_day(players_rem, G, {_, DTimeEnd, _, RTimeEnd}) ->
    %% Do player replacement upto RTimeEnd in players_orig
    Repls = [{Old, New}
             || #replacement{new_player = New,
                             replaced_player = Old,
                             time = RTime}
                    <- G#mafia_game.player_deaths, RTime =< RTimeEnd],
    Players = lists:foldr(fun({Old, New}, Ps) -> replace2(Old, New, Ps) end,
                          G#mafia_game.players_orig,
                          Repls),
    %% Remove dead players
    Deaths = [DUser || #death{player = DUser, time = DTime}
                           <- G#mafia_game.player_deaths, DTime =< DTimeEnd],
    Players -- Deaths;
get_day(players_deaths, G, {DTimeSta, DTimeEnd, RTimeSta, RTimeEnd}) ->
    lists:foldr(
      fun(R = #replacement{time = RTime}, Evs)
            when RTimeSta =< RTime, RTime < RTimeEnd -> [R | Evs];
         (D = #death{time = DTime}, Evs)
            when DTimeSta =< DTime, DTime < DTimeEnd -> [D | Evs];
         (_, Evs) -> Evs
      end,
      [],
      G#mafia_game.player_deaths).

replace2(Old, New, Ps) ->
    MatchF = fun(P) -> P == Old end,
    replace_p(MatchF, Ps, New).

%% -----------------------------------------------------------------------------

rgame(GameNum) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rgameI(GameNum) of
        [] -> [];
        [G] ->
            %% - reading game need to filter out "future" deaths
            Deaths2 =
                lists:foldr(
                  fun(#death{time = Time} = D, Acc)
                        when Time =< OffsetNow -> [D | Acc];
                     (#replacement{time = Time} = R, Acc)
                        when Time =< OffsetNow -> [R | Acc];
                     (_, Acc) -> Acc
                  end,
                  [],
                  G#mafia_game.player_deaths),
            %% - fix game_end if it is too early for it.
            GameEnd2 =
                case G#mafia_game.game_end of
                    {EndTime, _MsgId} = GE when EndTime =< OffsetNow ->
                        GE;
                    _ -> ?undefined
                end,
            G2 = G#mafia_game{player_deaths = Deaths2,
                              game_end = GameEnd2},
            [G2]
    end.

rgameI(GameNum) ->
    mnesia:dirty_read(mafia_game, GameNum).

%% -----------------------------------------------------------------------------
%% Read on primary key
-spec ruserUB(string() | binary(), site()) -> [#user{}].
ruserUB(User, Site) when is_list(User) -> ruserUBI(?l2ub(User), Site);
ruserUB(UserB, Site) when is_binary(UserB) -> ruserUBI(?b2ub(UserB), Site).

-spec ruserUB({binary(), site()}) -> [#user{}].
ruserUB({UserB, Site}) when is_binary(UserB) -> ruserUBI(?b2ub(UserB), Site).

-spec ruserUBI(binary(), site()) -> [#user{}].
ruserUBI(UserUB, Site) -> mnesia:dirty_read(user, {UserUB, Site}).

%% Read on secondary key
-spec ruser(string() | binary(), site()) -> [#user{}].
ruser(User, Site) when is_list(User) -> ruserI(?l2b(User), Site);
ruser(UserB, Site) when is_binary(UserB) -> ruserI(UserB, Site).

-spec ruserI(binary(), site()) -> [#user{}].
ruserI(UserB, Site) ->
    mnesia:dirty_index_read(user, {UserB, Site}, #user.name).

%% -----------------------------------------------------------------------------

is_alpha_num(C) when C < $0 -> ?false;
is_alpha_num(C) when C =< $9 -> ?true;
is_alpha_num(C) when C < $A -> ?false;
is_alpha_num(C) when C =< $Z -> ?true;
is_alpha_num(C) when C < $a -> ?false;
is_alpha_num(C) when C =< $z -> ?true;
is_alpha_num(C) when C > $z -> ?false.

remove_blockquotes(Msg) -> rm_bq(Msg, 0).
rm_bq("<blockquote" ++ Msg, Lvl)  -> rm_bq(Msg, Lvl + 1);
rm_bq("</blockquote>" ++ Msg, Lvl) -> rm_bq(Msg, Lvl - 1);
rm_bq([H | T], 0) -> [H | rm_bq(T, 0)];
rm_bq([_ | T], Lvl) -> rm_bq(T, Lvl);
rm_bq([], _) -> [].

-spec get_unicode_msg(MsgB :: binary()) -> string().
get_unicode_msg(MsgB) ->
    Msg0 = unicode:characters_to_list(MsgB),
    escapes_to_unicode(Msg0).

-spec escapes_to_unicode(Msg :: string()) -> string().
escapes_to_unicode("&#x" ++ T) ->
    %% Hexadecimal
    case find_semicolon(T) of
        not_found -> "&#x" ++ escapes_to_unicode(T);
        {EscStr, Rest} ->
            case catch list_to_integer(EscStr, 16) of
                {'EXIT', _} -> "&#x" ++ escapes_to_unicode(T);
                UCP -> [UCP | escapes_to_unicode(Rest)]
            end
    end;
escapes_to_unicode("&#" ++ T) ->
    %% Decimal
    case find_semicolon(T) of
        not_found -> "&#" ++ escapes_to_unicode(T);
        {EscStr, Rest} ->
            case catch list_to_integer(EscStr) of
                {'EXIT', _} -> "&#" ++ escapes_to_unicode(T);
                UCP -> [UCP | escapes_to_unicode(Rest)]
            end
    end;
escapes_to_unicode([$& | T]) ->
    case find_semicolon(T) of
        not_found -> [$& | escapes_to_unicode(T)];
        {EscStr, Rest} ->
            case find_ucp(EscStr) of
                not_found -> [$& | escapes_to_unicode(T)];
                UCP ->  [UCP | escapes_to_unicode(Rest)]
            end
    end;
escapes_to_unicode([H | T]) -> [H | escapes_to_unicode(T)];
escapes_to_unicode([]) -> [].

-spec find_semicolon(list()) -> not_found | {list(), list()}.
find_semicolon(Msg) ->
    %% read upto 11 chars between & and ;
    find_semicolon(Msg, 0, "").

find_semicolon([$; | T], _N, Acc) -> {?lrev(Acc), T};
find_semicolon(_, 11, _) -> not_found;
find_semicolon([], _, _) -> not_found;
find_semicolon([H | T], N, Acc) -> find_semicolon(T, N + 1, [H | Acc]).

-spec find_ucp(list()) -> not_found | integer().
find_ucp(EscStr) -> %55.
    Key = ?l2b(EscStr),
    case mnesia:dirty_read(?escape_sequence, Key) of
        [#?escape_sequence{unicode_point = UCP}] ->
            UCP;
        [] ->
            KeyU = ?l2ub(EscStr),
            case mnesia:dirty_index_read(?escape_sequence,
                                         KeyU,
                                         #?escape_sequence.esc_seq_upper) of
                [#?escape_sequence{unicode_point = UCP} | _] ->
                    UCP;
                [] ->
                    not_found
            end
    end.

%% -----------------------------------------------------------------------------

-define(PW_SPACE, 1000000).

set_new_password(User, Site) ->
    case ruserUB(User, Site) of
        [U = #user{name = {Name, _}}] ->
            Rand = rand:uniform(?PW_SPACE),
            PW = base64:encode_to_string(integer_to_list(Rand)),
            PwHash = erlang:phash2(PW, ?PW_SPACE),
            ?dwrite_user(U#user{pw_hash = PwHash}),
            {ok, {?b2l(Name), Site, PW, date()}}; %% send PW to user
        _ ->
            {error, user_not_found}
    end.

-spec check_password(string(), site(), string())
                    -> ok | {error, nomatch_user_password}.
check_password(User, Site, Password) ->
    %% Security improvment: Store time when fun returns error
    %% Delay response next time for this user to 30 seconds after last error
    case ruserUB(User, Site) of
        [#user{pw_hash = PwHashDb}] ->
            PwHashCmp = erlang:phash2(Password, ?PW_SPACE),
            if PwHashCmp == PwHashDb ->
                    ok;
               true ->
                    {error, nomatch_user_password}
            end;
        _ ->
            {error, nomatch_user_password}
    end.

copy_password_between_users(User1, Site1, User2, Site2)
  when is_list(User1),
       ?IS_SITE_OK(Site1),
       is_list(User2),
       ?IS_SITE_OK(Site2) ->
    case ruserUB(User1, Site1) of
        [#user{pw_hash = Pw}] when Pw /= ?undefined ->
            case ruserUB(User2, Site2) of
                [U2 = #user{name = {Name2, _}}] ->
                    ?dwrite_user(U2#user{pw_hash = Pw}),
                    {pw_copied, {?b2l(Name2), Site2}}; %% send PW to user
                _ ->
                    {error, user2_not_found}
            end;
        [#user{pw_hash = Pw}] when Pw == ?undefined ->
            {error, user1_has_no_password};
        _ ->
            {error, user1_not_found}
    end.

%% -----------------------------------------------------------------------------

-spec prev_msg(Msg :: #message{}) -> ?none | #message{}.
prev_msg(Msg) ->
    #message{msg_key = MsgKey,
             thread_id = ThId,
             page_num = PageNum
            } = Msg,
    {MsgId, Site} = MsgKey,
    P = hd(rpageI({ThId, PageNum, Site})),
    case lists:takewhile(fun(Mid) -> Mid < MsgId end, P#page_rec.message_ids) of
        [] ->
            case rpageI({ThId, PageNum - 1, Site}) of
                [] -> ?none;
                [P2] ->
                    hd(rmessI({lists:last(P2#page_rec.message_ids), Site}))
            end;
        MsgIds ->
            hd(rmessI({lists:last(MsgIds), Site}))
    end.

%% -----------------------------------------------------------------------------

get_path(P) when P == h_srv_root;
                 P == h_doc_root;
                 P == h_log_root;
                 P == h_tls_dir;
                 P == repo_dir ->
    {ok, [[Path]]} = init:get_argument(P),
    Path.

get_arg(P) when P == ?http_ip;
                P == ?http_interface ->
    case get_arg_file(P) of
        R1 = {ok, _} -> R1;
        _ ->
            get_arg_init(P)
    end.

%% {http_ip, "192.168.0.100"}.
%% {http_interface, "en1"}.
get_arg_file(P) ->
    case file:consult("HTTP_CONFIG.txt") of
        {ok, KVs} ->
            case lists:keyfind(P, 1, KVs) of
                false -> false;
                {_, Val} -> {ok, Val}
            end;
        _ -> false
    end.

get_arg_init(P) ->
    case init:get_argument(P) of
        {ok, [[Value]]} ->
            {ok, Value};
        _Error -> false
    end.

%% -----------------------------------------------------------------------------

-spec my_string_substr(list(), pos_integer(), pos_integer()) -> list().
my_string_substr(List, First, Last) ->
    my_string_substr(List, First, Last, 1).

my_string_substr([H | T], First, Last, Cur) when First =< Cur, Cur =< Last ->
    [H | my_string_substr(T, First, Last, Cur + 1)];
my_string_substr([_ | T], First, Last, Cur) when Cur < First ->
    my_string_substr(T, First, Last, Cur + 1);
my_string_substr(_, _First, Last, Cur) when Cur > Last -> [];
my_string_substr([], _First, _Last, _Cur) -> [].


%% -----------------------------------------------------------------------------

alpha_sort(Bs = [Bin | _]) when is_binary(Bin) ->
    Ls = alpha_sort([?b2l(B) || B <- Bs]),
    [?l2b(L) || L <- Ls];
alpha_sort(Strings) ->
    LE = fun(A, B) -> ?l2u(A) =< ?l2u(B) end,
    lists:sort(LE, Strings).

to_bin_sort([]) -> [];
to_bin_sort(LoL = [[_|_]|_]) ->
    [?l2b(L) || L <- alpha_sort(LoL)];
to_bin_sort(LoB = [Bin|_]) when is_binary(Bin) ->
    to_bin_sort([?b2l(L) || L <- LoB]).

%% -----------------------------------------------------------------------------

%% for mafia_lib...
-spec split_into_groups(NumPerRow :: integer(),
                        [Obj :: any()]) ->
                               [[Obj :: any()]].
split_into_groups(NumPerRow, Objects) ->
    {LastRow, Rows2} =
        lists:foldl(
          fun(U, {R, Rows1}) ->
                  if length(R) == NumPerRow ->
                          {[U], Rows1 ++ [R]};
                     true ->
                          {R ++[U], Rows1}
                  end
          end,
          {[], []},
          Objects),
    Rows2 ++ [LastRow].

%% -----------------------------------------------------------------------------


%% URL Chars: A-Za-z0-9-._~:/?#[]@!$&'()*+,;=%
-spec is_url_char(Char :: integer()) -> boolean().
is_url_char(C) when $A =< C, C =< $Z -> true;
is_url_char(C) when $a =< C, C =< $z -> true;
is_url_char(C) when $0 =< C, C =< $9 -> true;
is_url_char(C) -> lists:member(C, "-._~:/?#[]@!$&'()*+,;=%").

get_url([]) -> "";
get_url([H | T]) ->
    case is_url_char(H) of
        true -> [H | get_url(T)];
        false -> ""
    end.

-spec split_on_url_boundary(string()) -> {URL :: string(), string()}.
split_on_url_boundary(Msg) ->
    split_on_url_boundary(Msg, "").

split_on_url_boundary([], Acc) -> {?lrev(Acc), ""};
split_on_url_boundary([H | T] = Msg, Acc) ->
    case is_url_char(H) of
        true -> split_on_url_boundary(T, [H | Acc]);
        false -> {?lrev(Acc), Msg}
    end.

%% Remove "<br>" and white space in both ends of string (but not elsewhere)
%% Msg comes in and is returned reverted.
strip_br_white_rev(Msg) ->
    Msg2 = strip_br_white_b(Msg),
    ?lrev(strip_br_white_f(?lrev(Msg2))).

strip_br_white_fwd(Msg) ->
    Msg2 = strip_br_white_f(Msg),
    ?lrev(strip_br_white_b(?lrev(Msg2))).

strip_br_white_f([H | T]) when H =< $\s -> strip_br_white_f(T);
strip_br_white_f("<br>" ++ Msg) -> strip_br_white_f(Msg);
strip_br_white_f(Msg) -> Msg.

strip_br_white_b([H | T]) when H =< $\s -> strip_br_white_b(T);
strip_br_white_b(">rb<" ++ Msg) -> strip_br_white_b(Msg);
strip_br_white_b(Msg) -> Msg.

get_url_begin(#mafia_game{site = Site}) -> get_url_begin(Site);
get_url_begin(?webDip) -> ?UrlBeg;
get_url_begin(?vDip) -> ?UrlvDip;
get_url_begin(?wd2) -> ?UrlWd2.

bgcolor("") ->
    bgcolorI(?TURQUOISE_HEX);
bgcolor(Str) when is_list(Str) ->
    bgcolor(?l2b(Str));
bgcolor(Bin) when is_binary(Bin) ->
    Hash = erlang:phash2(Bin, 16#1000000),
    %% Color = Hash bor 16#C0C0C0,
    Blue = Hash band 255,
    Green = (Hash bsr 8) band 255,
    Red = (Hash bsr 16) band 255,
    {Red2, Green2, Blue2} = modify_colors(Red, Green, Blue),
    Color = (((Red2 bsl 8) + Green2) bsl 8) + Blue2,
    bgcolorI(integer_to_list(Color, 16)).

modify_colors(Red, Green, Blue) ->
    TargBright = 210,
    HighOut = 255,

    LowOutR = 130,
    LowOutG = 170,  %% less variation green
    LowOutB = 100,

    %% Scale to high value range, Green higher
    ModC = fun(Col, Low) ->
                   DiffOut = HighOut - Low,
                   Low + Col * DiffOut div 255
           end,
    R = ModC(Red, LowOutR),
    G = ModC(Green, LowOutG),
    B = ModC(Blue, LowOutB),

    %% Calculate brightness
    TargetBright2 = TargBright * TargBright,
    Bright2 = brightness2(R, G, B),

    %% Adjust to target brightness
    Div = 1000,
    Fac = math:sqrt(TargetBright2 / Bright2),
    Factor = if Fac < 0.95 -> 0.95; % Max 5% reduce
                true -> Fac
             end,
    DivFac = trunc(Div * Factor),
    Scale = fun(X) ->
                    X2 = (DivFac * X) div Div,
                    if X2 > 255 -> 255;
                       true -> X2
                    end
            end,
    {Scale(R), Scale(G), Scale(B)}.

%% @doc Brightness ^ 2
brightness2(R, G, B) -> 0.299 * R * R + 0.587 * G * G + 0.114 * B * B.

bgcolorI(ColorStr) ->
    [" bgcolor=\"#", ColorStr, "\""].

%% -----------------------------------------------------------------------------


split_on_first_char(P, Char) ->
    split_on_first_char(P, [], Char).

split_on_first_char([Char | T], Acc, Char) -> {?lrev(Acc), T};
split_on_first_char([], Acc, _) -> {?lrev(Acc), ""};
split_on_first_char([H | T], Acc, Char) ->
    split_on_first_char(T, [H | Acc], Char).

%% -----------------------------------------------------------------------------
%% @doc Replace element in list if MatchF returns true
-spec replace_p(function(), list(), term()) -> UpdatedList:: list().
replace_p(MatchF, List, NewElement) ->
    [case MatchF(E) of ?true -> NewElement; ?false -> E end || E <- List].

%% -----------------------------------------------------------------------------

%% read re:run matches
re_matches(_Str, []) -> [];
re_matches(Str, [{-1, _L}|SubStrs]) -> ["-1" |re_matches(Str, SubStrs)];
re_matches(Str, [{S, L}|SubStrs]) ->
    [string:substr(Str, S+1, L) | re_matches(Str, SubStrs)].

-type interval() :: {term(), term()}.
-type intervals() :: [interval()].

%% -----------------------------------------------------------------------------
%% Merge overlapping intervals (treat Intervals as an unordered set)
%% Example:
%%    merge_intervals([{1,5},{3,7},{10,14},{1,9},{13,20}]) -> [{1,9},{10,20}])
%% -----------------------------------------------------------------------------
-spec merge_intervals(Intervals::intervals()) -> intervals().
merge_intervals(Intervals) ->
    lists:sort(merge_intervals2(Intervals)).

merge_intervals2([]) -> [];
merge_intervals2([H | T]) ->
    case find_overlaps(H, T) of
        [] ->
            [H | merge_intervals2(T)];
        OverlappingIntervals ->
            MergedInterval = merge_overlap([H | OverlappingIntervals]),
            Rest = T -- OverlappingIntervals,
            merge_intervals2([MergedInterval | Rest])
    end.

%% -----------------------------------------------------------------------------
%% Returns all intervals in Intervals that overlaps with First
%% -----------------------------------------------------------------------------
-spec find_overlaps(First::interval(), Intervals::intervals()) -> intervals().
find_overlaps(_, []) -> [];
find_overlaps(First, [HInt | Intervals]) ->
    IsNeig = is_overlap(First, HInt),
    if IsNeig -> [HInt | find_overlaps(First, Intervals)];
       not IsNeig -> find_overlaps(First, Intervals)
    end.

%% -----------------------------------------------------------------------------
%% Checks if interval A and B are overlapping
%% -----------------------------------------------------------------------------
-define(IS_IN(P, Lo, Hi), ((Lo =< P) andalso (P =< Hi)) ).
-spec is_overlap(A::interval(), B::interval()) -> boolean().
is_overlap({ALo, _AHi}, {BLo, BHi}) when ?IS_IN(ALo, BLo, BHi) -> true;
is_overlap({_ALo, AHi}, {BLo, BHi}) when ?IS_IN(AHi, BLo, BHi) -> true;
is_overlap({ALo, AHi}, {BLo, _BHi}) when ?IS_IN(BLo, ALo, AHi) -> true;
is_overlap({ALo, AHi}, {_BLo, BHi}) when ?IS_IN(BHi, ALo, AHi) -> true;
is_overlap(_, _) -> false.

%% -----------------------------------------------------------------------------
%% Returns one interval spanning over the overlapping intervals
%% -----------------------------------------------------------------------------
-spec merge_overlap(intervals()) -> interval().
merge_overlap(L) ->
    {Lows, Highs} = lists:unzip(L),
    {lists:min(Lows), lists:max(Highs)}.


%% -----------------------------------------------------------------------------

%/2
%% Returns [{Page::integer(), MsgId :: msg_id()}]
all_msgids(ThId, Site) ->
    AllPages = pages_for_thread({ThId, Site}),
    all_msgids(ThId, Site, AllPages).

%/3
%% Returns [{Page::integer(), MsgId :: msg_id()}]
all_msgids(_, _, []) -> [];
all_msgids(ThId, Site, [PageN|PagesT]) ->
    all_msgids(ThId, Site, PagesT, PageN, ?rpage({ThId, PageN, Site})).

%/5
all_msgids(ThId, Site, PagesT, _PageN, []) ->
    all_msgids(ThId, Site, PagesT);
all_msgids(ThId, Site, PagesT, PageN, [PR]) ->
    all_msgids2(ThId, Site, PagesT, PageN, PR#page_rec.message_ids).

%% -----------------------------------------------------------------------------
%/5
all_msgids2(ThId, Site, PagesT, _PageN, []) ->
    all_msgids(ThId, Site, PagesT);
all_msgids2(ThId, Site, PagesT, PageN, [MId|MIds]) ->
    all_msgids3(ThId, Site, PagesT, PageN, [MId|MIds], ?rmess({MId, Site})).

%/6
%% Returns [{Page::integer(), MsgId :: msg_id()}]
all_msgids3(_, _, _, _, _, []) -> [];
all_msgids3(ThId, Site, PagesT, PageN, [MId|MIds], _) ->
    [{PageN, MId} | all_msgids2(ThId, Site, PagesT, PageN, MIds)].

%% -----------------------------------------------------------------------------
-type last_iter_msg_ref() :: ?none |
                             {ThId :: integer(),
                              Page :: integer(),
                              MsgId :: integer(),
                              MsgTime :: seconds1970()}.

%% Iterate through all message ids in one thread in time order
-spec iterate_all_msg_ids({thread_id(), site()},
                          MsgIdFun :: function()) -> last_iter_msg_ref().
iterate_all_msg_ids(ThId, Fun) ->
    iterate_all_msg_ids(ThId, Fun, all).

%% If the MsgIdFun returns an integer, the last returned integer
%% will also be returned from this fun as the MsgTime
-spec iterate_all_msg_ids({thread_id(), site()},
                          MsgIdFun :: function(),
                          PageFilter:: all | function())
                         -> last_iter_msg_ref().
iterate_all_msg_ids(Thread, MsgIdFun, PageFilter) ->
    iter_msgids(Thread, MsgIdFun, no_acc, PageFilter).

-spec iter_msgids({thread_id(), site()},
                  MsgIdFun :: function(),
                  Acc :: any(),
                  PageFilter:: all | function())
                 -> last_iter_msg_ref().
iter_msgids(Thread, MsgIdFun, Acc, PageFilter) ->
    All = pages_for_thread(Thread),
    Pages = if PageFilter == all ->
                    All;
               is_function(PageFilter) ->
                    lists:filter(PageFilter, All)
            end,
    %% get last page num
    iterate_all_msg_idsI(Thread, MsgIdFun, Pages, Acc,
                         erlang:fun_info(MsgIdFun, arity)).

%% Return reference to last iterated message
-type arity2resp() :: term().
-spec iterate_all_msg_idsI({thread_id(), site()},
                           MsgIdFun :: function(),
                           PageNums :: [integer()],
                           Acc :: term(),
                           {arity, integer()}
                          )
                          -> last_iter_msg_ref() | arity2resp().
iterate_all_msg_idsI(Thread = {ThId, Site},
                     MsgIdFun,
                     PageNums,
                     Acc,
                     {arity, Ar}) ->
    case mafia_lib:all_msgids(ThId, Site, PageNums) of
        [] -> ?none;
        PageMsgIds ->
            {LastPage, LastMsgId} = lists:last(PageMsgIds),
            if Ar == 1 ->
                    LastMsgTime =
                        lists:foldl(
                          fun({_, MId}, Acc2) ->
                                  R = MsgIdFun(MId),
                                  if is_integer(R) -> R;
                                     true -> Acc2
                                  end
                          end,
                          none,
                          PageMsgIds),
                    {Thread, LastPage, LastMsgId, LastMsgTime};
               Ar == 2 ->
                    AccOut =
                        lists:foldl(MsgIdFun, Acc,
                                    [Id || {_, Id} <- PageMsgIds]),
                    MsgIdFun(report, AccOut)
            end
    end.

%% -----------------------------------------------------------------------------
%% Counter updates
%% -----------------------------------------------------------------------------

inc_cnt(CntNameB) ->
    inc_cnt(CntNameB, ?none, 1).

inc_cnt(CntNameB, Inc) ->
    inc_cnt(CntNameB, ?none, Inc).

inc_cnt(CntNameB, Args, Inc) ->
    DayNum = mafia_time:utc_day1970(),
    [KeyGlobal, KeyDay] =
        if Args == ?none ->
                [{CntNameB, ?global},
                 {CntNameB, DayNum}];
           true ->
                [{CntNameB, ?global, Args},
                 {CntNameB, DayNum, Args}]
        end,
    dirty_update_counter(cnt, KeyGlobal, Inc),
    dirty_update_counter(cnt, KeyDay, Inc).

print_all_cnts() ->
    Guard = [],
    print_all_cntsI(standard_io, Guard).

print_all_cnts(Types) when is_list(Types) ->
    _ = [ print_all_cnts(Type) || Type <- Types];
print_all_cnts(NumLastDays) when is_integer(NumLastDays) ->
    DayNum = mafia_time:utc_day1970() - NumLastDays,
    Guard = [{'or', {'>=', '$2', DayNum}, {'==', '$2', ?global}}],
    print_all_cntsI(standard_io, Guard);
print_all_cnts(Type) when is_atom(Type) ->
    Guard = [{'==', '$1', Type}],
    print_all_cntsI(standard_io, Guard).

save_cnts_to_file() ->
    FN = mafia_file:cnt_filename(),
    {ok, Fd} = file:open(FN, [write]),
    Guard = [],
    print_all_cntsI(Fd, Guard),
    file:close(Fd),
    mnesia:clear_table(cnt),
    io:format("Saved all counters to ~s and "
              "cleared the counter table.\n", [FN]).

-define(l(V), to_list(V)).

print_all_cntsI(Fd, Guard) ->
    Pattern = mnesia:table_info(cnt, wild_pattern),
    MatchHead2 = Pattern#cnt{key = {'$1', '$2'}},
    MatchHead3 = Pattern#cnt{key = {'$1', '$2', '$3'}},
    Result = '$_',
    MatchExpr2 = [{MatchHead2, Guard, [Result]}],
    MatchExpr3 = [{MatchHead3, Guard, [Result]}],
    Cnts2 = mnesia:dirty_select(cnt, MatchExpr2),
    Cnts3 = mnesia:dirty_select(cnt, MatchExpr3),
    LessEq =
        fun(#cnt{key = A}, #cnt{key = B}) ->
                A1 = element(1, A), B1 = element(1, B),
                A2 = element(2, A), B2 = element(2, B),
                A3 = if size(A) < 3 -> ?undefined; true -> element(3, A) end,
                B3 = if size(B) < 3 -> ?undefined; true -> element(3, B) end,
                if A1 < B1 -> true;
                   A1 > B1 -> false;
                   is_atom(A2), is_integer(B2) -> true;
                   is_integer(A2), is_atom(B2) -> false;
                   A2 < B2 -> true;
                   A2 > B2 -> false;
                   A3 == ?undefined -> true;
                   B3 == ?undefined -> false;
                   true -> A3 =< B3
                end
        end,
    AllCnts = lists:sort(LessEq, Cnts2 ++ Cnts3),
    R = fun(I) -> string:right(?l(I), 2, $0) end,
    PrDay = fun(Day) ->
                    {Y, M, D} = mafia_time:utc_day2date(Day),
                    [R(Y), R(M), R(D)]
            end,
    PrintKey =
        fun({CntNameB, ?global}) ->
                ?l(CntNameB);
           ({CntNameB, Day}) ->
                [?l(CntNameB), ".", PrDay(Day)];
           ({CntNameB, ?global, Atom}) when is_atom(Atom) ->
                [?l(CntNameB), ".", ?l(Atom)];
           ({CntNameB, ?global, {Atom, Args}}) when is_atom(Atom) ->
                [?l(CntNameB), ".", ?l(Atom), ".",
                 print_args(Args)
                ];
           ({CntNameB, Day, Atom}) when is_atom(Atom) ->
                [?l(CntNameB), ".", PrDay(Day), ".", ?l(Atom)];
           ({CntNameB, Day, {Atom, Args}}) when is_atom(Atom) ->
                [?l(CntNameB), ".", PrDay(Day), ".", ?l(Atom), ".",
                 print_args(Args)]
        end,
    PrintCnt =
        fun(#cnt{key = K, value = V}) ->
                Row = [string:right(?i2l(V), 10), " ", PrintKey(K), "\n"],
                io:format(Fd, "~s", [Row])
        end,
    [PrintCnt(C) || C <- AllCnts],
    ok.

print_args([]) -> "no_args";
print_args(Args) -> string:join([repl(?b2l(A), $\s, $_)|| A <- Args], ".").

repl(L, In, Out) ->
    [if C == In -> Out ; true -> C end
     || C <- L].

to_list(A) when is_atom(A) -> ?a2l(A);
to_list(I) when is_integer(I) -> ?i2l(I);
to_list(V) -> V.

%% -----------------------------------------------------------------------------

dbg(Mod, Term) ->
    io:format("~s DBG ~p ~999p\n",
              [mafia_print:print_time(?console),
               Mod, Term]),
    Term.

dbg(Mod, Time, Term) ->
    io:format("~s DBG ~p ~999p\n",
              [mafia_print:print_time(?console, Time),
               Mod, Term]),
    Term.

dbg_str(Str) ->
    io:format("~s DBG ~s\n",
              [mafia_print:print_time(?console),
               Str]).

man(Time, Cmd) ->
    io:format("~s MANUAL ~999p\n",
              [mafia_print:print_time(?console, Time),
               Cmd]).

%% -----------------------------------------------------------------------------

stacktrace() ->
    try throw(a)
    catch throw:a ->
            erlang:get_stacktrace()
    end.

%% -----------------------------------------------------------------------------
%% EUNIT tests
%% -----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-define(OnG1,
        #mafia_game{thread_id = 1,
                    start_time = {{2000, 1, 2}, {0,0,0}},
                    game_end = ?undefined}).

-define(OnG2,
        #mafia_game{thread_id = 1,
                    start_time = {{2000, 1, 3}, {0,0,0}},
                    game_end = ?undefined}).

-define(UpG1,
        #mafia_game{thread_id = ?undefined,
                    start_time = {{2000, 2, 4}, {0,0,0}},
                    game_end = ?undefined}).

-define(UpG2,
        #mafia_game{thread_id = ?undefined,
                    start_time = {{2000, 2, 5}, {0,0,0}},
                    game_end = ?undefined}).

-define(FiG1,
        #mafia_game{thread_id = 1,
                    start_time = {{1999, 12, 1}, {0,0,0}},
                    game_end = {11, 11}}).

-define(FiG2,
        #mafia_game{thread_id = 1,
                    start_time = {{1999, 12, 2}, {0,0,0}},
                    game_end = {12, 12}
                   }).

sort_for_current_test_() ->
    [?_assertMatch([?FiG2, ?FiG1], sort_for_current([?FiG1, ?FiG2])),
     ?_assertMatch([?FiG2, ?FiG1], sort_for_current([?FiG2, ?FiG1])),

     ?_assertMatch([?OnG1, ?OnG2], sort_for_current([?OnG2, ?OnG1])),
     ?_assertMatch([?OnG1, ?OnG2], sort_for_current([?OnG1, ?OnG2])),

     ?_assertMatch([?UpG1, ?UpG2], sort_for_current([?UpG1, ?UpG2])),
     ?_assertMatch([?UpG1, ?UpG2], sort_for_current([?UpG2, ?UpG1])),

     ?_assertMatch(
        [?OnG1, ?OnG2, ?UpG1, ?UpG2, ?FiG2, ?FiG1],
        sort_for_current([?UpG2, ?UpG1, ?OnG1, ?OnG2, ?FiG1, ?FiG2]))
    ].

%% set_current_game
