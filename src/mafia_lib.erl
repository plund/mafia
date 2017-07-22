-module(mafia_lib).

-export([dwrite/2,

         rmess/1,
         rpage/1,
         rpage/2,
         pages_for_thread/1,
         all_page_keys/0,
         all_page_keys/1,

         rday/2,
         rgame/1,
         ruser/1,
         ruserUB/1,
         rmessI/1,

         set_new_password/1,
         check_password/2,

         %% prev_msg_id/1,
         prev_msg/1,

         get_path/1,
         get_arg/1,
         my_string_substr/3,
         alpha_sort/1,
         to_bin_sort/1,
         bgcolor/1,
         thid/1,
         gamename_to_thid/1,
         game_name/1,

         re_matches/2,
         merge_intervals/1,

         all_msgids/1,
         all_msgids/2,

         inc_cnt/1,
         inc_cnt/2,
         inc_cnt/3,
         print_all_cnts/0,
         print_all_cnts/1,
         save_cnts_to_file/0
        ]).

-include("mafia.hrl").

dwrite(page, Obj) ->
    dwrite_page(Obj);
dwrite(_Tag, Obj) ->
    mnesia:dirty_write(Obj).

rmess(MsgId) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rmessI(MsgId) of
        [] -> [];
        [M] when M#message.time > OffsetNow -> [];
        [M] -> [M]
    end.

rmessI(MsgId) ->
    mnesia:dirty_read(message, MsgId).

%% -----------------------------------------------------------------------------

rpage(Key) ->
    case rpageI(Key) of
        [] -> [];
        [P] ->
            MsgIds2 = [MId || MId <- P#page_rec.message_ids,
                              [] /= ?rmess(MId) ],
            if MsgIds2 == [] -> [];
               true ->
                    Complete = P#page_rec.complete andalso
                        MsgIds2 == P#page_rec.message_ids andalso
                        length(MsgIds2) > 25,
                    [P#page_rec{message_ids = MsgIds2,
                                complete = Complete}]
            end
    end.

rpage(ThId, Page) -> rpage({ThId, Page}).

rpageI(Key) -> mnesia:dirty_read(page_rec, Key).

%% update page_rec only if more info is added
dwrite_page(P) ->
    case rpageI(P#page_rec.key) of
        [] ->
            mnesia:dirty_write(P);
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
            mnesia:dirty_write(Db3)
    end.

pages_for_thread(ThId) ->
    [P || {_, P} <- all_page_keys(ThId)].

all_page_keys(Id) ->
    ThId = ?thid(Id),
    [K || K = {T,_} <- all_page_keys(), T == ThId].

all_page_keys() ->
    lists:sort([ K || K <- mnesia:dirty_all_keys(page_rec), [] /= rpage(K)]).


%% -----------------------------------------------------------------------------

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
        [] ->
            #mafia_day{key = Key,
                       thread_id = ThId,
                       day = DayNum,
                       votes = [],
                       end_votes = [],
                       players_rem = G#mafia_game.players_rem,
                       player_deaths = []
                      };
        [Day] ->
            Day
    end;
rday([], _) -> {?error, rday_no_such_game}.

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

-spec ruser(string() | binary()) -> [#user{}].
ruser(User) when is_list(User) -> ruserI(?l2b(User));
ruser(UserB) when is_binary(UserB) -> ruserI(UserB).

-spec ruserI(binary()) -> [#user{}].
ruserI(UserB) -> mnesia:dirty_index_read(user, UserB, #user.name).

-spec ruserUB(string() | binary()) -> [#user{}].
ruserUB(User) when is_list(User) -> ruserUBI(?l2ub(User));
ruserUB(UserB) when is_binary(UserB) -> ruserUBI(?b2ub(UserB)).

-spec ruserUBI(binary()) -> [#user{}].
ruserUBI(UserUB) -> mnesia:dirty_read(user, UserUB).

%% -----------------------------------------------------------------------------

-define(PW_SPACE, 1000000).

set_new_password(User) ->
    case mafia_lib:ruserUB(User) of
        [U = #user{name = Name}] ->
            Rand = rand:uniform(?PW_SPACE),
            PW = base64:encode_to_string(integer_to_list(Rand)),
            PwHash = erlang:phash2(PW, ?PW_SPACE),
            ?dwrite_user(U#user{pw_hash = PwHash}),
            {ok, {?b2l(Name), PW}}; %% send PW to user
        _ ->
            {error, user_not_found}
    end.

check_password(User, Password) ->
    %% Security improvment: Store time when fun returns error
    %% Delay response next time for this user to 30 seconds after last error
    case mafia_lib:ruserUB(User) of
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

%% -----------------------------------------------------------------------------

%% -spec prev_msg_id(Id :: msg_id()) -> ?none | msg_id().
%% prev_msg_id(Id) when is_integer(Id) ->
%%     case rmessI(Id) of
%%         [M] ->
%%             case prev_msg(M) of
%%                 PM = #message{} ->
%%                     PM#message.msg_id;
%%                 ?none -> ?none
%%             end
%%     end.

-spec prev_msg(Msg :: #message{}) -> ?none | #message{}.
prev_msg(Msg) ->
    #message{msg_id = MsgId,
             thread_id = ThId,
             page_num = PageNum
            } = Msg,
    P = hd(rpageI({ThId, PageNum})),
    case lists:takewhile(fun(Mid) -> Mid < MsgId end, P#page_rec.message_ids) of
        [] ->
            case rpageI({ThId, PageNum - 1}) of
                [] -> ?none;
                [P2] ->
                    hd(rmessI(lists:last(P2#page_rec.message_ids)))
            end;
        MsgIds ->
            hd(rmessI(lists:last(MsgIds)))
    end.

%% -----------------------------------------------------------------------------

get_path(P) when P == h_srv_root;
                 P == h_doc_root;
                 P == h_log_root;
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

alpha_sort(Strings) ->
    LE = fun(A, B) -> ?l2u(A) =< ?l2u(B) end,
    lists:sort(LE, Strings).

to_bin_sort(LoL = [[_|_]|_]) ->
    [?l2b(L) || L <- alpha_sort(LoL)];
to_bin_sort(LoB = [Bin|_]) when is_binary(Bin) ->
    to_bin_sort([?b2l(L) || L <- LoB]).

%% -----------------------------------------------------------------------------

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

thid(ThId) when is_integer(ThId) ->
    ThId;
thid(GN) when is_atom(GN) ->
    case gamename_to_thid(GN) of
        ThId when is_integer(ThId) ->
            ThId;
        ?undefined ->
            {?error, {?undefined, GN}}
    end.

gamename_to_thid(GN) when is_atom(GN) ->
    case ?getv(?reg_threads) of
        ?undefined -> ?undefined;
        Regs ->
            case lists:keyfind(GN, 1, Regs) of
                {_, ThId} ->
                    io:format("Translating ~p to ~p\n", [GN, ThId]),
                    ThId;
                false -> ?undefined
            end
    end.

%% -----------------------------------------------------------------------------

game_name(GN) when is_atom(GN) ->
    GN;
game_name(ThId) when is_integer(ThId) ->
    case thid_to_gamename(ThId) of
        ?undefined ->
            {?error, {?undefined, ThId}};
        GN when is_atom(GN) ->
            GN
    end.

thid_to_gamename(ThId) when is_integer(ThId) ->
    case ?getv(?reg_threads) of
        ?undefined -> ?undefined;
        Regs ->
            case lists:keyfind(ThId, 2, Regs) of
                {GN, _} ->
                    io:format("Translating ~p to ~p\n", [ThId, GN]),
                    GN;
                false -> ?undefined
            end
    end.

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

all_msgids(ThId) ->
    AllPages = pages_for_thread(ThId),
    all_msgids(ThId, AllPages).

all_msgids(_ThId, []) -> [];
all_msgids(ThId, [PageN|PagesT]) ->
    all_msgids(ThId, PagesT, PageN, ?rpage({ThId, PageN})).

all_msgids(ThId, PagesT, _PageN, []) ->
    all_msgids(ThId, PagesT);
all_msgids(ThId, PagesT, PageN, [PR]) ->
    all_msgids2(ThId, PagesT, PageN, PR#page_rec.message_ids).

all_msgids2(ThId, PagesT, _PageN, []) ->
    all_msgids(ThId, PagesT);
all_msgids2(ThId, PagesT, PageN, [MId|MIds]) ->
    all_msgids2(ThId, PagesT, PageN, [MId|MIds], ?rmess(MId)).

all_msgids2(_ThId, _PagesT, _PageN, _MIds, []) -> [];
all_msgids2(ThId, PagesT, PageN, [MId|MIds], _) ->
    [{PageN, MId} | all_msgids2(ThId, PagesT, PageN, MIds)].

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
    mnesia:dirty_update_counter(cnt, KeyGlobal, Inc),
    mnesia:dirty_update_counter(cnt, KeyDay, Inc).

print_all_cnts() ->
    Guard = [],
    print_all_cntsI(standard_io, Guard).

print_all_cnts(NumLastDays) ->
    DayNum = mafia_time:utc_day1970() - NumLastDays,
    Guard = [{'or', {'>=', '$2', DayNum}, {'==', '$2', ?global}}],
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
