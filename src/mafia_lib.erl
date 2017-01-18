-module(mafia_lib).

-export([rmess/1,
         rpage/1,
         rpage/2,
         pages_for_thread/1,
         page_keys_for_thread/1,

         rday/2,
         rgame/1,
         ruser/1,
         ruserUB/1,
         rmessI/1,

         bgcolor/1,
         thid/1,
         gamename_to_thid/1,
         dl2phase/1,
         dl2time/1,
         set_dl_time/2,
         phase_time2dl/2,

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
                    [P#page_rec{message_ids = MsgIds2}]
            end
    end.

rpage(ThId, Page) -> rpage({ThId, Page}).

rpageI(Key) -> mnesia:dirty_read(page_rec, Key).

pages_for_thread(ThId) ->
    [P || {_, P} <- page_keys_for_thread(ThId)].

page_keys_for_thread(ThId) ->
    Tab = page_rec,
    PageKeys0 = [K || K= {GK, _Page} <- mnesia:dirty_all_keys(Tab),
                      GK == ThId],
    Exists = fun(PRK) ->
                     PRec = hd(mnesia:dirty_read(Tab, PRK)),
                     [] /= rmess(hd(PRec#page_rec.message_ids))
             end,
    PageKeys = [K || K <- PageKeys0, Exists(K)],
    lists:sort(PageKeys).

%% -----------------------------------------------------------------------------

rday(ThId, {DayNum, _}) ->
    rday(ThId, DayNum);
rday(ThId, DayNum) when is_integer(ThId) ->
    rday(?rgame(ThId), DayNum);
rday([#mafia_game{} = G], DayNum) ->
    rday(G, DayNum);
rday(#mafia_game{} = G, DayNum) ->
    ThId = G#mafia_game.key,
    case mnesia:dirty_read(mafia_day, Key = {ThId, DayNum}) of
        [] ->
            [#mafia_day{key = Key,
                        thread_id = ThId,
                        day = DayNum,
                        votes = [],
                        end_votes = [],
                        players_rem = G#mafia_game.players_rem,
                        player_deaths = []
                       }];
        [Day] ->
            [Day]
    end.

%% -----------------------------------------------------------------------------

rgame(ThId) ->
    OffsetNow = mafia_time:utc_secs1970(),
    case rgameI(ThId) of
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

rgameI(ThId) ->
    mnesia:dirty_read(mafia_game, ThId).

%% -----------------------------------------------------------------------------

ruser(User) when is_list(User) -> ruserUB(?l2ub(User));
ruser(User) when is_binary(User) -> ruserUB(?b2ub(User)).

ruserUB(UserUB) -> mnesia:dirty_read(user, UserUB).

%% -----------------------------------------------------------------------------

bgcolor(Str) when is_list(Str) ->
    bgcolor(?l2b(Str));
bgcolor(Bin) when is_binary(Bin) ->
    Hash = erlang:phash2(Bin, 16#1000000),
    Color = Hash bor 16#C0C0C0,
    [" bgcolor=\"#", integer_to_list(Color, 16), "\""].

%% -----------------------------------------------------------------------------

thid(ThId) when is_integer(ThId) ->
    ThId;
thid(GN) when is_atom(GN) ->
    case gamename_to_thid(GN) of
        ThId when is_integer(ThId) ->
            ThId;
        ?undefined -> {?error, ?undefined}
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

dl2phase(DL) when ?IS_DL(DL) -> element(1, DL).
dl2time(DL) when ?IS_DL(DL) -> element(2, DL).

set_dl_time(DL, Time) -> setelement(2, DL, Time).

phase_time2dl(?game_ended, Time) -> {?game_ended, Time};
phase_time2dl({Num, Don}, Time) -> {{Num, Don}, Time}.

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
    AllPages = mafia:pages_for_thread(ThId),
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
    MatchHead2 = #cnt{key = {'$1', '$2'}, _='_'},
    MatchHead3 = #cnt{key = {'$1', '$2', '$3'}, _='_'},
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
