-module(mafia).

-include("mafia.hrl").
%% todo:
%% ok - Day sequencing
%% ok - Vote finding listing
%% ok - store full pages as files locally "m24_threadid_page.txt"
%%   ok - use local files as input when then do exist.
%%   ok - add game_num (24) as game attribute into game record.
%%   ok - construct filename
%%   ok - test store a binary/list message.
%% - fix bug timezone_game
%%   - move stuff to mafia_time
%% - redo vote data using page_rec -> message
%%   - print_vote per voter variant
%%   - make sure only remaining players votes are counted
%%   - make sure min 2 letters indicate?
%%   - add "raw" text for every vote
%%   - fun to take message record as input

-export([set_thread_id/1,
         show_settings/0,
         print_pages_for_thread/0,
         print_pages_for_thread/1,
         t/0, t/1,
         pm/1,
         pp/0, pp/1, pp/2,
         pps/1, pps/2,
         print_votes/0,
         remove_mnesia/0,
         set_kv/2
        ]).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================
remove_mnesia() -> mafia_db:remove_mnesia().
    
set_kv(K,V) -> mafia_db:set_kv(K,V).
    
get_kv(K) -> mafia_db:get_kv(K).

-spec set_thread_id(ThId :: integer())  -> ok.
set_thread_id(ThId) when is_integer(ThId) ->
    set_kv(thread_id, ThId),
    PageToRead =
        case find_pages_for_thread(ThId) of
            [] -> 1;
            Pages ->
                lists:max(Pages)
        end,
    set_kv(page_to_read, PageToRead),
    ok.

show_settings() ->
    PrintSettings =
        fun(K) -> Setting = hd(mnesia:dirty_read(?kv_store, K)),
                  SetKey = element(2, Setting),
                  SetVal = element(3, Setting),
                  io:format("~p: ~p\n", [SetKey, SetVal])
        end,
    [PrintSettings(K) || K <- mnesia:dirty_all_keys(?kv_store)],
    ok.

print_pages_for_thread() ->
    ThId = get_kv(thread_id),
    print_pages_for_thread(ThId).

print_pages_for_thread(ThId) ->
    Pages = find_pages_for_thread(ThId),
    io:format("Thread ~p has stored Pages ~w\n", [ThId, Pages]).

t() ->
    mafia_db:setup_mnesia(),
    inets:start(),
    Thread = get_kv(thread_id),
    Page = get_kv(page_to_read),
    %% Page = get_kv(page_to_read),
    t(#s{thread_id = Thread, page = Page}).

t(S) ->
    case get_body(S) of
        {ok, S2} -> print_usr_msgs(S2);
        error -> error
    end.

print_votes() ->
    ThId = get_kv(thread_id),
    print_votes(ThId, 1).

pm(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [Msg] ->
            print_message_full(Msg);
        [] -> io:format("Message ID ~p not found\n", [MsgId])
    end.

pp() ->
    Page = get_kv(page_to_read),
    pp(Page).

pp({ThId, Page}) ->
    pp(ThId, Page);
pp(Page) ->
    ThId = get_kv(thread_id),
    pp(ThId, Page).

pp(ThId, Page) ->
    print_page(ThId, Page, fun print_message_full/1).

pps({ThId, Page}) ->
    pps(ThId, Page).

pps(ThId, Page) ->
    print_page(ThId, Page, fun print_message_summary/1).

%% =============================================================================
%% INTERNAL FUNCTIONS
%% =============================================================================
-spec get_body(S :: #s{}) -> {ok, #s{}} | error.
get_body(S) ->
    get_body(S, is_page_on_file(S)).

get_body(S, {file, Body}) ->
    {ok, S#s{body = {stripped, Body}}};
get_body(S, no_file) ->
    S2 = make_url(S),
    case httpc:request(S2#s.url) of
        {ok, {_StatusLine, _Headers, Body}} ->
            {ok, S2#s{body = Body}};
        {ok, {_StatusCode, Body}} ->
            {ok, S2#s{body = Body}};
        {ok, _ReqId} ->
            error;
        {error, _Reason} ->
            error
    end.

is_page_on_file(S) ->
    %% Do we have body on file?
    %% we only store complete pages on file!
    %% - store full pages as files locally "m24_threadid_page.txt"
    FileName = th_filename(S),
    case file:read_file(FileName) of
        {ok, BodyBin} ->
            {file, binary_to_list(BodyBin)};
        {error,enoent} ->
            no_file
    end.

th_filename(#s{thread_id = Thread, page = Page}) ->
    DirName = "thread_pages",
    file:make_dir(DirName),
    ThFileName = i2l(Thread) ++ "_" ++ i2l(Page) ++ ".txt",
    case mnesia:dirty_read(mafia_game, Thread) of
        [] ->
            filename:join(DirName, ThFileName);
        [G] ->
            GamePrefix = "m" ++ i2l(G#mafia_game.game_num) ++ "_",
            filename:join(DirName, GamePrefix ++ ThFileName)
    end.

%% -> ok | {error, Reason}
store_page(S, Body) ->
    FileName = th_filename(S),
    case file:read_file_info(FileName) of
        {error, enoent} ->
            file:write_file(FileName, Body);
        _ ->
            {error, efileexist}
    end.

make_url(S) ->
    Url = ?UrlBeg ++ i2l(S#s.thread_id) ++ ?UrlMid ++ i2l(S#s.page) ++ ?UrlEnd,
    S#s{url = Url}.

print_usr_msgs(S) ->
    B2 = get_thread_section(S#s.body),
    S2 = check_this_page(S#s{body=B2}),
    case S#s.body of
        {stripped, _} -> ok; % page already stored on file
        _ -> % not stored
            if not S2#s.is_last_page -> % page complete > STORE IT!
                    store_page(S2, B2);
               true -> ok
            end
    end,
    print_usr_msgs2(S2, B2).

get_thread_section({stripped, Body}) ->
    %% got from file and they are already stripped there.
    Body;
get_thread_section(Body) ->
    ThId = get_kv(thread_id),
    ThStartStr = "<div class=\"thread threadID" ++ i2l(ThId), %%"1404320",
    B2 = rm_to_after(Body, ThStartStr),
    ThEndStr = "<div class=\"thread thread",
    {_, ThreadStr} = read_to_before(B2, ThEndStr),
    ThreadStr. %% This should be stored on file, if it is a full page

%% find page nums "Page 177 of 177", if it exists
%% sets #s.is_last_page,
%%      #s.page_num_last_read = #s.page = PageLastRead,
%%      #s.page_total_last_read = PageTotal
-spec check_this_page(S :: #s{}) -> #s{}.
check_this_page(S) ->
    {_, Head} = read_to_before(S#s.body, "class=\"message-head"),
    %%<em>Page <strong>177</strong> of <strong>177</strong>
    {PageLastRead, PageTotal} =
        case rm_to_after(Head, ["<em>Page <strong>"]) of
            "" -> {1, 1};
            B2 ->
                {B3, PageStr} = read_to_before(B2, "</strong>"),
                LastRead = list_to_integer(PageStr),
                B4 = rm_to_after(B3, ["</strong> of <strong>"]),
                {_B5, PageTotStr} = read_to_before(B4, "</strong>"),
                Total = list_to_integer(PageTotStr),
                {LastRead, Total}
        end,
    IsLastPage = if PageLastRead == PageTotal -> true;
                    true -> false
                 end,
    case IsLastPage of
        true ->
            set_kv(page_to_read, PageLastRead),
            ok;
        false ->
            set_kv(page_to_read, PageLastRead + 1)
            %% set_kv(page_complete, PageLastRead)
    end,
    S#s{is_last_page = IsLastPage,
        page_num_last_read = PageLastRead,
        page = PageLastRead,
        page_total_last_read = PageTotal}.

print_usr_msgs2(_S, "") ->
    done;
print_usr_msgs2(S, Body) ->
    B3 = rm_to_after(Body, ["<div class=\"reply", "<div class=\"message-head", "profile.php?user", ">"]),
    {B4, UserRaw} = read_to_before(B3, "<"),
    UserStr = strip(UserRaw),
    B4a = rm_to_after(B4, "messageID=\""),
    {B4a2, MsgId} = read_to_before(B4a, "\""),
    B4b = rm_to_after(B4a2, "unixtime=\""),
    {B4c, TimeStr} = read_to_before(B4b, "\""),
    B5 = rm_to_after(B4c, ["<div class=\"message-contents\"", ">"]),
    {B6, MsgRaw} = read_to_before(B5, "</div>"),
    Msg = strip_fix(MsgRaw),

    if UserStr /= "" ->
            MsgIdInt = list_to_integer(MsgId),
            Time = list_to_integer(TimeStr),
            User = list_to_binary(UserStr),
            case mnesia:dirty_read(message, MsgIdInt) of
                [] ->
                    io:format("New ~w - ~w - ~w~n", [S#s.thread_id, S#s.page, MsgIdInt]),
                    update_page_rec(S, MsgIdInt),
                    MsgR = write_message_rec(S, MsgIdInt, User, Time, Msg),
                    check_for_vote(S, MsgIdInt, User, Time, Msg),
                    print_message_full(MsgR);
                [MsgR] ->
                    print_message_summary(MsgR)
            end;
       true -> ok
    end,
    print_usr_msgs2(S, B6).

update_page_rec(S, MsgIdInt) ->
    PageRec =
        case mnesia:dirty_read(page_rec, {S#s.thread_id, S#s.page}) of
            [] -> #page_rec{key = {S#s.thread_id, S#s.page},
                            message_ids = [MsgIdInt],
                            thread_id = S#s.thread_id,
                            complete = false};
            [P = #page_rec{message_ids = MsgIds, complete = Comp}] ->
                MsgIds2 = MsgIds ++ [MsgIdInt],
                Comp2 = Comp orelse not S#s.is_last_page,
                P#page_rec{message_ids = MsgIds2,
                           complete = Comp2}
        end,
    mnesia:dirty_write(PageRec).

find_pages_for_thread(ThId) ->
    MatchHead = #page_rec{key = {'$1', '$2'}, _='_'},
    Guard = {'==', '$1', ThId},
    Result = '$2',
    mnesia:dirty_select(page_rec,[{MatchHead, [Guard], [Result]}]).

write_message_rec(S, MsgIdInt, User, Time, Msg) ->
    mnesia:dirty_write(
      M = #message{msg_id = MsgIdInt,
                   thread_id = S#s.thread_id,
                   page_num = S#s.page,
                   user_name = User,
                   time = Time,
                   message = list_to_binary(Msg)
                  }
     ),
    M.

check_for_vote(S, MsgId, User, Time, Msg) ->
    MsgUC = string:to_upper(Msg),
    case mnesia:dirty_read(mafia_game, S#s.thread_id) of
        [] -> ignore;
        [#mafia_game{players_rem = Players} = G] ->
            case rm_to_after(MsgUC, "##VOTE") of
                "" -> ignore;
                RestUC ->
                    case rank(Players, RestUC) of
                        [{_,TopP}] ->
                            vote(S, G, User, Time, MsgId, TopP, true);
                        [{NumV, _P1}, {NumV, _P2}|_] ->
                            VoteStr = waste_spaces(
                                        string:left(
                                          waste_spaces(RestUC), 15)),
                            Vote = list_to_binary(VoteStr),
                            vote(S, G, User, Time, MsgId, Vote, false);
                        [{NumV1, TopP}, {NumV2, _}|_] when NumV1 > NumV2 ->
                            vote(S, G, User, Time, MsgId, TopP, true)
                    end
            end
    end.

vote(S, G, User, Time, MsgId, Vote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, Time) of
        {DayNum, ?day} ->
            Key = {S#s.thread_id, DayNum},
            case mnesia:dirty_read(mafia_day, Key) of
                [] ->
                    Day = #mafia_day{
                      key = Key,
                      thread_id = S#s.thread_id,
                      day = DayNum,
                      votes = [{User, [{Time, MsgId, S#s.page, Vote, IsOkVote}]}],
                      complete = false
                     },
                    mnesia:dirty_write(Day);
                [#mafia_day{votes = Votes} = Day] ->
                    Votes2 =
                        case lists:keyfind(User, 1, Votes) of
                            false ->
                                [{User, [{Time, MsgId, S#s.page, Vote, IsOkVote}]} | Votes];
                            {User, UVotes} ->
                                UVotes2 = [{Time, MsgId, S#s.page, Vote, IsOkVote} | UVotes],
                                lists:keystore(User, 1, Votes, {User, UVotes2})
                        end,
                    mnesia:dirty_write(Day#mafia_day{votes = Votes2})
            end;
        _ ->
            ignore
    end.

print_votes(ThId, DayNum) ->
    Key = {ThId, DayNum},
    case mnesia:dirty_read(mafia_day, Key) of
        [] ->
            ignore;
        [#mafia_day{votes = Votes}] ->
            VoteSummary =
                lists:foldl(fun({User, UserVotes}, Acc) ->
                                    case lists:dropwhile(fun(V) -> not element(5, V) end,
                                                         UserVotes) of
                                        [VoteInfo|_] ->
                                            Vote = element(4, VoteInfo),
                                            add_vote(Vote, {element(1,VoteInfo), User}, Acc);
                                        [] -> Acc
                                    end
                            end,
                            [],
                            Votes),
            %%Fun(A, B) is to return true if A compares less than or equal to B in the ordering
            GtEq = fun(A, B) -> element(2, A) >= element(2, B) end,
            VoteSum2 = lists:sort(GtEq, VoteSummary),
            io:format("Votes day ~p\n"
                      "------------\n", [DayNum]),
            [ begin
                  Voters = [element(2,VI) || VI <- lists:sort(VoteInfos)],
                  io:format("~s - ~p - ", [binary_to_list(Vote), N]),
                  io:format("~s\n", [string:join([binary_to_list(Voter) || Voter <- Voters], ", ")])
              end || {Vote, N, VoteInfos} <- VoteSum2],
            print_votes(ThId, DayNum + 1)
    end.

add_vote(Vote, User, Acc) ->
    case lists:keyfind(Vote, 1, Acc) of
        false ->
            [{Vote, 1, [User]} | Acc];
        {_, _NumV, Voters} ->
            Voters2 = (Voters -- [User]) ++ [User],
            NumV2 = length(Voters2),
            lists:keystore(Vote, 1, Acc, {Vote, NumV2, Voters2})
    end.

waste_spaces(L) -> [E || E <- L, E /= $\s].

rank(Players, RestUC) ->
    RestUCW = waste_spaces(RestUC),
    F = fun(P) ->
                PlayerUCW = string:to_upper(waste_spaces(binary_to_list(P))),
                r_count(PlayerUCW, RestUCW, 0)
        end,
    lists:reverse(lists:sort([{F(P), P} || P <- Players])).

r_count([Hp|Tp], [Hr|Tr], N) when Hp == Hr ->
    r_count(Tp, Tr, N+1);
r_count([_Hp|_Tp], [], N) ->
    N;
r_count([], [_Hr|_Tr], N) ->
    N;
r_count([Hp|_Tp], [Hr|_Tr], N) when Hp /= Hr ->
    N-1;
r_count([], [], N) ->
    N+1.

print_page(ThId, Page, PrintFun) ->
    MsgIds = case mnesia:dirty_read(page_rec, {ThId, Page}) of
                 [] -> [];
                 [#page_rec{message_ids = MIds}] -> MIds
             end,
    DoPrint =
        fun(MsgId) ->
                [Msg] = mnesia:dirty_read(message, MsgId),
                PrintFun(Msg)
        end,
    [DoPrint(MsgId) || MsgId <- MsgIds],
    ok.

print_message_full(M) ->
    io:format("User  : ~s\n"
              "Page  : ~s\n"
              "Time  : ~s\n"
              "Thread: ~s\n"
              "Msg id: ~s\n"
              "Wrote : \"~s\"\n"
              "\n",
              [binary_to_list(M#message.user_name),
               i2l(M#message.page_num),
               fix_time(M#message.time),
               i2l(M#message.thread_id),
               i2l(M#message.msg_id),
               binary_to_list(M#message.message)
              ]).

print_message_summary(M) ->
    Msg = fix_sum(binary_to_list(M#message.message)),
    MsgLen = length(Msg),
    Max = 30,
    MsgShort = if MsgLen > Max -> string:left(Msg, Max) ++ "...";
                  true -> Msg
               end,
    io:format("~s, "
              "p ~s, "
              " ~s, "
              "id: ~s, "
              "\"~s\"\n",
              [string:left(binary_to_list(M#message.user_name), 12),
               i2l(M#message.page_num),
               fix_time(M#message.time),
               i2l(M#message.msg_id),
               MsgShort
              ]).

%% half this fun should go to mafia_time
fix_time(Time) when is_integer(Time) ->
    {TzH, Dst} =
        case get_kv(print_time) of
            game -> {get_kv(timezone_game), get_kv(dst_game)};
            user -> {get_kv(timezone_user), get_kv(dst_user)};
            Loc when Loc == utc;
                     Loc == zulu;
                     Loc == gmt ->
                {0, false}
        end,
    fix_time(Time, TzH, Dst).

fix_time(Time, TzH, Dst) when is_integer(Time) ->
    try 
        Time2 = Time + (TzH + if Dst -> 1; true -> 0 end) * ?HourSecs,
        {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(Time2),
        {Y, M, D} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({1970,1,1}) + Days1970),
        case {TzH, Dst} of
            {0, false} ->
                io_lib:format("~s-~s-~sZ~s:~s:~s", [p(Y), p(M), p(D), p(HH), p(MM), p(SS)]);
            _ ->
                DstStr = case Dst of false -> "N"; true -> "DST" end,
                io_lib:format("~s-~s-~sT~s:~s:~s (~s ~s)",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS), i2l(TzH), DstStr])
        end
    catch _:_ -> ""
    end.

i2l(I) -> integer_to_list(I).

p(I) when I > 9 -> i2l(I);
p(I) -> string:right(i2l(I), 2, $0).

rm_to_after(Str, []) -> Str;
rm_to_after(Str, [Search|T]) when is_list(Search) ->
    rm_to_after(rm_to_after(Str, Search), T);
% return string()
rm_to_after(Str, Search) ->
    case string:str(Str, Search) of
        0 -> "";
        P ->
            lists:nthtail(P-1+length(Search), Str)
    end.

% return  {NewStr, ReadText}
read_to_before(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {"", ""};
        P ->
            Str2 = lists:nthtail(P-1, Str),
            Read = string:left(Str, P-1),
            {Str2, Read}
    end.

strip_fix(Str) ->
    S2 = strip(Str),
    fix(S2).

%% skip unicode for a while
fix("&gt;" ++ T) -> [ $> | fix(T)];
fix("&lt;" ++ T) -> [ $< | fix(T)];
fix("&lsquo;" ++ T) -> [ $' | fix(T)];
fix("&rsquo;" ++ T) -> [ $' | fix(T)];
fix("&ldquo;" ++ T) -> [ $\" | fix(T)];
fix("&rdquo;" ++ T) -> [ $\" | fix(T)];
%% fix("&lsquo;" ++ T) -> [ $‘ | fix(T)];
%% fix("&rsquo;" ++ T) -> [ $’ | fix(T)];
%% fix("&ldquo;" ++ T) -> [ $“ | fix(T)];
%% fix("&rdquo;" ++ T) -> [ $” | fix(T)];
fix("<br />" ++ T) ->  [ $\n | fix(T)];
fix([H|T]) -> [H|fix(T)];
fix("") -> "".

fix_sum([$\n|T]) -> [$\s|fix_sum(T)];
fix_sum([H|T]) -> [H|fix_sum(T)];
fix_sum("") -> "".

strip(Str) ->
    t_strip(h_strip(Str)).

h_strip([H|T]) when H =< $\s -> strip(T);
h_strip(Str) -> Str.

t_strip(Str) ->
    lists:reverse(h_strip(lists:reverse(Str))).

%% Order in page source
%% 0. threadID1404320
%% 1. threadID="1404320"
%% 2. <the thread title>
%% 3. <em>Page <strong>177</strong> of <strong>177</strong>

%% Find threadid "threadID1404320" 
%% split html on "<div class=\"reply"
%% inside we should find "<div class=\"message-head", "profile.php?user", ">", 
%% copy user name until we find next "<"
%% find "messageID=\""
%% copy msgid to next "\""
%% find "unixtime=\""
%% copy unixtime to next "\""
%% find "<div class=\"message-contents\"", ">"
%% copy message text unit next "</div>" BUT ignore "<b />"
