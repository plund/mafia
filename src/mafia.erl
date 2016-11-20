-module(mafia).

-include("mafia.hrl").
%% todo:
%% - add set_kv checks
%% - redo vote data using page_rec -> message
%%   - print_vote per voter variant
%%   - make sure only remaining players votes are counted
%%   - make sure min 2 letters indicate?
%%   - Alias list "HR", "BB", "MEME"
%%   ok - add "raw" text for every vote
%%   ok - refresh_votes
%%   ok - fun to take message record as input
%%   ok - new record #vote{}
%% ok - fix bug timezone_game
%%   ok - move stuff to mafia_time

-export([set_thread_id/1,
         show_settings/0,
         print_pages_for_thread/0,
         print_pages_for_thread/1,
         t/0, t/1,
         pm/1,
         pp/0, pp/1, pp/2,
         pps/1, pps/2,
         print_votes/0,
         refresh_votes/0,
         setup_mnesia/0,
         remove_mnesia/0
        ]).

%% Libary
-export([b2l/1,
         l2b/1,
         i2l/1,
         l2i/1,
         get_kv/1,
         set_kv/2
        ]).

-export([cmp_vote_raw/0
        ]).

%% =============================================================================
%% EXPORTED FUNCTIONS
%% =============================================================================
i2l(I) -> integer_to_list(I).
l2i(L) -> list_to_integer(L).

b2l(B) -> binary_to_list(B).
l2b(L) -> list_to_binary(L).
    
set_kv(K,V) -> mafia_db:set_kv(K,V).
    
get_kv(K) -> mafia_db:get_kv(K).

setup_mnesia() -> mafia_db:setup_mnesia().

remove_mnesia() -> mafia_db:remove_mnesia().

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

refresh_votes() ->
    mnesia:clear_table(mafia_day),
    ThId = get_kv(thread_id),
    Pages = lists:sort(find_pages_for_thread(ThId)),
    F = fun(Page) ->
                Key = {ThId, Page},
                [PR] = mnesia:dirty_read(page_rec, Key),
                MsgIds = PR#page_rec.message_ids,
                lists:foreach(fun check_for_vote/1, MsgIds),
                Key
        end,
    [F(P) || P <- Pages].

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
            {file, b2l(BodyBin)};
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
                LastRead = l2i(PageStr),
                B4 = rm_to_after(B3, ["</strong> of <strong>"]),
                {_B5, PageTotStr} = read_to_before(B4, "</strong>"),
                Total = l2i(PageTotStr),
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
            MsgIdInt = l2i(MsgId),
            Time = l2i(TimeStr),
            User = l2b(UserStr),
            case mnesia:dirty_read(message, MsgIdInt) of
                [] ->
                    io:format("New ~w - ~w - ~w~n", [S#s.thread_id, S#s.page, MsgIdInt]),
                    update_page_rec(S, MsgIdInt),
                    MsgR = write_message_rec(S, MsgIdInt, User, Time, Msg),
                    check_for_vote(MsgR),
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
                   message = l2b(Msg)
                  }
     ),
    M.

check_for_vote(MsgId) when is_integer(MsgId) ->
    case mnesia:dirty_read(message, MsgId) of
        [] -> ignore;
        [Msg] -> check_for_vote(Msg)
    end;
check_for_vote(M = #message{}) ->
    Msg = b2l(M#message.message),
    MsgUC = string:to_upper(Msg),
    case mnesia:dirty_read(mafia_game, M#message.thread_id) of
        [] -> ignore;
        [#mafia_game{players_rem = Players} = G] ->
            SearchStr = "##VOTE",
            case rm_to_after_pos(MsgUC, SearchStr) of
                {0, ""} -> ignore;
                {Pos, RestUC} ->
                    RawVote =
                        l2b(string:strip(
                              string:left(
                                get_after_pos(Pos, length(SearchStr), Msg),
                                15))),
                    case rank(Players, RestUC) of
                        [{_,TopP}] ->
                            vote(M, G, TopP, RawVote, true);
                        [{NumV, _P1}, {NumV, _P2}|_] ->
                            VoteStr = waste_spaces(
                                        string:left(
                                          waste_spaces(RestUC), 15)),
                            Vote = l2b(VoteStr),
                            vote(M, G, Vote, RawVote, false);
                        [{NumV1, TopP}, {NumV2, _}|_] when NumV1 > NumV2 ->
                            vote(M, G, TopP, RawVote, true)
                    end
            end
    end.

vote(M, G, Vote, RawVote, IsOkVote) ->
    %% find mafia_day
    case mafia_time:calculate_phase(G, M#message.time) of
        {DayNum, ?day} ->
            Key = {M#message.thread_id, DayNum},
            User = M#message.user_name,
            NewVote = #vote{time = M#message.time,
                            id = M#message.msg_id,
                            page = M#message.page_num,
                            vote = Vote,
                            raw = RawVote,
                            valid = IsOkVote
                           },
            Day =
                case mnesia:dirty_read(mafia_day, Key) of
                    [] ->
                        #mafia_day{key = Key,
                                   thread_id = M#message.thread_id,
                                   day = DayNum,
                                   votes = [],
                                   complete = false
                                  };
                    [Day2] -> Day2
                end,
            Votes = Day#mafia_day.votes,
            Votes2 = case lists:keyfind(User, 1, Votes) of
                         false ->
                             [{User, [NewVote]} | Votes];
                         {User, UVotes} ->
                             UVotes2 = [NewVote | UVotes],
                             lists:keystore(User, 1, Votes, {User, UVotes2})
                     end,
            mnesia:dirty_write(Day#mafia_day{votes = Votes2});
        _ ->
            ignore
    end.

cmp_vote_raw() ->
    ThId = get_kv(thread_id),
    DayNum = 1,
    Key = {ThId, DayNum},
    case mnesia:dirty_read(mafia_day, Key) of
        [] ->
            ignore;
        [#mafia_day{votes = GVotes}] ->
            [begin
                 VoteSum = [{b2l(V#vote.vote), b2l(V#vote.raw), V#vote.valid} || V <- Votes],
                 {b2l(User), VoteSum}
             end || {User, Votes} <- GVotes]
    end.

print_votes(ThId, DayNum) ->
    Key = {ThId, DayNum},
    case mnesia:dirty_read(mafia_day, Key) of
        [] ->
            ignore;
        [#mafia_day{votes = Votes}] ->
            VoteSummary =
                lists:foldl(
                  fun({User, UserVotes}, Acc) ->
                          case lists:dropwhile(
                                 fun(V) -> not V#vote.valid end,
                                 UserVotes) of
                              [V|_] ->
                                  Vote = V#vote.vote,
                                  add_vote(Vote, V#vote.time, User, Acc);
                              [] -> Acc
                          end
                  end,
                  [],
                  Votes),
            %% Sort sumary on number of received votes
            GtEq = fun(A, B) -> element(2, A) >= element(2, B) end,
            VoteSum2 = lists:sort(GtEq, VoteSummary),
            io:format("Votes day ~p\n"
                      "------------\n", [DayNum]),
            [begin
                 Voters = [Voter || {_VoteTime, Voter} <- lists:sort(VoteInfos)],
                 io:format("~s - ~p - ", [b2l(Vote), N]),
                 VotersInTimeOrder =
                     [b2l(Voter) || Voter <- Voters],
                 io:format("~s\n", [string:join(VotersInTimeOrder, ", ")])
             end || {Vote, N, VoteInfos} <- VoteSum2],
            print_votes(ThId, DayNum + 1)
    end.

add_vote(Vote, Time, User, Acc) ->
    case lists:keyfind(Vote, 1, Acc) of
        false ->
            [{Vote, 1, [{Time, User}]} | Acc];
        {_, _NumV, Voters} ->
            Voters2 = Voters ++ [{Time, User}],
            NumV2 = length(Voters2),
            lists:keystore(Vote, 1, Acc, {Vote, NumV2, Voters2})
    end.

waste_spaces(L) -> [E || E <- L, E /= $\s].

rank(Players, RestUC) ->
    RestUCW = waste_spaces(RestUC),
    F = fun(P) ->
                PlayerUCW = string:to_upper(waste_spaces(b2l(P))),
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
              [b2l(M#message.user_name),
               i2l(M#message.page_num),
               print_time(M#message.time),
               i2l(M#message.thread_id),
               i2l(M#message.msg_id),
               b2l(M#message.message)
              ]).

print_message_summary(M) ->
    Msg = fix_sum(b2l(M#message.message)),
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
              [string:left(b2l(M#message.user_name), 12),
               i2l(M#message.page_num),
               print_time(M#message.time),
               i2l(M#message.msg_id),
               MsgShort
              ]).

%% half this fun should go to mafia_time
print_time(Time) when is_integer(Time) ->
    {TzH, Dst} = mafia_time:get_tz_dst(),
    print_time(Time, TzH, Dst).

print_time(Time, TzH, Dst) when is_integer(Time) ->
    try
        {{Y, M, D}, {HH,MM,SS}} = mafia_time:local_datetime_for_secs1970(Time, TzH, Dst),
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

p(I) when I > 9 -> i2l(I);
p(I) -> string:right(i2l(I), 2, $0).

rm_to_after(Str, []) -> Str;
rm_to_after(Str, [Search|T]) when is_list(Search) ->
    rm_to_after(rm_to_after(Str, Search), T);
% return string()
rm_to_after(Str, Search) ->
    element(2, rm_to_after_pos(Str, Search)).

rm_to_after_pos(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {0, ""};
        P -> {P, get_after_pos(P, length(Search), Str)}
    end.

get_after_pos(P, Len, Str) ->
    lists:nthtail(P - 1 + Len, Str).

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
