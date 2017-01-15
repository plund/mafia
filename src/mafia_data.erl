-module(mafia_data).

%% Manual
-export([refresh_messages/0,
         refresh_messages/1,
         refresh_messages/2,
         refresh_votes/0,
         refresh_votes/1,
         refresh_stat/0
        ]).

%% interface
-export([downl/0, % Human
         downl/1, % Human
         downl_web/1  % from web
        ]).

%% library
-export([
         rm_to_after_pos/2,
         get_after_pos/3,
         sum_stat/2
        ]).

%% utilities
-export([update_stat/1,
         compress_txt_files/0,
         grep/1, grep/2,
         iterate_all_msgs/2
         %%iterate_all_msg_ids/3,
        ]).

-include("mafia.hrl").

-type last_iter_msg_ref() :: none |
                             {ThId :: integer(),
                              Page :: integer(),
                              MsgId :: integer(),
                              MsgTime :: seconds1970()}.

-record(s,
        {page_to_read :: page_num(),  %% either page num to get and when got the
         %%                      actual page num
         is_last_page :: boolean(),
         body_on_file = false :: boolean(),
         page_last_read :: page_num(),
         page_total_last_read :: page_num(),
         thread_id :: thread_id(),
         url :: string(),
         body :: string(),
         utc_time :: seconds1970(),
         check_vote_fun :: function(),
         dl_time :: ?undefined | millisecs(),
         do_refresh_msgs = false :: boolean(),
         last_msg_time
        }).

%% Download any thread
-spec downl() -> ok.
downl() ->
    downl2(#s{}).

-spec downl(do_refresh_msgs) -> ok.
downl(S = #s{}) -> downl2(S);
downl(do_refresh_msgs) ->
    downl2(#s{do_refresh_msgs = true}).

downl2(S) when S#s.thread_id == ?undefined ->
    downl2(S#s{thread_id = ?getv(?thread_id)});
downl2(S) when S#s.page_to_read == ?undefined ->
    downl2(S#s{page_to_read = ?getv(?page_to_read)});
downl2(S) ->
    mafia:setup_mnesia(),
    inets:start(),
    download(S),
    ok.

-spec download(#s{}) -> {ok | {error, Reason :: term()}, #s{}}.
download(S) when S#s.utc_time == ?undefined ->
    ThId = S#s.thread_id,
    S2 = case ?rgame(ThId) of
             [G] ->
                 LMT = G#mafia_game.last_msg_time,
                 S#s{check_vote_fun = check_vote_msgid_fun(ThId, LMT)};
             [] -> S
         end,
    download(
      S2#s{utc_time = mafia_time:utc_secs1970()});
download(S) ->
    case get_body(S#s{dl_time = ?undefined}) of
        {ok, S2} ->
            S3 = analyse_body(S2),
            if not S3#s.is_last_page ->
                    if not S3#s.body_on_file ->
                            timer:sleep(10000);
                       true -> ok
                    end,
                    download(S3);
               true -> {ok, S3}
            end;
        {error, _Error} = Err ->
            {Err, S}
    end.

%% Download a game thread
-spec downl_web(integer()) -> ok.
downl_web(GameKey) when is_integer(GameKey) ->
    downl_web(?rgame(GameKey));
downl_web([]) -> ok;
downl_web([G]) -> downl_web(G);
downl_web(G = #mafia_game{}) ->
    GameKey = G#mafia_game.key,
    InitPage = G#mafia_game.page_to_read,
    {Page, LMT2} = check_db(GameKey, InitPage, G#mafia_game.last_msg_time),
    case download(#s{thread_id = GameKey,
                     page_to_read = Page,
                     last_msg_time = LMT2
                    }) of
        {ok, S2} ->
            update_page_to_read(GameKey, S2#s.page_to_read, S2#s.last_msg_time);
        {{error, _R}, S2} ->
            update_page_to_read(GameKey, S2#s.page_to_read, S2#s.last_msg_time)
    end,
    ok.

%% Read DB for messages first
-spec check_db(Key::thread_id(),
               InitPage::integer(),
               LastMsgTime :: seconds1970()) ->
                      {PageNum :: integer(),
                       LastMsgTime :: seconds1970()}.
check_db(Key, P, LMT) ->
    MsgIdF = check_vote_msgid_fun(Key, LMT),
    RPageF = fun(P2) -> ?rpage(Key, P2) end,
    check_db(MsgIdF, RPageF, P, P, RPageF(P), LMT).

check_db(_MsgIdF, _RPageF, Start, Start, [], LMT) -> {Start, LMT};
check_db(_MsgIdF, _RPageF, P, _Start, [], LMT) -> {P - 1, LMT};
check_db(MsgIdF, RPageF, P, Start, [PageRec], LMT) ->
    LastMsgTime =
        lists:foldl(fun(MsgId, Acc) ->
                            R = MsgIdF(MsgId),
                            if is_integer(R) -> R;
                               true -> Acc
                            end
                    end,
                    LMT,
                    PageRec#page_rec.message_ids),
    NextP = P + 1,
    check_db(MsgIdF, RPageF, NextP, Start, RPageF(NextP), LastMsgTime).

update_page_to_read(GameKey, PageToRead, LastMsgTime)
  when is_integer(GameKey), is_integer(PageToRead) ->
    G = hd(?rgame(GameKey)),
    if PageToRead /= G#mafia_game.page_to_read;
       LastMsgTime /= G#mafia_game.last_msg_time ->
            mnesia:dirty_write(G#mafia_game{page_to_read = PageToRead,
                                            last_msg_time = LastMsgTime});
       true -> ok
    end.

refresh_messages() -> refresh_messages(?game_key).

refresh_messages(?game_key = K) -> refresh_messages(?getv(K));
refresh_messages(?thread_id = K) -> refresh_messages(?getv(K));
refresh_messages(ThId) -> refresh_messages(ThId, true).

-spec refresh_messages(ThId :: integer(), DoVotes :: boolean()) -> ok.
refresh_messages(ThId, DoVotes) ->
    ?set(?page_to_read, 1),
    mafia_db:reset_game(ThId), %% to reset last_msg_time...
    %% Remove messages for msg_ids found in page_rec of thread
    MsgIdFun = fun(MsgId) -> mnesia:dirty_delete(message, MsgId) end,
    iterate_all_msg_ids(ThId, MsgIdFun, all),

    %% Remove page_recs for thread
    [mnesia:dirty_delete(page_rec, K)
     || K = {ThId2, _P} <- mnesia:dirty_all_keys(page_rec), ThId2 == ThId],

    %% Populate tables message and page_rec again
    downl(#s{thread_id = ThId, page_to_read = 1}),

    if DoVotes ->
            refresh_votes(ThId);
       true -> ok
    end.

-spec refresh_votes() -> ok.
refresh_votes() ->
    ThId = ?getv(?game_key),
    clear_mafia_day_and_stat(ThId),
    refresh_votes(ThId, ?rgame(ThId), all, soft).

refresh_votes(?game_key = K) -> refresh_votes(?getv(K));
refresh_votes(?thread_id = K) -> refresh_votes(?getv(K));
refresh_votes(ThId) when is_integer(ThId) ->
    clear_mafia_day_and_stat(ThId),
    refresh_votes(ThId, ?rgame(ThId), all, soft);
refresh_votes(hard) ->
    ThId = ?getv(?game_key),
    clear_mafia_day_and_stat(ThId),
    ?dbg(hard_game_reset),
    %% Reinitialize the game table
    mafia_db:reset_game(ThId),
    refresh_votes(ThId, ?rgame(ThId), all);
refresh_votes({upto, EndPage}) when is_integer(EndPage) ->
    ThId = ?getv(?game_key),
    case curr_page_to_read(ThId) of
        no_game -> ok;
        PageNumG when is_integer(PageNumG) ->
            PageFilter =
                if PageNumG > EndPage ->
                        Method = soft,
                        clear_mafia_day_and_stat(ThId),
                        fun(Page) -> Page =< EndPage end;
                   true ->
                        Method = softer,
                        fun(Page) ->
                                PageNumG =< Page andalso
                                    Page =< EndPage
                        end
                end,
            refresh_votes(ThId, ?rgame(ThId), PageFilter, Method)
    end.

refresh_votes(_ThId, [], _F, _Method) ->
    ok;
refresh_votes(ThId, [G], PageFilter, soft) ->
    ?dbg(soft_game_reset),
    G2 = G#mafia_game{last_msg_time = ?undefined},
    refresh_votes_soft(ThId, G2, PageFilter);
refresh_votes(ThId, [G], PageFilter, softer) ->
    ?dbg(softer_game_reset),
    refresh_votes_soft(ThId, G, PageFilter).

refresh_votes_soft(ThId, G, PageFilter) ->
    G2 = G#mafia_game{
           players_rem = G#mafia_game.players_orig,
           player_deaths =
               [case D of
                    #death{} = D -> D#death{is_deleted = true};
                    Other -> Other
                end
                || D <- G#mafia_game.player_deaths]
          },
    mnesia:dirty_write(G2),
    refresh_votes(ThId, G2, PageFilter).

refresh_votes(_ThId, [], _F) ->
    ok;
refresh_votes(ThId, [G], PageFilter) ->
    refresh_votes(ThId, G, PageFilter);
refresh_votes(ThId, G = #mafia_game{}, PageFilter) ->
    MsgIdFun = check_vote_msgid_fun(ThId, G#mafia_game.last_msg_time),
    case iterate_all_msg_ids(ThId, MsgIdFun, PageFilter) of
        none ->
            ?dbg({last_iter_msg_ref, none}),
            ok;
        {ThId, PageNum, MsgId, MsgTime} ->
            ?dbg({last_iter_msg_ref, ThId, PageNum, MsgId}),
            M = hd(?rmess(MsgId)),
            if MsgTime /= M#message.time ->
                    ?dbg({refresh_votes, not_same, MsgTime, M#message.time});
               true -> ok
            end,
            update_page_to_read(ThId, PageNum, MsgTime)
    end,
    mafia_web:update_current(),
    ok.

%% MsgId and #message{} are ok
check_vote_msgid_fun(ThId, LMT) ->
    Cmds = case file:consult(mafia_file:cmd_filename(ThId)) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    DoCheck =
        fun(Msg) ->
                MsgId = Msg#message.msg_id,
                Resp = mafia_vote:check_for_vote(Msg),
                [erlang:apply(M, F, A) || #cmd{msg_id = MId,
                                               mfa = {M, F, A}} <- Cmds,
                                          MId == MsgId],
                Resp
        end,
    fun(Msg = #message{}) when LMT == ?undefined;
                               LMT < Msg#message.time ->
                    DoCheck(Msg);
       (MsgId) when is_integer(MsgId) ->
            case ?rmess(MsgId) of
                [Msg] when LMT == ?undefined; LMT < Msg#message.time ->
                    DoCheck(Msg);
                _ -> ignore
            end;
       (_) -> ignore
    end.

curr_page_to_read(ThId) when is_integer(ThId) ->
    case ?rgame(ThId) of
        [] -> no_game;
        [G] -> G#mafia_game.page_to_read
    end.

%% clear #mafia_day and #stat for this ThId
clear_mafia_day_and_stat(ThId) ->
    clear_mafia_day(ThId),
    clear_stat(ThId),
    ok.

clear_mafia_day(ThId) ->
    %% Remove mafia_days for ThId
    Res = [mnesia:dirty_delete(mafia_day, K)
           || K = {ThId2, _} <- mnesia:dirty_all_keys(mafia_day),
              ThId2 == ThId],
    ?dbg({clear_mafia_day, ThId, deleted, length(Res)}).

%% Remove stats for thread.
clear_stat(ThId) ->
    StatKeyCheck = fun(Id, K) -> case K of
                                     {_, Id} -> true;
                                     {_, Id, _} -> true;
                                     _ -> false
                                 end
                   end,
    Res = [mnesia:dirty_delete(stat, K)
           || K <- mnesia:dirty_all_keys(stat), StatKeyCheck(ThId, K)],
    ?dbg({clear_stat, ThId, deleted, length(Res)}).

refresh_stat() ->
    mnesia:clear_table(stat),
    ThId = ?getv(?game_key),
    refresh_stat(ThId, ?rgame(ThId)).

refresh_stat(_ThId, []) -> ok;
refresh_stat(ThId, [_G]) ->
    iterate_all_msg_ids(ThId, fun update_stat/1),
    ok.

update_stat(MsgId) when is_integer(MsgId) ->
    update_stat(MsgId, ?rmess(MsgId));
update_stat(M = #message{}) ->
    update_stat(M#message.msg_id, [M]).

update_stat(_MsgId, []) -> ok;
update_stat(MsgId, [M = #message{thread_id = ThId}]) ->
    update_stat(MsgId, M, ?rgame(ThId)).

update_stat(_MsgId, _M, []) -> ok;
update_stat(MsgId, M, [G = #mafia_game{}]) ->
    #message{thread_id = ThId,  %% :: thread_id(),
             user_name = UserB, %% :: user(),
             time = Time,       %% :: seconds1970(),
             message = MsgBin   %% :: message()
            } = M,
    %% UserB is in correct case in messages!
    UserUB = ?b2ub(UserB),
    Phase = mafia_time:calculate_phase(G, Time),
    Key1 = {UserUB, ThId},
    Key2 = {UserUB, ThId, Phase},
    Msg = ?b2l(MsgBin),
    Count = #stat{msg_ids = [MsgId],
                  num_chars = size(MsgBin),
                  num_words = length(string:tokens(Msg , " ,.\t\r\n")),
                  num_postings = 1
                 },
    update_stats_db(Key1, MsgId, Count),
    update_stats_db(Key2, MsgId, Count),
    ok.

update_stats_db(K, MsgId, Count) ->
    %% check_if_phase_record_exist
    update_stats_db(K, MsgId, Count, mnesia:dirty_read(stat, K)).

update_stats_db(K, MsgId, Count, []) ->
    mnesia:dirty_write(
      Count#stat{key = K,
                 msg_ids = [MsgId]});
update_stats_db(_K, MsgId, Count, [Stat = #stat{}]) ->
    %% check if msg_id already is in db
    %% if not add stats
    case lists:member(MsgId, Stat#stat.msg_ids) of
        true -> same;
        false -> mnesia:dirty_write(sum_stat(Count, Stat))
    end.

sum_stat(#stat{key = KA,
               msg_ids = MsgIdsA,
               num_chars = NChA,
               num_words = NWoA,
               num_postings = NPoA
              },
         #stat{key = KB,
               msg_ids = MsgIdsB,
               num_chars = NChB,
               num_words = NWoB,
               num_postings = NPoB
              }) ->
    Key = if KA == ?undefined -> KB;
             true -> KA
          end,
    MsgIds =
        case {is_list(MsgIdsA), is_list(MsgIdsB)} of
            {true, true} -> MsgIdsA ++ MsgIdsB;
            {false, true} -> [MsgIdsA | MsgIdsB];
            {true, false} -> [MsgIdsB | MsgIdsA];
            {false, false} -> [MsgIdsA, MsgIdsB]
        end,
    #stat{key = Key,
          msg_ids = MsgIds,
          num_chars = NChA + NChB,
          num_words = NWoA + NWoB,
          num_postings = NPoA + NPoB
         };
sum_stat(#prstat{key = KA,
                 msg_ids = MsgIdsA,
                 num_chars = NChA,
                 num_words = NWoA,
                 num_postings = NPoA
                },
         #prstat{key = KB,
                 msg_ids = MsgIdsB,
                 num_chars = NChB,
                 num_words = NWoB,
                 num_postings = NPoB
                }) ->
    Key = if KA == ?undefined -> KB;
             true -> KA
          end,
    MsgIds =
        case {is_list(MsgIdsA), is_list(MsgIdsB)} of
            {true, true} -> MsgIdsA ++ MsgIdsB;
            {false, true} -> [MsgIdsA | MsgIdsB];
            {true, false} -> [MsgIdsB | MsgIdsA];
            {false, false} -> [MsgIdsA, MsgIdsB]
        end,
    NewNumPosts = NPoA + NPoB,
    NewNumWords = NWoA + NWoB,
    #prstat{key = Key,
            msg_ids = MsgIds,
            num_chars = NChA + NChB,
            num_words = NewNumWords,
            num_postings = NewNumPosts,
            words_per_post = NewNumWords / NewNumPosts
           }.

grep(Str) -> grep(Str, summary).

grep(Str, s) -> grep(Str, summary);
grep(Str, f) -> grep(Str, full);
grep(Str, summary) ->
    grepI(Str, fun mafia_print:print_message_summary/1);
grep(Str, full) ->
    grepI(Str, fun mafia_print:print_message_full/1).

grepI(Str, PrintF)  ->
    ThId = ?getv(?thread_id),
    StrU = ?l2u(Str),
    GrepF =
        fun(M) ->
                Msg = ?b2l(M#message.message),
                MsgU = ?l2u(Msg),
                case string:str(MsgU, StrU) of
                    0 -> ok;
                    _P ->
                        PrintF(M)
                end
        end,
    iterate_all_msgs(ThId, GrepF),
    ok.

-spec iterate_all_msgs(ThId :: integer(),
                       MsgFun :: function())
                      -> term().
iterate_all_msgs(ThId, MsgFun) ->
    iterate_all_msgs(ThId, MsgFun, erlang:fun_info(MsgFun, arity)).

iterate_all_msgs(ThId, MsgFun, {arity,1}) ->
    Pages = mafia:pages_for_thread(ThId),
    F = fun(Page) ->
                [PR] = ?rpage(ThId, Page),
                Msgs = [hd(?rmess(MsgId)) || MsgId <- PR#page_rec.message_ids],
                lists:foreach(MsgFun, Msgs),
                {ThId, Page}
        end,
    [F(P) || P <- Pages],
    ok;
iterate_all_msgs(ThId, MsgFun, {arity,2}) ->
    Pages = mafia:pages_for_thread(ThId),
    MsgIds = lists:foldl(
               fun(P, Acc) ->
                       [#page_rec{message_ids = MsgIds}] =
                           ?rpage(ThId, P),
                       Acc ++ MsgIds
               end,
               [],
               Pages),
    lists:foldl(fun(MsgId, Acc2) ->
                        MsgFun(hd(?rmess(MsgId)), Acc2)
                end,
                MsgFun(acc, init),
                MsgIds).

%% Iterate through all message ids in one thread in time order
iterate_all_msg_ids(ThId, Fun) ->
    iterate_all_msg_ids(ThId, Fun, all).


%% If the MsgIdFun returns an integer, the last returned integer
%% will also be returned from this fun as the MsgTime
-spec iterate_all_msg_ids(ThId :: integer(),
                          MsgIdFun :: function(),
                          PageFilter:: all | function())
                         -> last_iter_msg_ref().
iterate_all_msg_ids(ThId, MsgIdFun, Filter) ->
    All = mafia:pages_for_thread(ThId),
    Pages = if Filter == all -> All;
               is_function(Filter) ->
                    lists:filter(Filter, All)
            end,
    %% get last page num
    iterate_all_msg_idsI(ThId, MsgIdFun, Pages).

%% Return reference to last iterated message
-spec iterate_all_msg_idsI(ThId :: integer(),
                           MsgIdFun :: function(),
                           PageNums :: [integer()])
                          -> last_iter_msg_ref().
iterate_all_msg_idsI(ThId, MsgIdFun, PageNums) ->
    PageMsgIds = mafia_lib:all_msgids(ThId, PageNums),
    {LastPage, LastMsgId} = lists:last(PageMsgIds),
    LastMsgTime =
        lists:foldl(fun({_, MId}, Acc) ->
                            R = MsgIdFun(MId),
                            if is_integer(R) -> R; true -> Acc end
                    end,
                    none,
                    PageMsgIds),
    {ThId, LastPage, LastMsgId, LastMsgTime}.

%% -----------------------------------------------------------------------------

-spec get_body(S :: #s{}) -> {ok, #s{}} | {error, term()}.
get_body(S) ->
    get_body(S, get_body_from_file(S)).

-spec get_body(#s{}, term()) -> {ok, Body::term()} | {error, term()}.
get_body(S, {file, Body}) ->
    S2 = check_this_page(S#s{body = Body,
                             body_on_file = true}),
    {ok, S2};
get_body(S, no_file) ->
    S2 = make_url(S#s{body_on_file = false}),
    get_body2(S2, http_request(S2)).

make_url(S) ->
    Url = ?UrlBeg ++ ?i2l(S#s.thread_id) ++ ?UrlMid ++ ?i2l(S#s.page_to_read)
        ++ ?UrlEnd,
    S#s{url = Url}.

-spec get_body2(#s{}, term()) -> {ok, Body::term()} | {error, term()}.
get_body2(_S2, {error, _} = Error) -> Error;
get_body2(S2, {ok, Body}) ->
    Body2 = get_thread_section(Body),
    S3 = check_this_page(S2#s{body=Body2}),
    if not S3#s.is_last_page -> % page complete > STORE IT!
            store_page(S3, Body2);
       true -> ok
    end,
    {ok, S3}.

get_thread_section(Body) ->
    ThId = ?getv(?thread_id),
    ThStartStr = "<div class=\"thread threadID" ++ ?i2l(ThId),
    B2 = rm_to_after(Body, ThStartStr),
    ThEndStr = "<div class=\"thread thread",
    {_, ThreadStr} = read_to_before(B2, ThEndStr),
    ThreadStr.

http_request(S2) ->
    ?inc_cnt(http_requests),
    A = erlang:monotonic_time(millisecond),
    case httpc:request(S2#s.url) of
        {ok, {_StatusLine, _Headers, Body}} ->
            ?inc_cnt(http_responses),
            B = erlang:monotonic_time(millisecond),
            ?dbg({download_wait_ms, B - A}),
            {ok, Body};
        {ok, {_StatusCode, Body}} ->
            ?inc_cnt(http_responses),
            B = erlang:monotonic_time(millisecond),
            ?dbg({download_wait_ms, B - A}),
            {ok, Body};
        {ok, _ReqId} ->
            ?inc_cnt(http_errors),
            {error, no_body};
        {error, _Reason} ->
            ?inc_cnt(http_errors),
            {error, http_req}
    end.

get_body_from_file(S) ->
    %% Do we have body on file?
    %% we only store complete pages on file!
    %% Store full pages as compressed files:
    %%     thread_pages/m24_threadid_page.txt.tgz
    {_FileName, TarBallName} = th_filenames_read(S),
    case erl_tar:extract(TarBallName, [memory, compressed]) of
        {ok, [{_, BodyBin}]} ->
            %% io:format("Found page ~p on file\n",[S#s.page_to_read]),
            {file, ?b2l(BodyBin)};
        {error, {TarBallName, enoent}} ->
            %% io:format("Did NOT find ~p on file\n",[S#s.page_to_read]),
            no_file;
        Unexp ->
            io:format("Did NOT find ~p on file ~p\n",[S#s.page_to_read, Unexp]),
            no_file
    end.

%% -> ok | {error, Reason}
store_page(S, Body) ->
    {FileName, TarBallName} = th_filenames_store(S),
    case file:read_file_info(TarBallName) of
        {error, enoent} ->
            file:write_file(FileName, Body),
            erl_tar:create(TarBallName, [FileName], [compressed, verbose]),
            file:delete(FileName);
        _ ->
            {error, efileexist}
    end.

th_filenames_read(S) ->
    mafia_file:th_filenames(S#s.thread_id, S#s.page_to_read).

th_filenames_store(S) ->
    mafia_file:th_filenames(S#s.thread_id, S#s.page_last_read).

%% -----------------------------------------------------------------------------

%% compressed 1656 K data in less than 0.09 sec
compress_txt_files() ->
    Dir = "thread_pages",
    {ok, Files} = file:list_dir(Dir),
    Files2 = [ filename:join(Dir, F)
               || F = "m24_" ++ _ <- Files,
                  case ?lrev(F) of
                        "txt." ++ _ -> true;
                        _ -> false
                    end],
    [erl_tar:create(F++".tgz", [F], [compressed, verbose]) || F <- Files2],
    [file:delete(F)|| F <- Files2].

%% -----------------------------------------------------------------------------

%% find page nums "Page 177 of 177", if it exists
%% sets #s.is_last_page,
%%      #s.page_last_read = #s.page_to_read = PageLastRead,
%%      #s.page_total_last_read = PageTotal
-spec check_this_page(S :: #s{}) -> #s{}.
check_this_page(S) ->
    {_, Head} = read_to_before(S#s.body, "class=\"message-head"),
    %%<em>Page <strong>177</strong> of <strong>177</strong>
    CurPage = S#s.page_to_read,
    {PageLastRead, PageTotal} =
        case rm_to_after(Head, ["<em>Page <strong>"]) of
            "" -> {CurPage, CurPage};
            B2 ->
                {B3, PageStr} = read_to_before(B2, "</strong>"),
                LastRead = ?l2i(PageStr),
                B4 = rm_to_after(B3, ["</strong> of <strong>"]),
                {_B5, PageTotStr} = read_to_before(B4, "</strong>"),
                Total = ?l2i(PageTotStr),
                {LastRead, Total}
        end,
    IsLastPage = if PageLastRead == PageTotal -> true;
                    true -> false
                 end,
    PageToRead = case IsLastPage of
                     true ->
                         %% ?set(?page_to_read, PageLastRead),
                         PageLastRead;
                     false ->
                         ?set(?page_to_read, PageLastRead + 1),
                         %% ?set(?page_complete, PageLastRead),
                         PageLastRead + 1
                 end,
    S#s{is_last_page = IsLastPage,
        page_last_read = PageLastRead,
        page_to_read = PageToRead,
        page_total_last_read = PageTotal}.

%% -----------------------------------------------------------------------------

analyse_body(S = #s{body = ""}) -> S;
analyse_body(S) ->
    Body = S#s.body,
    B3 = rm_to_after(Body, ["<div class=\"reply",
                            "<div class=\"message-head",
                            "profile.php?user", ">"]),
    {B4, UserRaw} = read_to_before(B3, "<"),
    UserStr = strip(UserRaw),
    B4a = rm_to_after(B4, "messageID=\""),
    {B4a2, MsgIdStr} = read_to_before(B4a, "\""),
    B4b = rm_to_after(B4a2, "unixtime=\""),
    {B4c, TimeStr} = read_to_before(B4b, "\""),
    B5 = rm_to_after(B4c, ["<div class=\"message-contents\"", ">"]),
    {B6, MsgRaw} = read_to_before(B5, "</div>"),
    Msg = strip(MsgRaw),

    analyse_body(S#s{body = B6}, {UserStr, MsgIdStr, TimeStr, Msg}).

analyse_body(S, {"", _MsgIdStr, _TimeStr, _Msg}) -> S;
analyse_body(S, {UserStr, MsgIdStr, TimeStr, Msg}) ->
    analyse_body(S, ?l2b(UserStr), ?l2i(MsgIdStr), ?l2i(TimeStr), Msg).

analyse_body(S, _User, _MsgId, Time, _Msg)
  when Time > S#s.utc_time ->
    %% ?dbg({analyse_body, utc_time}),
    PageLastRead = S#s.page_last_read,
    ?set(?page_to_read, PageLastRead),
    S#s{is_last_page = true,
        page_to_read = PageLastRead
       };
analyse_body(S, User, MsgId, Time, Msg) ->
    CheckVote = S#s.check_vote_fun,
    S2 = case update_page_rec(S, MsgId) of
             ?unchanged ->
                 S;
             A when A == ?new_page; A== ?add_id ->
                 MsgR = write_message_rec(S, MsgId, User, Time, Msg),
                 mafia_vote:verify_msg_user(MsgR),
                 if is_function(CheckVote) -> CheckVote(MsgR);
                    true -> ok
                 end,
                 mafia_print:print_message_summary(MsgR),
                 S#s{last_msg_time = Time}
         end,
    analyse_body(S2).

%% -----------------------------------------------------------------------------

rm_to_after(Str, []) -> Str;
rm_to_after(Str, [Search|T]) when is_list(Search) ->
    rm_to_after(rm_to_after(Str, Search), T);
%% return string()
rm_to_after(Str, Search) ->
    element(2, rm_to_after_pos(Str, Search)).

rm_to_after_pos(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {0, ""};
        P -> {P, get_after_pos(P, length(Search), Str)}
    end.

get_after_pos(P, Len, Str) ->
    lists:nthtail(P - 1 + Len, Str).

%% -----------------------------------------------------------------------------

%% return  {NewStr, ReadText}
read_to_before(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {"", ""};
        P ->
            Str2 = lists:nthtail(P-1, Str),
            Read = string:left(Str, P-1),
            {Str2, Read}
    end.

%% -----------------------------------------------------------------------------

-spec update_page_rec(#s{}, msg_id()) -> ?new_page | ?add_id | ?unchanged.
update_page_rec(S, MsgIdInt) ->
    {Action, PageRec} =
        case ?rpage(S#s.thread_id, S#s.page_last_read) of
            [] -> {?new_page,
                   #page_rec{key = {S#s.thread_id, S#s.page_last_read},
                             message_ids = [MsgIdInt],
                             thread_id = S#s.thread_id,
                             complete = false}};
            [P = #page_rec{message_ids = MsgIds, complete = Comp}] ->
                IsMember = lists:member(MsgIdInt, MsgIds),
                MsgIds2 = MsgIds ++
                    case IsMember of
                        false -> [MsgIdInt];
                        true -> []
                    end,
                Act = if IsMember -> ?unchanged;
                         not IsMember -> ?add_id
                      end,
                Comp2 = Comp orelse not S#s.is_last_page,
                {Act, P#page_rec{message_ids = MsgIds2,
                                 complete = Comp2}}
        end,
    mnesia:dirty_write(PageRec),
    Action.

write_message_rec(S, MsgIdInt, User, Time, Msg) ->
    mnesia:dirty_write(
      M = #message{msg_id = MsgIdInt,
                   thread_id = S#s.thread_id,
                   page_num = S#s.page_last_read,
                   user_name = User,
                   time = Time,
                   message = ?l2b(Msg)
                  }
     ),
    M.

strip(Str) ->
    t_strip(h_strip(Str)).

h_strip([H|T]) when H =< $\s -> strip(T);
h_strip(Str) -> Str.

t_strip(Str) ->
    ?lrev(h_strip(?lrev(Str))).
