-module(mafia_data).

%% Manual
-export([refresh_messages/0,
         refresh_messages/1,
         refresh_votes/0,
         refresh_stat/0
        ]).

%% interface
-export([man_downl/0, % Human
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

-type last_iter_msg_ref() :: ?none |
                             {ThId :: integer(),
                              Page :: integer(),
                              MsgId :: integer(),
                              MsgTime :: seconds1970()}.

-record(s,
        {page_to_read :: ?undefined | page_num(),
         %% either page num to get and when got the
         %% actual page num
         is_last_page :: ?undefined | boolean(),
         body_on_file = false :: boolean(),
         page_last_read :: ?undefined |  page_num(),
         page_total_last_read :: ?undefined | page_num(),
         thread_id :: ?undefined | thread_id(),
         url :: ?undefined | string(),
         body :: ?undefined | string(),
         utc_time :: ?undefined | seconds1970(),
         check_vote_fun :: ?undefined | function(),
         dl_time :: ?undefined | millisecs(),
         do_refresh_msgs = false :: boolean(),
         last_msg_time,
         last_msg_id
        }).

%% MANUAL
%% Download any thread
%% set first: ?thread_id
%%       ?page_to_read
-spec man_downl() -> ok.
man_downl() ->
    downl2(#s{}).

-spec downl(#s{} | do_refresh_msgs) -> ok.
downl(S = #s{}) -> downl2(S);
downl(do_refresh_msgs) ->
    downl2(#s{do_refresh_msgs = true}).

downl2(S) when S#s.thread_id == ?undefined ->
    downl2(S#s{thread_id = ?getv(?thread_id)});
downl2(S) when S#s.page_to_read == ?undefined ->
    downl2(S#s{page_to_read = ?getv(?page_to_read)});
downl2(S) ->
    download(S),
    ok.

-spec download(#s{}) -> {ok | {error, Reason :: term()}, #s{}}.
download(S) when S#s.utc_time == ?undefined ->
    ThId = S#s.thread_id,
    S2 = case ?rgame(ThId) of
             [G] ->
                 LMI = G#mafia_game.last_msg_id,
                 LMT = G#mafia_game.last_msg_time,
                 S#s{check_vote_fun = check_vote_msgid_fun(ThId, LMI, LMT)};
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
-spec downl_web(integer() | #mafia_game{} | [#mafia_game{}]) -> ok.
downl_web(GameKey) when is_integer(GameKey) ->
    downl_web(?rgame(GameKey));
downl_web([]) -> ok;
downl_web([G]) -> downl_web(G);
downl_web(G = #mafia_game{}) ->
    GameKey = G#mafia_game.key,
    LMI = G#mafia_game.last_msg_id,
    LMT = G#mafia_game.last_msg_time,
    S = #s{utc_time = mafia_time:utc_secs1970(),
           check_vote_fun = check_vote_msgid_fun(GameKey, LMI, LMT),
           thread_id = GameKey,
           page_to_read = G#mafia_game.page_to_read,
           last_msg_id = LMI,
           last_msg_time = LMT
          },
    ?dbg({down_web, S#s.page_to_read, LMT}),
    {Page, LMI2, LMT2} = check_db(G),
    case download(S#s{check_vote_fun =
                          check_vote_msgid_fun(GameKey, LMI2, LMT2),
                      page_to_read = Page,
                      last_msg_id = LMI2,
                      last_msg_time = LMT2
                     }) of
        {_, S3} ->
            update_page_to_read(GameKey,
                                S3#s.page_to_read,
                                S3#s.last_msg_id,
                                S3#s.last_msg_time)
    end,
    ok.

check_db(G) ->
    InitPage = G#mafia_game.page_to_read,
    {MsgIdFun, Acc} = checkvote_fun(G, true),
    Filter = fun(Page) -> Page >= InitPage end,
    {_, PageNum, MsgId, MsgTime} =
        iter_msgids(G#mafia_game.key, MsgIdFun, Acc, Filter),
    {PageNum, MsgId, MsgTime}.

update_page_to_read(GameKey, PageToRead, LastMsgId, LastMsgTime)
  when is_integer(GameKey), is_integer(PageToRead) ->
    G = hd(?rgame(GameKey)),
    if PageToRead /= G#mafia_game.page_to_read;
       LastMsgId /= G#mafia_game.last_msg_id ->
            ?dwrite_game(G#mafia_game{page_to_read = PageToRead,
                                      last_msg_id = LastMsgId,
                                      last_msg_time = LastMsgTime});
       true -> ok
    end.

refresh_messages() -> refresh_messages(?game_key).

-spec refresh_messages(ThId :: integer() | ?game_key | ?thread_id) -> ok.
refresh_messages(?game_key = K) -> refresh_messages(?getv(K));
refresh_messages(?thread_id = K) -> refresh_messages(?getv(K));
refresh_messages(Id) ->
    ThId = ?thid(Id),
    ?set(?page_to_read, 1), %% Is this really needed ?

    %% Delete mafia_game
    mafia_db:reset_game(ThId), %% resets last_msg_id, last_msg_time...

    %% Delete mafia_day
    _ = [mnesia:dirty_delete(mafia_day, K)
         || K = {Th,_} <- mnesia:dirty_all_keys(mafia_day),
            Th == ThId],
    %% Delete messages for msg_ids found in page_rec of thread
    MsgIdFun = fun(MsgId) -> mnesia:dirty_delete(message, MsgId) end,
    iterate_all_msg_ids(ThId, MsgIdFun, all),

    %% Delete page_recs for thread
    [mnesia:dirty_delete(page_rec, K)
     || K = {ThId2, _P} <- mnesia:dirty_all_keys(page_rec), ThId2 == ThId],

    %% Populate tables message and page_rec again
    downl(#s{thread_id = ThId, page_to_read = 1}).

    %% if DoVotes ->
    %%         refresh_votes();
    %%    true -> ok
    %% end.

%% -spec refresh_votes() -> ok.
refresh_votes() ->
    ThId = ?getv(?game_key),
    clear_mafia_day_and_stat(ThId),
    ?dbg(hard_game_reset),
    %% Reinitialize the game table
    mafia_db:reset_game(ThId),
    refresh_votes(ThId, ?rgame(ThId), all).

refresh_votes(_ThId, [], _F) ->
    ok;
refresh_votes(ThId, [G], PageFilter) ->
    refresh_votes(ThId, G, PageFilter);
refresh_votes(ThId, G = #mafia_game{}, PageFilter) ->
    {MsgIdFun, Acc} = checkvote_fun(G, false),
    case iter_msgids(ThId, MsgIdFun, Acc, PageFilter) of
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
            update_page_to_read(ThId, PageNum, MsgId, MsgTime)
    end,
    mafia_web:update_current(),
    ok.

-record(acc, {thid,
              last_page,
              last_msg_id,
              last_msg_time,
              dls}).

%% MsgId and #message{} are ok
checkvote_fun(G, DoPrint) ->
    %% iterate_all_msg_ids
    Cmds = case file:consult(mafia_file:cmd_filename(G#mafia_game.key)) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    REs = mafia_vote:get_regexs(),
    DoCheck =
        fun(Msg) ->
                MsgId = Msg#message.msg_id,
                Resp = mafia_vote:check_cmds_votes(REs, Msg),
                [erlang:apply(M, F, A) || #cmd{msg_id = MId,
                                               mfa = {M, F, A}} <- Cmds,
                                          MId == MsgId],
                Resp
        end,
    GameKey = G#mafia_game.key,
    LMI = G#mafia_game.last_msg_id,
    LMT = G#mafia_game.last_msg_time,
    DLs = if LMT == ?undefined -> ?lrev(G#mafia_game.deadlines);
             true ->
                  lists:dropwhile(fun(#dl{time = DT}) -> DT =< LMT end,
                                  ?lrev(G#mafia_game.deadlines))
          end,
    {fun(report, A) ->
             {A#acc.thid, A#acc.last_page, A#acc.last_msg_id,
              A#acc.last_msg_time};
        (MsgId, Acc) when Acc#acc.last_msg_time == ?undefined ->
             [Msg] = ?rmess(MsgId),
             MsgTime = Msg#message.time,
             Acc#acc{last_page = Msg#message.page_num,
                     last_msg_id = MsgId,
                     last_msg_time = MsgTime};
        (MsgId, Acc) ->
             [Msg] = ?rmess(MsgId),
             MsgTime = Msg#message.time,
             if MsgTime >= Acc#acc.last_msg_time,
                MsgId /= Acc#acc.last_msg_id ->
                     DoCheck(Msg),
                     if DoPrint ->
                             mafia_print:print_message_summary(Msg);
                        true -> ok
                     end,
                     NextDLs =
                         if [] == Acc#acc.dls -> [];
                            true ->
                                 PrevMsgT = Acc#acc.last_msg_time,
                                 DL = hd(Acc#acc.dls),
                                 DeadT = DL#dl.time,
                                 if PrevMsgT < DeadT, DeadT =< MsgTime ->
                                         %% dl passed generate
                                         gen_hist_and_get_dls(MsgTime, Acc);
                                    MsgTime < DeadT -> Acc#acc.dls
                                 end
                         end,
                     Acc#acc{last_page = Msg#message.page_num,
                             last_msg_id = MsgId,
                             last_msg_time = MsgTime,
                             dls = NextDLs};
                true ->
                     Acc
             end
     end,
     #acc{thid = GameKey,
          last_page = G#mafia_game.page_to_read,
          last_msg_id = LMI,
          last_msg_time = LMT,
          dls = DLs
         }
    }.

gen_hist_and_get_dls(NextT, Acc) ->
    mafia_web:regen_history(NextT, Acc#acc.thid),
    [G] = ?rgame(Acc#acc.thid),
    lists:dropwhile(fun(#dl{time = DT}) -> DT =< NextT end,
                    ?lrev(G#mafia_game.deadlines)).

%% MsgId and #message{} are ok
check_vote_msgid_fun(ThId, LMI, LMT) ->
    Cmds = case file:consult(mafia_file:cmd_filename(ThId)) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    REs = mafia_vote:get_regexs(),
    DoCheck =
        fun(Msg) ->
                MsgId = Msg#message.msg_id,
                Resp = mafia_vote:check_cmds_votes(REs, Msg),
                [erlang:apply(M, F, A) || #cmd{msg_id = MId,
                                               mfa = {M, F, A}} <- Cmds,
                                          MId == MsgId],
                Resp
        end,
    fun(Msg = #message{}) when Msg#message.time >= LMT,
                               Msg#message.msg_id /= LMI ->
            DoCheck(Msg);
       (MsgId) when is_integer(MsgId) ->
            case ?rmess(MsgId) of
                [Msg] when LMI /= Msg#message.msg_id ->
                    DoCheck(Msg);
                _ -> ignore
            end;
       (_) -> ignore
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
    %% UserUB = ?b2ub(UserB),
    Phase = case mafia_time:calculate_phase(G, Time) of
                #phase{don = ?game_ended} -> ?game_ended;
                #phase{num = Num, don = DoN} -> {Num, DoN}
            end,
    Key1 = {UserB, ThId},
    Key2 = {UserB, ThId, Phase},
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
    ?dwrite_stat(
       Count#stat{key = K,
                  msg_ids = [MsgId]});
update_stats_db(_K, MsgId, Count, [Stat = #stat{}]) ->
    %% check if msg_id already is in db
    %% if not add stats
    case lists:member(MsgId, Stat#stat.msg_ids) of
        true -> same;
        false -> ?dwrite_stat(sum_stat(Count, Stat))
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
            {true, true} ->
                MsgIdsA ++ MsgIdsB
                %% Dialyzer says the 3 next cannot happen
                %% {false, true} -> [MsgIdsA | MsgIdsB];
                %% {true, false} -> [MsgIdsB | MsgIdsA];
                %% {false, false} -> [MsgIdsA, MsgIdsB]
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
    iter_msgids(ThId, MsgIdFun, no_acc, Filter).

iter_msgids(ThId, MsgIdFun, Acc, PageFilter) ->
    All = mafia:pages_for_thread(ThId),
    Pages = if PageFilter == all ->
                    All;
               is_function(PageFilter) ->
                    lists:filter(PageFilter, All)
            end,
    %% get last page num
    iterate_all_msg_idsI(ThId, MsgIdFun, Pages, Acc,
                         erlang:fun_info(MsgIdFun, arity)).

%% Return reference to last iterated message
-type arity2resp() :: term().
-spec iterate_all_msg_idsI(ThId :: integer(),
                           MsgIdFun :: function(),
                           PageNums :: [integer()],
                           Acc :: term(),
                           {arity, integer()}
                          )
                          -> last_iter_msg_ref() | arity2resp().
iterate_all_msg_idsI(ThId, MsgIdFun, PageNums, Acc, {arity, Ar}) ->
    case mafia_lib:all_msgids(ThId, PageNums) of
        [] -> ?none;
        PageMsgIds ->
            {LastPage, LastMsgId} = lists:last(PageMsgIds),
            if Ar == 1 ->
                    LastMsgTime =
                        lists:foldl(
                          fun({_, MId}, Acc2) ->
                                  R = MsgIdFun(MId),
                                  if is_integer(R) -> R;
                                     true -> Acc2 end
                          end,
                          none,
                          PageMsgIds),
                    {ThId, LastPage, LastMsgId, LastMsgTime};
               Ar == 2 ->
                    AccOut =
                        lists:foldl(MsgIdFun, Acc,
                                    [Id || {_, Id} <- PageMsgIds]),
                    MsgIdFun(report, AccOut)
            end
    end.

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
            {file, ?b2l(BodyBin)};
        {error, {TarBallName, enoent}} ->
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
                         PageLastRead;
                     false ->
                         ?set(?page_to_read, PageLastRead + 1),
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
             A when A == ?new_page; A == ?add_id ->
                 MsgR = write_message_rec(S, MsgId, User, Time, Msg),
                 mafia_vote:verify_msg_user(MsgR),
                 if is_function(CheckVote) -> CheckVote(MsgR);
                    true -> ok
                 end,
                 mafia_print:print_message_summary(MsgR),
                 S#s{last_msg_id = MsgId,
                     last_msg_time = Time}
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

%% refactor tail_pos_and_len(Pos, MatchStr, String) -> string().
get_after_pos(P, Len, Str) when is_integer(Len) ->
    lists:nthtail(P - 1 + Len, Str);
get_after_pos(P, Search, Str) when is_list(Search) ->
    get_after_pos(P, length(Search), Str).

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
                {Act, MsgIds2} =
                    case lists:member(MsgIdInt, MsgIds) of
                        false -> {?add_id, MsgIds ++ [MsgIdInt]};
                        true -> {?unchanged, MsgIds}
                    end,
                Comp2 = Comp orelse not S#s.is_last_page,
                {Act, P#page_rec{message_ids = MsgIds2,
                                 complete = Comp2}}
        end,
    ?dwrite_page(PageRec),
    remove_duplicate_msgid(S, Action, MsgIdInt),
    Action.

%% @doc Possibly remove MsgId from page 1 when adding MsgId to page 2.
%% (webdiplomacy.net can have upto 48 messages on page 1 before
%% splitting them into the 2 first pages)
remove_duplicate_msgid(S = #s{page_last_read = 2}, Action, MsgId)
  when Action == ?add_id;
       Action == ?new_page ->
    case ?rpage(S#s.thread_id, 1) of
        [P = #page_rec{message_ids = MsgIds}] ->
            case lists:member(MsgId, MsgIds) of
                true ->
                    ?dbg({rm_duplicate_page1, MsgId}),
                    ?dwrite_page(P#page_rec{message_ids = MsgIds -- [MsgId]});
                false -> ok
            end;
        _ ->
            ok
    end;
remove_duplicate_msgid(_, _, _) ->
    ok.

write_message_rec(S, MsgIdInt, User, Time, Msg) ->
    ?dwrite_msg(
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
