-module(mafia_data).

%% Manual
-export([refresh_messages/0,
         refresh_messages/1,
         refresh_votes/0,
         refresh_votes/1,
         refresh_stat/0,
         refresh_stat/1
        ]).

%% interface
-export([man_downl/0, % Human
         man_downl/1, % Human
         downl_web/1, % from web
         downl_web/3  % from web
        ]).

%% library
-export([get_after_pos/3,
         sum_stat/2
        ]).

%% utilities
-export([update_stat/2,
         compress_txt_files/0,
         iterate_all_game_msgs/3,

         delete_game_data_in_other_tabs/1,
         reset_game/1
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
         game_rec :: ?undefined | #mafia_game{},
         game_num :: ?undefined | integer(),
         site = ?webDip :: site(),
         thread_id :: ?undefined | thread_id(),
         is_pregame = false :: boolean(),
         url :: ?undefined | string(),
         body :: ?undefined | string(),
         utc_time :: ?undefined | seconds1970(),
         check_vote_fun :: ?undefined | function(),
         dl_time :: ?undefined | millisecs(),
         do_refresh_msgs = false :: boolean(),
         last_msg_time,
         last_msg_id
        }).

%% -----------------------------------------------------------------------------
%% MANUAL
%% Download any thread
-spec man_downl() -> {ok | {error, Reason :: term()}, #s{}}.
man_downl() ->
    ThId = ?getv(?thread_id),
    Page = ?getv(?page_to_read),
    do_man_downl(ThId, Page).

man_downl(ThId) ->
    do_man_downl(ThId, 1).

do_man_downl(ThId, Page)
  when is_integer(ThId), is_integer(Page) ->
    Question =
        ?l2a("Do you want to download thread " ++ ?i2l(ThId) ++
                 " starting from page " ++ ?i2l(Page) ++ " (NO/yes)> "),
    Answer = io:get_line(Question),
    case string:to_upper(Answer) of
        "YES" ++ _ ->
            download(#s{thread_id = ThId,
                        page_to_read = Page
                       });
        _ ->
            no_download
    end.

%% -----------------------------------------------------------------------------

-spec download(#s{}) -> {ok | {error, Reason :: term()}, #s{}}.
%% sets #s.utc_time and #s.check_vote_fun
download(S) when S#s.utc_time == ?undefined ->
    S2 = case S#s.game_rec of
             ?undefined -> S;
             G ->
                 LMI = G#mafia_game.last_msg_id,
                 LMT = G#mafia_game.last_msg_time,
                 S#s{check_vote_fun =
                         check_vote_msgid_fun(G, LMI, LMT, S#s.is_pregame)}
         end,
    do_download(
      S2#s{utc_time = mafia_time:utc_secs1970()});
download(S) ->
    do_download(S).

%% The loop
do_download(S) ->
    case get_body(S#s{dl_time = ?undefined}) of
        {ok, S2} ->
            S3 = analyse_body(S2),
            if not S3#s.is_last_page ->
                    if not S3#s.body_on_file ->
                            timer:sleep(10000);
                       true -> ok
                    end,
                    do_download(S3);
               true -> {ok, S3}
            end;
        {error, _Error} = Err ->
            {Err, S}
    end.

%% -----------------------------------------------------------------------------
%% Automatic triggered downloads

-spec downl_web(integer()) -> ok.
downl_web(GNum) ->
    downl_web(GNum, ?undefined, ?undefined).

downl_web(GNum, DL, Offset) when is_integer(GNum) ->
    downl_web(?rgame(GNum), DL, Offset);
downl_web([], _, _) -> ok;
downl_web([G], DL, Offset) -> downl_web(G, DL, Offset);

downl_web(G = #mafia_game{game_num = GNum,
                          thread_id = GThId,
                          signup_thid = SuThId},
          DL, Offset)
  when is_integer(GThId); is_integer(SuThId) ->
    SystemTime1 = mafia_time:system_time_ms(),
    {ThId, IsPregame} =
        if is_integer(GThId) -> {GThId, false};
           is_integer(SuThId) -> {SuThId, true}
        end,
    Page = G#mafia_game.page_to_read,
    LMI = G#mafia_game.last_msg_id,
    LMT = G#mafia_game.last_msg_time,
    S0 = #s{utc_time = mafia_time:utc_secs1970(),
            game_rec = G,
            game_num = GNum,
            site = G#mafia_game.site,
            thread_id = ThId,
            is_pregame = IsPregame,
            page_to_read = Page
           },
    {Page2, LMI2, LMT2} = case check_db(S0) of
                              ?none -> {Page, LMI, LMT};
                              V -> V
                          end,
    S = S0#s{check_vote_fun = check_vote_msgid_fun(G, LMI2, LMT2, IsPregame),
             page_to_read = Page2,
             last_msg_id = LMI2,
             last_msg_time = LMT2
            },
    case do_download(S) of
        {_, S3} ->
            update_page_to_read(G#mafia_game.game_num,
                                S3#s.page_to_read,
                                S3#s.last_msg_id,
                                S3#s.last_msg_time)
    end,
    maybe_log_dl(DL, G#mafia_game.game_num, S3, SystemTime1, Offset),
    ok;
downl_web(#mafia_game{}, _, _) -> %% pre-game
    ok.

maybe_log_dl(?undefined, _, _, _, _) -> ok;
maybe_log_dl(DL = #dl{phase = #phase{}},
             GNum,
             S,
             SystemTime1,
             NtpOffsetSecs) ->
    SystemTime2 = mafia_time:system_time_ms(),
    OffsetMicro = trunc(NtpOffsetSecs * 1000000),
    DlInfo = #dl_poll_info{game_num = GNum,
                           page_to_read = S#s.page_to_read,
                           last_msg_id = S#s.last_msg_id,
                           last_msg_time = S#s.last_msg_time,
                           dl = DL,
                           offset = OffsetMicro,
                           time1 = SystemTime1,
                           time2 = SystemTime2
                          },
    mafia_file:dl_info_to_file(DlInfo).

check_db(S) ->
    InitPage = S#s.page_to_read,
    G = S#s.game_rec,
    {MsgIdFun, Acc} = checkvote_fun(G, true),
    Filter = fun(Page) -> Page >= InitPage end,
    case iter_msgids({S#s.thread_id, S#s.site}, MsgIdFun, Acc, Filter) of
        {_, PageNum, MsgId, MsgTime} ->
            {PageNum, MsgId, MsgTime};
        ?none -> %% unread thread - nothing in DB
            ?none
    end.

update_page_to_read(GNum, PageToRead, LastMsgId, LastMsgTime)
  when is_integer(GNum), is_integer(PageToRead) ->
    G = hd(?rgame(GNum)),
    if PageToRead /= G#mafia_game.page_to_read;
       LastMsgId /= G#mafia_game.last_msg_id ->
            ?dwrite_game(game_l1,
                         G#mafia_game{page_to_read = PageToRead,
                                      last_msg_id = LastMsgId,
                                      last_msg_time = LastMsgTime});
       true -> ok
    end.

%% -----------------------------------------------------------------------------

refresh_messages() -> refresh_messages(?game_key).

-spec refresh_messages(GNum :: integer() | ?game_key) -> ok.
refresh_messages(?game_key = K) -> refresh_messages(?getv(K));
refresh_messages(GNum) ->
    refresh_messagesI(?rgame(GNum)).

refresh_messagesI([G = #mafia_game{game_num = GNum,
                                   thread_id = ThId}])
  when is_integer(ThId) ->
    %% reset mafia_game to initial values
    G2 = reset_game(G),
    delete_game_data_in_other_tabs(G2),
    %% Populate tables message and page_rec again
    case download(#s{game_rec = G2,
                     game_num = GNum,
                     thread_id = ThId,
                     page_to_read = 1}) of
        {ok, _S} -> ok;
        {E = {error, _}, _S} -> E
    end.

delete_game_data_in_other_tabs(GNum) when is_integer(GNum) ->
    delete_game_data_in_other_tabs(?rgame(GNum));
delete_game_data_in_other_tabs([]) -> {error, no_game};
delete_game_data_in_other_tabs([G]) ->
    delete_game_data_in_other_tabs(G);
delete_game_data_in_other_tabs(#mafia_game{game_num = GNum,
                                           thread_id = ThId,
                                           site = Site}) ->
    %% Delete mafia_day
    _ = [mnesia:dirty_delete(mafia_day, K)
         || K = {GN, _} <- mnesia:dirty_all_keys(mafia_day), GN == GNum],

    %% Delete messages for msg_ids found in page_rec of thread
    MsgIdFun = fun(MsgId) -> mnesia:dirty_delete(message, MsgId) end,
    iterate_all_msg_ids({ThId, Site}, MsgIdFun, all),

    %% Delete page_recs for thread
    [mnesia:dirty_delete(page_rec, K)
     || K = {ThId2, _P}
            <- mnesia:dirty_all_keys(page_rec),
        ThId2 == ThId],

    %% Delete all stat data
    clear_stat(GNum).

%% Old game reset (having destroyed players_orig AND
%% having a mafia_db:make_game_rec/1 fun clause)
reset_game(#mafia_game{game_num = GNum}) when 24 =< GNum, GNum =< 29 ->
    mafia_db:reset_game(GNum),
    hd(?rgame(GNum));
%% New game reset
reset_game(G = #mafia_game{}) ->
    G2 = G#mafia_game{
           players_rem = G#mafia_game.players_orig,
           player_deaths = [],
           page_to_read = 1,
           game_end = ?undefined,
           last_msg_id = ?undefined,
           last_msg_time = ?undefined},
    G3 = mafia_time:initial_deadlines(G2),
    ?dwrite_game(game_l2, G3),
    G3.

%% -spec refresh_votes() -> ok.
refresh_votes() ->
    GNum = ?getv(?game_key),
    refresh_votes(GNum).

refresh_votes(all) ->
    AllGNums = mafia_lib:all_keys(mafia_game),
    T = fun() -> erlang:monotonic_time(millisecond) end,
    Start = T(),
    [refresh_votes(GNum) || GNum <- AllGNums],
    Stop = T(),
    {refresh_votes, [{milliseconds_used, Stop - Start},
                     {num_games, length(AllGNums)},
                     {games, AllGNums}]};
refresh_votes(GNum) when is_integer(GNum) ->
    clear_mafia_day_and_stat(GNum),
    refresh_votes(?rgame(GNum), all).

refresh_votes([], _F) ->
    ok;
refresh_votes([G], PageFilter) ->
    refresh_votes(G, PageFilter);
refresh_votes(#mafia_game{thread_id = ?undefined}, _) ->
    {error, game_not_running};
refresh_votes(G0 = #mafia_game{}, PageFilter) ->
    %% Reinitialize the game table
    G = reset_game(G0),
    ThId = G#mafia_game.thread_id,
    Site = G#mafia_game.site,
    {MsgIdFun, Acc} = checkvote_fun(G, false),
    case iter_msgids({ThId, Site}, MsgIdFun, Acc, PageFilter) of
        none ->
            ?dbg({last_iter_msg_ref, none}),
            ok;
        {_, PageNum, MsgId, MsgTime} ->
            ?dbg({last_iter_msg_ref, ThId, PageNum, MsgId}),
            M = hd(?rmess({MsgId, Site})),
            if MsgTime /= M#message.time ->
                    ?dbg({refresh_votes, not_same, MsgTime, M#message.time});
               true -> ok
            end,
            update_page_to_read(G#mafia_game.game_num,
                                PageNum,
                                MsgId,
                                MsgTime)
    end,
    ok.

-record(acc, {game_num,
              thid,
              last_page,
              last_msg_id,
              last_msg_time,
              dls}).

%% MsgId and #message{} are ok
checkvote_fun(G, DoPrint) ->
    GNum = G#mafia_game.game_num,
    Site = G#mafia_game.site,
    CommandFile = mafia_file:cmd_filename(G),
    Cmds = case file:consult(CommandFile) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    REs = mafia_vote:get_regexs(),
    IgnoredMsgs =
        [MId || #cmd{msg_id = MId,
                     mfa = {_, ignore_message, _}} <- Cmds],
    DoCheck =
        fun(Msg) ->
                MsgId = ?e1(Msg#message.msg_key),
                mafia:add_user(Msg#message.user_name, Site),
                G2 = hd(?rgame(GNum)),
                case lists:member(MsgId, IgnoredMsgs) of
                    false -> mafia_vote:check_cmds_votes(G2, REs, Msg);
                    true -> ok
                end,
                MFAs = [MFA || #cmd{msg_id = MId, mfa = MFA} <- Cmds,
                               MId == MsgId],
                [erlang:apply(M, F, A) || {M, F, A} <- MFAs],
                ok
        end,
    LMI = G#mafia_game.last_msg_id,
    LMT = G#mafia_game.last_msg_time,
    DLs = if LMT == ?undefined -> ?lrev(G#mafia_game.deadlines);
             true ->
                  lists:dropwhile(fun(#dl{time = DT}) -> DT =< LMT end,
                                  ?lrev(G#mafia_game.deadlines))
          end,
    {fun(report, A) ->
             {A#acc.game_num, A#acc.last_page, A#acc.last_msg_id,
              A#acc.last_msg_time};
        (MsgId, Acc) when Acc#acc.last_msg_time == ?undefined ->
             [Msg] = ?rmess({MsgId, Site}),
             MsgTime = Msg#message.time,
             Acc#acc{last_page = Msg#message.page_num,
                     last_msg_id = MsgId,
                     last_msg_time = MsgTime};
        (MsgId, Acc) ->
             [Msg] = ?rmess({MsgId, Site}),
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
%%% Here the logic for when reaching the deadline needs to be modified.
%%% 1 find out if the deadline is a day deadline (DDL)
%%% 2 for a DDL, do we have a msgid stored as the last one (DDL_LMI)
%%% 3 If DDL_LMI, check MsgId == DDL_LMI
%%%   else use DeadT as below/before
                                 if PrevMsgT < DeadT, DeadT =< MsgTime ->
                                         %% dl reached => generate
                                         gen_hist_and_get_dls(MsgTime, Acc);
                                    MsgTime < DeadT ->
                                         Acc#acc.dls;
                                    true -> %% after game end
                                         Acc#acc.dls
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
     #acc{game_num = GNum,
          thid = G#mafia_game.thread_id, %thread
          last_page = G#mafia_game.page_to_read,
          last_msg_id = LMI,
          last_msg_time = LMT,
          dls = DLs
         }
    }.

gen_hist_and_get_dls(NextT, Acc) ->
    game:regen_history(NextT, Acc#acc.game_num),
    [G] = ?rgame(Acc#acc.game_num),
    lists:dropwhile(fun(#dl{time = DT}) -> DT =< NextT end,
                    ?lrev(G#mafia_game.deadlines)).

%% MsgId and #message{} are ok
check_vote_msgid_fun(_, _, _, IsPregame) when IsPregame -> ?undefined;
check_vote_msgid_fun(G, LMI, LMT, _IsPregame) ->
    DoCheck = do_check_fun(G),
    fun(Msg = #message{}) when Msg#message.time >= LMT,
                               ?e1(Msg#message.msg_key) /= LMI ->
            DoCheck(Msg);
       (MsgId) when is_integer(MsgId) ->
            Site = G#mafia_game.site,
            case ?rmess({MsgId, Site}) of
                [Msg] when Msg#message.time >= LMT,
                           ?e1(Msg#message.msg_key) /= LMI ->
                    DoCheck(Msg);
                _ -> ignore
            end;
       (_) -> ignore
    end.

do_check_fun(G) ->
    GNum = G#mafia_game.game_num,
    Cmds = case file:consult(mafia_file:cmd_filename(G)) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    REs = mafia_vote:get_regexs(),
    fun(Msg) ->
            MsgId = ?e1(Msg#message.msg_key),
            G2 = hd(?rgame(GNum)),
            Resp = mafia_vote:check_cmds_votes(G2, REs, Msg),
            [erlang:apply(M, F, A) || #cmd{msg_id = MId,
                                           mfa = {M, F, A}} <- Cmds,
                                      MId == MsgId],
            Resp
    end.

%% clear #mafia_day and #stat for this ThId
clear_mafia_day_and_stat(GNum) ->
    clear_mafia_day(GNum),
    clear_stat(GNum),
    ok.

clear_mafia_day(GNum) ->
    %% Remove mafia_days
    Res = [mnesia:dirty_delete(mafia_day, K)
           || K = {GNum2, _} <- mnesia:dirty_all_keys(mafia_day),
              GNum2 == GNum],
    ?dbg({clear_mafia_day, GNum, deleted, length(Res)}).

%% Remove stats for game.
clear_stat(GNum) ->
    StatKeyCheck = fun(Id, K) -> case K of
                                     {_, Id} -> true;
                                     {_, Id, _} -> true;
                                     _ -> false
                                 end
                   end,
    Res = [mnesia:dirty_delete(stat, K)
           || K <- mnesia:dirty_all_keys(stat), StatKeyCheck(GNum, K)],
    ?dbg({clear_stat, GNum, deleted, length(Res)}).

refresh_stat() ->
    mnesia:clear_table(stat),
    GNum = ?getv(?game_key),
    refresh_stat(?rgame(GNum)).

refresh_stat(GNum) when is_integer(GNum) ->
    refresh_stat(?rgame(GNum));
refresh_stat([]) -> ok;
refresh_stat([G = #mafia_game{thread_id = ThId,
                              site = Site}]) ->
    UpdateStatF = fun(MsgId) ->
                          update_stat(G, ?rmess(MsgId))
                  end,
    iterate_all_msg_ids({ThId, Site}, UpdateStatF),
    ok.

update_stat(_, []) -> ok;
update_stat(G, [M]) ->
    update_stat(G, M);
update_stat(G = #mafia_game{}, M = #message{}) ->
    GNum = G#mafia_game.game_num,
    #message{msg_key = MsgKey,
             user_name = UserB, %% :: user(),
             time = Time,       %% :: seconds1970(),
             message = MsgBin   %% :: message()
            } = M,
    MsgId = ?e1(MsgKey),
    Phase = case mafia_time:calculate_phase(G, Time) of
                #phase{ptype = ?game_start} -> ?game_start;
                #phase{ptype = ?game_ended} -> ?game_ended;
                #phase{num = Num, ptype = Ptype} -> {Num, Ptype}
            end,
    Key1 = {UserB, GNum},
    Key2 = {UserB, GNum, Phase},
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
    MsgIds = MsgIdsA ++ MsgIdsB,
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
    MsgIds = MsgIdsA ++ MsgIdsB,
    NewNumPosts = NPoA + NPoB,
    NewNumWords = NWoA + NWoB,
    #prstat{key = Key,
            msg_ids = MsgIds,
            num_chars = NChA + NChB,
            num_words = NewNumWords,
            num_postings = NewNumPosts,
            words_per_post = NewNumWords / NewNumPosts
           }.

iterate_all_game_msgs(GNum, DoSignup, MsgFun) ->
    case ?rgame(GNum) of
        [#mafia_game{thread_id = ThId,
                     site = Site,
                     signup_thid = SuThId}] ->
            Pks1 = if DoSignup, is_integer(SuThId) ->
                           mafia_lib:all_page_keys({SuThId, Site});
                      true -> []
                   end,
            Pks2 = if is_integer(ThId) ->
                           mafia_lib:all_page_keys({ThId, Site});
                      true -> []
                   end,
            PageKeys = Pks1 ++ Pks2,
            iterate_all_msgs(MsgFun, PageKeys, Site,
                             erlang:fun_info(MsgFun, arity));
        _ ->
            []
    end.

iterate_all_msgs(MsgFun, PageKeys, Site, {arity,1}) ->
    F = fun(PageKey) ->
                [PR] = ?rpage(PageKey),
                Msgs = [hd(?rmess({MsgId, Site}))
                        || MsgId <- PR#page_rec.message_ids],
                lists:foreach(MsgFun, Msgs),
                PageKey
        end,
    [F(PK) || PK <- PageKeys],
    ok;
iterate_all_msgs(MsgFun, PageKeys, Site, {arity,2}) ->
    MsgIds = lists:foldl(
               fun(PKey, Acc) ->
                       [#page_rec{message_ids = MsgIds}] =
                           ?rpage(PKey),
                       Acc ++ MsgIds
               end,
               [],
               PageKeys),
    lists:foldl(fun(MsgId, Acc2) ->
                        MsgFun(hd(?rmess({MsgId, Site})), Acc2)
                end,
                MsgFun(acc, init),
                MsgIds).

%% Iterate through all message ids in one thread in time order
iterate_all_msg_ids(ThId, Fun) ->
    iterate_all_msg_ids(ThId, Fun, all).

%% If the MsgIdFun returns an integer, the last returned integer
%% will also be returned from this fun as the MsgTime
-spec iterate_all_msg_ids({thread_id(), site()},
                          MsgIdFun :: function(),
                          PageFilter:: all | function())
                         -> last_iter_msg_ref().
iterate_all_msg_ids(Thread, MsgIdFun, Filter) ->
    iter_msgids(Thread, MsgIdFun, no_acc, Filter).

iter_msgids(Thread, MsgIdFun, Acc, PageFilter) ->
    All = mafia_lib:pages_for_thread(Thread),
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
    UrlBeg = mafia_lib:get_url_begin(S#s.site),
    Url = UrlBeg ++ ?i2l(S#s.thread_id) ++ ?UrlMid ++ ?i2l(S#s.page_to_read)
        ++ ?UrlEnd,
    S#s{url = Url}.

-spec get_body2(#s{}, term()) -> {ok, Body::term()} | {error, term()}.
get_body2(_S2, {error, _} = Error) -> Error;
get_body2(S2, {ok, Body}) ->
    Body2 = get_thread_section(S2#s.thread_id, Body),
    S3 = check_this_page(S2#s{body=Body2}),
    if not S3#s.is_last_page ->
            %% page complete > STORE IT on file!
            store_page(S3, Body2);
       true -> ok
    end,
    {ok, S3}.

get_thread_section(ThId, Body) ->
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
            ?dbg({download_wait_ms,
                  {S2#s.game_num, S2#s.site, S2#s.page_to_read},
                  B - A}),
            {ok, Body};
        {ok, {_StatusCode, Body}} ->
            ?inc_cnt(http_responses),
            B = erlang:monotonic_time(millisecond),
            ?dbg({download_wait_ms_2,
                  {S2#s.game_num, S2#s.site, S2#s.page_to_read},
                  B - A}),
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
    mafia_file:th_filenames(S#s.game_rec, S#s.thread_id, S#s.page_to_read).

th_filenames_store(S) ->
    mafia_file:th_filenames(S#s.game_rec, S#s.thread_id, S#s.page_last_read).

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

analyse_body(S = #s{body = ""}) ->
    S;
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
             ?changed ->
                 MsgR = write_message_rec(S, MsgId, User, Time, Msg),
                 mafia:add_user(User, S#s.site),
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
    element(2, rm_to_after_match(Str, Search)).

%% @doc return string after match
rm_to_after_match(Str, Search) ->
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

-spec update_page_rec(#s{}, msg_id()) -> ?changed | ?unchanged.
update_page_rec(S, MsgIdInt) ->
    {Action, PageRec} =
        case ?rpage(S#s.thread_id, S#s.page_last_read, S#s.site) of
            [] ->
                {?changed,
                 #page_rec{key = {S#s.thread_id, S#s.page_last_read, S#s.site},
                           site = S#s.site,
                           thread_id = S#s.thread_id,
                           message_ids = [MsgIdInt],
                           complete = false}};
            [P = #page_rec{message_ids = MsgIds, complete = Comp}] ->
                MsgIds2 =
                    case lists:member(MsgIdInt, MsgIds) of
                        false -> MsgIds ++ [MsgIdInt];
                        true -> MsgIds
                    end,
                Comp2 = Comp orelse not S#s.is_last_page,
                P2 = P#page_rec{message_ids = MsgIds2,
                                complete = Comp2},
                Act = case P2 of
                          P -> ?unchanged;
                          _ -> ?changed
                      end,
                {Act, P2}
        end,
    if Action == ?changed ->
            ?dwrite_page(PageRec);
       true -> ok
    end,
    remove_duplicate_msgid(S, Action, MsgIdInt),
    Action.

%% @doc Possibly remove MsgId from page 1 when adding MsgId to page 2.
%% (webdiplomacy.net can have upto 48 messages on page 1 before
%% splitting them into the 2 first pages)
remove_duplicate_msgid(S = #s{page_last_read = 2}, Action, MsgId)
  when Action == ?changed ->
    case ?rpage(S#s.thread_id, 1, S#s.site) of
        [P = #page_rec{message_ids = MsgIds}] ->
            case lists:member(MsgId, MsgIds) of
                true ->
                    ?dbg({rm_duplicate_on_page1, MsgId}),
                    ?dwrite_page(P#page_rec{message_ids = MsgIds -- [MsgId]});
                false -> ok
            end;
        _ ->
            ok
    end;
remove_duplicate_msgid(_, _, _) ->
    ok.

write_message_rec(S, MsgIdInt, User, Time, MsgText) ->
    MsgKey = {MsgIdInt, S#s.site},
    Msg = #message{msg_key = MsgKey,
                   thread_id = S#s.thread_id,
                   page_num = S#s.page_last_read,
                   user_name = User,
                   time = Time,
                   message = ?l2b(MsgText)
                  },
    ?dwrite_msg(Msg),
    Msg.

strip(Str) ->
    t_strip(h_strip(Str)).

h_strip([H|T]) when H =< $\s -> strip(T);
h_strip(Str) -> Str.

t_strip(Str) ->
    ?lrev(h_strip(?lrev(Str))).
