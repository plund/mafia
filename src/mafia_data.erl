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
         find_pos_and_split/2,
         get_after_pos/3,
         sum_stat/2
        ]).

%% utilities
-export([update_stat/1,
         compress_txt_files/0,
         grep/1, grep/2,
         iterate_all_msgs/2,
         iterate_all_msg_ids/3,
         manual_cmd_to_file/2,
         manual_cmd_from_file/2
        ]).

-include("mafia.hrl").

-type last_iter_msg_ref() :: none |
                             {ThId :: integer(),
                              Page :: integer(),
                              MsgId :: integer()}.

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
         dl_time :: ?undefined | millisecs(),
         do_refresh_msgs = false :: boolean()
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
download(S) ->
    case get_body(S#s{dl_time = ?undefined}) of
        {ok, S2} ->
            analyse_body(S2),
            if not S2#s.is_last_page ->
                    if not S2#s.body_on_file ->
                            sleep(10000);
                       true -> ok
                    end,
                    download(S2);
               true -> {ok, S2}
            end;
        {error, _Error} = Err ->
            {Err, S}
    end.

%% Download a game thread
-spec downl_web(integer()) -> ok.
downl_web(GameKey) when is_integer(GameKey) ->
    downl_web(?rgame(GameKey));
downl_web([]) -> ok;
downl_web([G]) ->
    GameKey = G#mafia_game.key,
    InitPage = G#mafia_game.page_to_read,
    Page = check_db(GameKey, InitPage),
    case download(#s{thread_id = GameKey,
                     page_to_read = Page}) of
        {ok, S2} ->
            %% ?dbg({downl_web_a, InitPage, Page, S2#s.page_to_read}),
            update_page_to_read(GameKey, S2#s.page_to_read);
        {{error, _R}, S2} ->
            %% ?dbg({downl_web_b, InitPage, Page, S2#s.page_to_read}),
            update_page_to_read(GameKey, S2#s.page_to_read)
    end,
    ok.

check_db(Key, P) ->
    MsgIdF = check_vote_msgid_fun(Key),
    ReadF = fun(P2) -> ?rpage(Key, P2) end,
    check_db(MsgIdF, ReadF, P, P, ReadF(P)).

check_db(_MsgIdF, _ReadF, Start, Start, []) -> Start;
check_db(_MsgIdF, _ReadF, P, _Start, []) -> P - 1;
check_db(MsgIdF, ReadF, P, Start, [PageRec]) ->
    [MsgIdF(MsgId) || MsgId <- PageRec#page_rec.message_ids],
    NextP = P + 1,
    check_db(MsgIdF, ReadF, NextP, Start, ReadF(NextP)).

update_page_to_read(GameKey, PageToRead)
  when is_integer(GameKey), is_integer(PageToRead) ->
    G = hd(?rgame(GameKey)),
    if PageToRead /= G#mafia_game.page_to_read ->
            mnesia:dirty_write(G#mafia_game{page_to_read = PageToRead});
       true -> ok
    end.

sleep(MilliSecs) ->
    receive after MilliSecs -> ok end.

refresh_messages() -> refresh_messages(?game_key).

refresh_messages(?game_key = K) -> refresh_messages(?getv(K));
refresh_messages(?thread_id = K) -> refresh_messages(?getv(K));
refresh_messages(ThId) -> refresh_messages(ThId, true).

-spec refresh_messages(ThId :: integer(), DoVotes :: boolean()) -> ok.
refresh_messages(ThId, DoVotes) ->
    ?set(?page_to_read, 1),
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
    refresh_votes(ThId, ?rgame(ThId), all, soft).

refresh_votes(?game_key = K) -> refresh_votes(?getv(K));
refresh_votes(?thread_id = K) -> refresh_votes(?getv(K));
refresh_votes(ThId) when is_integer(ThId) ->
    clear_mafia_day_and_stat(ThId),
    refresh_votes(ThId, ?rgame(ThId), all, soft);
refresh_votes(hard) ->
    ThId = ?getv(?game_key),
    clear_mafia_day_and_stat(ThId),
    refresh_votes(ThId, ?rgame(ThId), all, hard);
refresh_votes({upto, EndPage}) when is_integer(EndPage) ->
    ThId = ?getv(?game_key),
    case curr_page_to_read(ThId) of
        no_game -> ok;
        PageNumG when is_integer(PageNumG) ->
            PageFilter =
                if PageNumG > EndPage ->
                        clear_mafia_day_and_stat(ThId),
                        fun(Page) -> Page =< EndPage end;
                   true ->
                        fun(Page) ->
                                PageNumG =< Page andalso
                                    Page =< EndPage
                        end
                end,
            refresh_votes(ThId, ?rgame(ThId), PageFilter, soft)
    end.

refresh_votes(_ThId, [], _F, _Method) ->
    ok;
refresh_votes(ThId, [G], PageFilter, Method) ->
    if Method == soft ->
            G2 = G#mafia_game{
                   players_rem = G#mafia_game.players_orig,
                   player_deaths = [D#death{is_deleted = true}
                                    || D <- G#mafia_game.player_deaths]
                  },
            mnesia:dirty_write(G2);
       Method == hard ->
            %% Reinitialize the game table
            mafia_db:write_default_table(game, ThId)
    end,
    MsgIdFun = check_vote_msgid_fun(ThId),
    case iterate_all_msg_ids(ThId, MsgIdFun, PageFilter) of
        none ->
            ?dbg({last_iter_msg_ref, none}),
            ok;
        {ThId, PageNum, MsgId} ->
            ?dbg({last_iter_msg_ref, ThId, PageNum, MsgId}),
            update_page_to_read(ThId, PageNum)
    end,
    ok.

check_vote_msgid_fun(ThId) ->
    Cmds = case file:consult(cmd_filename(ThId)) of
               {ok, CmdsOnFile} -> [C || C = #cmd{} <- CmdsOnFile];
               _ -> []
           end,
    fun(MsgId) ->
            mafia_vote:check_for_vote(MsgId),
            [erlang:apply(M, F, A) || #cmd{msg_id = MId,
                                           mfa = {M, F, A}} <- Cmds,
                                      MId == MsgId]
    end.

curr_page_to_read(ThId) when is_integer(ThId) ->
    case ?rgame(ThId) of
        [] -> no_game;
        [G] -> G#mafia_game.page_to_read
    end.

%% clear #mafia_day and #stat for this ThId
clear_mafia_day_and_stat(ThId) ->
    ?dbg({clear_mafia_day_and_stat, ThId}),
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
                Msgs = [hd(mnesia:dirty_read(message, MsgId))
                        || MsgId <- PR#page_rec.message_ids],
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
                        MsgFun(hd(mnesia:dirty_read(message, MsgId)), Acc2)
                end,
                MsgFun(acc, init),
                MsgIds).

%% Iterate through all message ids in one thread in time order
iterate_all_msg_ids(ThId, Fun) ->
    iterate_all_msg_ids(ThId, Fun, all).

-spec iterate_all_msg_ids(ThId :: integer(),
                          MsgIdFun :: function(),
                          PageFilter:: all | function())
                         -> last_iter_msg_ref().
iterate_all_msg_ids(ThId, Fun, Filter) ->
    All = mafia:pages_for_thread(ThId),
    Pages = if Filter == all -> All;
               is_function(Filter) ->
                    lists:filter(Filter, All)
            end,
    %% get last page num
    iterate_all_msg_idsI(ThId, Fun, Pages).

%% Return reference to last iterated message
-spec iterate_all_msg_idsI(ThId :: integer(),
                           Fun :: function(),
                           Pages :: [integer()])
                          -> last_iter_msg_ref().
iterate_all_msg_idsI(ThId, Fun, Pages) ->
    F = fun(Page) ->
                [PR] = ?rpage(ThId, Page),
                MsgIds = PR#page_rec.message_ids,
                lists:foreach(Fun, MsgIds),
                %% get last msg id
                case MsgIds of
                    [] -> none;
                    _ -> {ThId, Page, lists:max(MsgIds)}
                end
        end,
    PageRes = [F(P) || P <- Pages],
    case [PR || PR <- PageRes, PR /= none] of
        [] -> none;
        PRs2 -> lists:last(PRs2)
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
    %% io:format("gb ~p\n", [S2#s.page_to_read]),
    get_body2(S2, http_request(S2)).

make_url(S) ->
    Url = ?UrlBeg ++ ?i2l(S#s.thread_id) ++ ?UrlMid ++ ?i2l(S#s.page_to_read) ++ ?UrlEnd,
    S#s{url = Url}.

-spec get_body2(#s{}, term()) -> {ok, Body::term()} | {error, term()}.
get_body2(_S2, {error, _} = Error) -> Error;
get_body2(S2, {ok, Body}) ->
    %% io:format("gb2 ~p\n", [S2#s.page_to_read]),
    Body2 = get_thread_section(Body),
    S3 = check_this_page(S2#s{body=Body2}),
    if not S3#s.is_last_page -> % page complete > STORE IT!
            store_page(S3, Body2);
       true -> ok
    end,
    %% io:format("gb2 ~p\n", [S3#s.page_to_read]),
    {ok, S3}.

get_thread_section(Body) ->
    ThId = ?getv(?thread_id),
    ThStartStr = "<div class=\"thread threadID" ++ ?i2l(ThId),
    B2 = rm_to_after(Body, ThStartStr),
    ThEndStr = "<div class=\"thread thread",
    {_, ThreadStr} = read_to_before(B2, ThEndStr),
    ThreadStr.

http_request(S2) ->
    A = erlang:monotonic_time(millisecond),
    case httpc:request(S2#s.url) of
        {ok, {_StatusLine, _Headers, Body}} ->
            B = erlang:monotonic_time(millisecond),
            io:format("Download wait ~w millisecs\n", [B - A]),
            {ok, Body};
        {ok, {_StatusCode, Body}} ->
            B = erlang:monotonic_time(millisecond),
            io:format("Download wait ~w millisecs\n", [B - A]),
            {ok, Body};
        {ok, _ReqId} ->
            {error, no_body};
        {error, _Reason} ->
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

th_filenames_read(S) ->
    th_filenames(th_filename(S#s.thread_id, S#s.page_to_read)).

th_filenames_store(S) ->
    th_filenames(th_filename(S#s.thread_id, S#s.page_last_read)).

th_filenames(FileName) ->
    TarBallName = FileName ++ ".tgz",
    {FileName, TarBallName}.

th_filename(Thread, Page) ->
    DirName = "thread_pages",
    verify_exist(DirName),
    GamePrefix = mafia:game_file_prefix(Thread),
    DirName2 = GamePrefix ++ ?i2l(Thread),
    verify_exist(filename:join(DirName, DirName2)),
    BaseName = ?i2l(Page) ++ ".txt",
    filename:join([DirName, DirName2, BaseName]).

cmd_filename(#s{thread_id = Thread}) ->
    cmd_filename(Thread);
cmd_filename(Thread) ->
    DirName =  "command_files",
    verify_exist(DirName),
    GamePrefix = mafia:game_file_prefix(Thread),
    BaseName = GamePrefix ++ ?i2l(Thread) ++ "_manual_cmds.txt",
    filename:join([DirName, BaseName]).

verify_exist(DirName) ->
    case file:read_file_info(DirName) of
        {error, enoent} ->
            file:make_dir(DirName);
        _ -> ok
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

manual_cmd_to_file(ThId, Cmd) ->
    FN = cmd_filename(ThId),
    DoAppend =
        case file:consult(FN) of
            {error, enoent} -> true;
            {ok, CmdsOnFile} ->
                not lists:member(Cmd, CmdsOnFile)
        end,
    if DoAppend ->
            {ok, Fd} = file:open(FN, [append]),
            io:format(Fd, "~999p.\n", [Cmd]),
            file:close(Fd);
       not DoAppend -> ok
    end.

manual_cmd_from_file(ThId, Cmd) ->
    FN = cmd_filename(ThId),
    case file:consult(FN) of
        {error, enoent} -> true;
        {ok, CmdsOnFile} ->
            NewCmds = CmdsOnFile -- [Cmd],
            if NewCmds /= CmdsOnFile ->
                    NewCmdsSorted = lists:keysort(#cmd.msg_id, NewCmds),
                    {ok, Fd} = file:open(FN, [write]),
                    [io:format(Fd, "~999p.\n", [C]) || C <- NewCmdsSorted],
                    file:close(Fd);
               true -> ok
            end
    end.

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

analyse_body(#s{body = ""}) -> ok;
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

    if UserStr /= "" ->
            MsgId = ?l2i(MsgIdStr),
            Msgs = ?rmess(MsgId),
            if Msgs == [] orelse S#s.do_refresh_msgs ->
                    Time = ?l2i(TimeStr),
                    User = ?l2b(UserStr),
                    update_page_rec(S, MsgId),
                    MsgR = write_message_rec(S, MsgId, User, Time, Msg),
                    mafia_vote:verify_user(MsgR),
                    mafia_vote:check_for_vote(S, MsgR),
                    mafia_print:print_message_summary(MsgR);
               true ->
                    ok
            end;
       true -> ok
    end,
    analyse_body(S#s{body = B6}).

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

find_pos_and_split(Str, Search) ->
    case string:str(Str, Search) of
        0 -> {0, "", ""};
        P -> {P,
              string:left(Str, P - 1),
              get_after_pos(P, length(Search), Str)}
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

%% Data base stuff?
update_page_rec(S, MsgIdInt) ->
    PageRec =
        case ?rpage(S#s.thread_id, S#s.page_last_read) of
            [] -> #page_rec{key = {S#s.thread_id, S#s.page_last_read},
                            message_ids = [MsgIdInt],
                            thread_id = S#s.thread_id,
                            complete = false};
            [P = #page_rec{message_ids = MsgIds, complete = Comp}] ->
                MsgIds2 = MsgIds ++
                    case lists:member(MsgIdInt, MsgIds) of
                        false -> [MsgIdInt];
                        true -> []
                    end,
                Comp2 = Comp orelse not S#s.is_last_page,
                P#page_rec{message_ids = MsgIds2,
                           complete = Comp2}
        end,
    mnesia:dirty_write(PageRec).

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
