-module(mafia_data).

%% interface
-export([downl/0]).
%% library
-export([
         rm_to_after_pos/2,
         find_pos_and_split/2,
         get_after_pos/3,
         sum_stat/2,
         game_file_prefix/1
        ]).

%% utilities
-export([refresh_messages/0,
         refresh_votes/0,
         refresh_votes/1,
         refresh_stat/0,
         compress_txt_files/0,
         grep/1, grep/2,
         iterate_all_msgs/2,
         iterate_all_msg_ids/3
        ]).

-import(mafia,
        [
         getv/1,
         set/2,

         b2l/1,
         b2ub/1,
         l2b/1,
         l2u/1,
         i2l/1,
         l2i/1,
         lrev/1,
         rgame/1
        ]).

-include("mafia.hrl").

downl() ->
    mafia:setup_mnesia(),
    inets:start(),
    Thread = getv(thread_id),
    Page = getv(page_to_read),
    download(#s{thread_id = Thread,
                page = Page}).

download(S) ->
    case get_body(S#s{dl_time = undefined}) of
        {ok, S2} ->
            analyse_body(S2),
            if not S2#s.is_last_page ->
                    if not S2#s.body_on_file ->
                            sleep(10000);
                       true -> ok
                    end,
                    Page = getv(page_to_read),
                    download(S2#s{page = Page});
               true -> ok
            end;
        {error, _Error} -> error
    end.

sleep(MilliSecs) ->
    receive after MilliSecs -> ok end.

refresh_messages() ->
    mafia:set(page_to_read, 1),
    mnesia:clear_table(message),
    mnesia:clear_table(page_rec),
    mnesia:clear_table(stat),
    mafia:downl(),
    refresh_votes().

refresh_votes() ->
    mnesia:clear_table(mafia_day),
    ThId = getv(thread_id),
    refresh_votes(ThId, rgame(ThId), all, soft).

refresh_votes(EndPage) ->
    mnesia:clear_table(mafia_day),
    ThId = getv(thread_id),
    Filter = fun(Page) -> Page =< EndPage end,
    refresh_votes(ThId, rgame(ThId), Filter, soft).

refresh_votes(_ThId, [], _F, _Method) -> ok;
refresh_votes(ThId, [G], Filter, Method) ->
    G2 = if Method == soft ->
                 G#mafia_game{
                   players_rem = G#mafia_game.players_orig,
                   player_deaths = [D#death{is_deleted = true}
                                    || D <- G#mafia_game.player_deaths]
                  };
            Method == hard ->
                 G#mafia_game{players_rem = G#mafia_game.players_orig,
                              player_deaths = []}
         end,
    mnesia:dirty_write(G2),
    iterate_all_msg_ids(ThId, fun mafia_vote:check_for_vote/1, Filter),
    Pages = lists:sort(mafia:find_pages_for_thread(ThId)),
    "Thread " ++ i2l(ThId) ++ "; Pages: " ++
        string:join([i2l(P) || P <- Pages],",").

refresh_stat() ->
    mnesia:clear_table(stat),
    ThId = getv(thread_id),
    refresh_stat(ThId, rgame(ThId)).

refresh_stat(_ThId, []) -> ok;
refresh_stat(ThId, [_G]) ->
    iterate_all_msg_ids(ThId, fun update_stat/1),
    ok.

update_stat(MsgId) ->
    update_stat(MsgId, mnesia:dirty_read(message, MsgId)).

update_stat(_MsgId, []) -> ok;
update_stat(MsgId, [M = #message{thread_id = ThId}]) ->
    update_stat(MsgId, M, rgame(ThId)).

update_stat(_MsgId, _M, []) -> ok;
update_stat(MsgId, M, [G = #mafia_game{}]) ->
    #message{thread_id = ThId,  %% :: thread_id(),
             user_name = UserB, %% :: user(),
             time = Time,       %% :: seconds1970(),
             message = MsgBin   %% :: message()
            } = M,
    %% UserB is in correct case in messages!
    UserUB = b2ub(UserB),
    Phase = mafia_time:calculate_phase(G, Time),
    Key1 = {UserUB, ThId},
    Key2 = {UserUB, ThId, Phase},
    Msg = b2l(MsgBin),
    Count = #stat{num_chars = size(MsgBin),
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
    Key = if KA == undefined -> KB;
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
    ThId = getv(thread_id),
    StrU = l2u(Str),
    GrepF =
        fun(M) ->
                Msg = b2l(M#message.message),
                MsgU = l2u(Msg),
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
    Pages = lists:sort(mafia:find_pages_for_thread(ThId)),
    F = fun(Page) ->
                Key = {ThId, Page},
                [PR] = mnesia:dirty_read(page_rec, Key),
                Msgs = [hd(mnesia:dirty_read(message, MsgId))
                        || MsgId <- PR#page_rec.message_ids],
                lists:foreach(MsgFun, Msgs),
                Key
        end,
    [F(P) || P <- Pages],
    ok;
iterate_all_msgs(ThId, MsgFun, {arity,2}) ->
    Pages = lists:sort(mafia:find_pages_for_thread(ThId)),
    io:format("NumPs ~p\n", [length(Pages)]),
    MsgIds = lists:foldl(
               fun(P, Acc) ->
                       [#page_rec{message_ids = MsgIds}] =
                           mnesia:dirty_read(page_rec, {ThId, P}),
                       Acc ++ MsgIds
               end,
               [],
               Pages),
    io:format("NumIds ~p\n", [length(MsgIds)]),
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
                         -> term().
iterate_all_msg_ids(ThId, Fun, Filter) ->
    All =  lists:sort(mafia:find_pages_for_thread(ThId)),
    Pages = if Filter == all -> All;
               is_function(Filter) ->
                    lists:filter(Filter, All)
            end,
    iterate_all_msg_idsI(ThId, Fun, Pages).

iterate_all_msg_idsI(ThId, Fun, Pages) ->
    F = fun(Page) ->
                Key = {ThId, Page},
                [PR] = mnesia:dirty_read(page_rec, Key),
                MsgIds = PR#page_rec.message_ids,
                lists:foreach(Fun, MsgIds),
                Key
        end,
    [F(P) || P <- Pages].

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
    Url = ?UrlBeg ++ i2l(S#s.thread_id) ++ ?UrlMid ++ i2l(S#s.page) ++ ?UrlEnd,
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
    ThId = getv(thread_id),
    ThStartStr = "<div class=\"thread threadID" ++ i2l(ThId),
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
    {_FileName, TarBallName} = th_filenames(S),
    case erl_tar:extract(TarBallName, [memory, compressed]) of
        {ok, [{_, BodyBin}]} ->
            %% io:format("Found page ~p on file\n",[S#s.page]),
            {file, b2l(BodyBin)};
        {error, {TarBallName, enoent}} ->
            %% io:format("Did NOT find ~p on file\n",[S#s.page]),
            no_file;
        Unexp ->
            io:format("Did NOT find ~p on file ~p\n",[S#s.page, Unexp]),
            no_file
    end.

th_filenames(S) ->
    FileName = th_filename(S),
    TarBallName = FileName ++ ".tgz",
    {FileName, TarBallName}.

th_filename(#s{thread_id = Thread, page = Page}) ->
    DirName = "thread_pages",
    verify_exist(DirName),
    GamePrefix = case rgame(Thread) of
                     [] -> "";
                     [G] -> game_file_prefix(G)
                 end,
    DirName2 = GamePrefix ++ i2l(Thread),
    verify_exist(filename:join(DirName, DirName2)),
    BaseName = i2l(Page) ++ ".txt",
    filename:join([DirName, DirName2, BaseName]).

game_file_prefix(G) ->
    "m" ++ i2l(G#mafia_game.game_num) ++ "_".

verify_exist(DirName) ->
    case file:read_file_info(DirName) of
        {error, enoent} ->
            file:make_dir(DirName);
        _ -> ok
    end.

%% -> ok | {error, Reason}
store_page(S, Body) ->
    {FileName, TarBallName} = th_filenames(S),
    case file:read_file_info(TarBallName) of
        {error, enoent} ->
            file:write_file(FileName, Body),
            erl_tar:create(TarBallName, [FileName], [compressed, verbose]),
            file:delete(FileName);
        _ ->
            {error, efileexist}
    end.

%% -----------------------------------------------------------------------------

%% compressed 1656 K data in less than 0.09 sec
compress_txt_files() ->
    Dir = "thread_pages",
    {ok, Files} = file:list_dir(Dir),
    Files2 = [ filename:join(Dir, F)
               || F = "m24_" ++ _ <- Files,
                  case lrev(F) of
                        "txt." ++ _ -> true;
                        _ -> false
                    end],
    [erl_tar:create(F++".tgz", [F], [compressed, verbose]) || F <- Files2],
    [file:delete(F)|| F <- Files2].

%% -----------------------------------------------------------------------------

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
            set(page_to_read, PageLastRead),
            ok;
        false ->
            set(page_to_read, PageLastRead + 1)
            %% set(page_complete, PageLastRead)
    end,
    S#s{is_last_page = IsLastPage,
        page_num_last_read = PageLastRead,
        page = PageLastRead,
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
    Msg = strip_fix(MsgRaw),

    if UserStr /= "" ->
            MsgId = l2i(MsgIdStr),
            Time = l2i(TimeStr),
            User = l2b(UserStr),
            case mnesia:dirty_read(message, MsgId) of
                [] ->
                    update_page_rec(S, MsgId),
                    MsgR = write_message_rec(S, MsgId, User, Time, Msg),
                    update_stat(MsgId),
                    mafia_vote:check_for_vote(S, MsgR),
                    mafia_print:print_message_summary(MsgR);
                [_MsgR] ->
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
        case mnesia:dirty_read(page_rec, {S#s.thread_id, S#s.page}) of
            [] -> #page_rec{key = {S#s.thread_id, S#s.page},
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
                   page_num = S#s.page,
                   user_name = User,
                   time = Time,
                   message = l2b(Msg)
                  }
     ),
    M.

strip(Str) ->
    t_strip(h_strip(Str)).

h_strip([H|T]) when H =< $\s -> strip(T);
h_strip(Str) -> Str.

t_strip(Str) ->
    lrev(h_strip(lrev(Str))).

strip_fix(Str) ->
    strip(Str).
    %% S2 = strip(Str),
    %% fix(S2).

%% skip unicode for a while
fix("&gt;" ++ T) -> [ $> | fix(T)];
fix("&lt;" ++ T) -> [ $< | fix(T)];
fix("&amp;" ++ T) -> [ $& | fix(T)];
fix("&acute;" ++ T) -> [ $´ | fix(T)];
fix("&lsquo;" ++ T) -> [ $' | fix(T)];
fix("&rsquo;" ++ T) -> [ $' | fix(T)];
fix("&ldquo;" ++ T) -> [ $\" | fix(T)];
fix("&rdquo;" ++ T) -> [ $\" | fix(T)];
fix("&hellip;" ++ T) -> [ $\., $\., $\. | fix(T)];
%% fix("&lsquo;" ++ T) -> [ $‘ | fix(T)];
%% fix("&rsquo;" ++ T) -> [ $’ | fix(T)];
%% fix("&ldquo;" ++ T) -> [ $“ | fix(T)];
%% fix("&rdquo;" ++ T) -> [ $” | fix(T)];
fix("<br />" ++ T) ->  [ $\n | fix(T)];
fix([H|T]) -> [H|fix(T)];
fix("") -> "".
