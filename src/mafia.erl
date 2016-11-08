-module(mafia).

-compile(export_all).

-export([t/0, t/1,
         pr/0, pr/1,
         remove_mnesia/0 ]).

-define(ThId, 1404320).
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").
%-define(URL, "http://webdiplomacy.net/forum.php?newsendtothread=1404320").
-define(Url2, "http://webdiplomacy.net/forum.php?threadID=1404320&page-thread=1#threadPager").
%-define(FulLURL, "http://webdiplomacy.net/forum.php?newsendtothread=1404320&viewthread=1404320&reply=success#postbox").


%% FIX THIS!! Find LAST PAGE READING THIS struct in the beginning of the page  <em>Page <strong>174</strong> of <strong>174</strong></em></div></div><div style="

-record(kv_store,
        {key,
         value
        }).

-type thread_id() :: integer(). 
-type page_num() :: integer(). 
-type msg_id() :: integer(). 

-record(page_rec,
        {key :: {thread_id(), page_num()},
         message_ids :: [msg_id()]
        }).

-record(message,
        {msg_id :: msg_id(),
         thread_id :: thread_id(),
         page_num :: page_num(),
         user_name,
         time,
         message
        }).

-record(s , {page, thread_id, url, body}).

t() ->
    setup_mnesia(),
    inets:start(),
    Page = get_kv(page_to_read),
    t(#s{page=Page}).

t(S) ->
    case get_body(S) of
        {ok, S2} -> print_usr_msgs(S2);
        error -> error
    end.

get_body(S) ->
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

-spec was_this_last_page(S :: #s{}) -> boolean().
was_this_last_page(S) ->
    case get_body(S#s{page = S#s.page + 1}) of
        error -> true;
        {ok, _S2} -> false
    end.

make_url(S) ->
    Url = ?UrlBeg ++ integer_to_list(?ThId) ++ ?UrlMid ++ integer_to_list(S#s.page) ++ ?UrlEnd,
    S#s{url = Url, thread_id = ?ThId}.

print_usr_msgs(S) ->
    %page_to_read,
    B2 = get_thread_section(S#s.body),
    print_usr_msgs2(S, B2).

print_usr_msgs2(S, "") ->
    case was_this_last_page(S) of
        true -> ok;
        false ->
            Page = S#s.page,
            set_kv(page_to_read, Page + 1),
            set_kv(page_complete, Page)
    end,
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
            MsgR =
                case mnesia:dirty_read(message, MsgIdInt) of
                    [] ->
                        io:format("New ~w - ~w - ~w~n", [S#s.thread_id, S#s.page, MsgIdInt]),
                        PageRec =
                            case mnesia:dirty_read(page_rec, {S#s.thread_id, S#s.page}) of
                                [] -> #page_rec{key = {S#s.thread_id, S#s.page},
                                                message_ids = [MsgIdInt]};
                                [P = #page_rec{message_ids = MsgIds}] ->
                                    P#page_rec{message_ids = MsgIds ++ [MsgIdInt]}
                            end,
                        mnesia:dirty_write(PageRec),
                        mnesia:dirty_write(
                          M = #message{msg_id = MsgIdInt,
                                       thread_id = S#s.thread_id,
                                       page_num = S#s.page,
                                       user_name = list_to_binary(UserStr),
                                       time = list_to_integer(TimeStr),
                                       message = list_to_binary(Msg)
                                      }
                         ),
                        M;
                    [M] -> M
                end,
            print_usr_msg3(MsgR);
       true -> ok
    end,
    print_usr_msgs2(S, B6).

pr() ->
    Page = get_kv(page_to_read),
    pr(Page).

pr(Page) ->
    ThId = ?ThId,
    MsgIds = case mnesia:dirty_read(page_rec, {ThId, Page}) of
                 [] -> [];
                 [#page_rec{message_ids = MIds}] -> MIds
             end,
    DoPrint =
        fun(MsgId) ->
                [Msg] = mnesia:dirty_read(message, MsgId),
                print_usr_msg3(Msg)
        end,
    [DoPrint(MsgId) || MsgId <- MsgIds].

print_usr_msg3(M) ->
    io:format("User : ~s\n"
              "MsgId: ~s\n"
              "Time : ~s\n"
              "Wrote: \"~s\"\n"
              "\n",
              [binary_to_list(M#message.user_name),
               integer_to_list(M#message.msg_id),
               fix_time(M#message.time),
               binary_to_list(M#message.message)]).


get_thread_section(Body) ->
    B2 = rm_to_after(Body, "<div class=\"thread threadID1404320"),
    {_, ThreadStr} = read_to_before(B2, "<div class=\"thread thread"),
    ThreadStr.

fix_time(Time) when is_integer(Time) ->
    TzH = get_kv(timezone),
    Dst = true,
    fix_time(Time, TzH, Dst).

fix_time(Time, TzH, Dst) when is_integer(Time) ->
    %% {17104,{0,3,2}}
    try 
        Time2 = Time + (TzH + if Dst -> 1; true -> 0 end) * 3600,
        {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(Time2),
        {Y, M, D} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({1970,1,1}) + Days1970),
        case {TzH, Dst} of
            {0, false} ->
                io_lib:format("~s-~s-~sZ~s:~s:~s", [p(Y), p(M), p(D), p(HH), p(MM), p(SS)]);
            _ ->
                DstStr = case Dst of false -> "N"; true -> "DST" end,
                io_lib:format("~s-~s-~sT~s:~s:~s (~s) ~s",
                              [p(Y), p(M), p(D), p(HH), p(MM), p(SS), i2l(TzH), DstStr])
        end
    catch _:_ -> ""
    end.

i2l(I) -> integer_to_list(I).

p(I) when I > 9 -> integer_to_list(I);
p(I) -> string:right(integer_to_list(I), 2, $0).

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

strip(Str) ->
    t_strip(h_strip(Str)).

h_strip([H|T]) when H =< $\s -> strip(T);
h_strip(Str) -> Str.

t_strip(Str) ->
    lists:reverse(h_strip(lists:reverse(Str))).

-spec setup_mnesia() -> ok | schema_existed_already | {error, Reason::term()}.
setup_mnesia() ->
    case mnesia:create_schema([node()]) of
        {error,{_,{already_exists,_}}} ->
            start_mnesia(already_exists),
            schema_existed_already;
        Other ->
            start_mnesia(do_create),
            Other
    end.

remove_mnesia() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

-spec start_mnesia(Op :: already_exists | do_create )
                  -> mnesia_start_ok | {error, Reason::term()}.
start_mnesia(Op) ->
    case mnesia:start() of
        ok ->
            if Op == do_create ->
                    create_tables(),
                    insert_initial_data();
               true ->
                    ok
            end,
            mnesia_start_ok;
        Other ->
            Other
    end.

insert_initial_data() ->
    set_kv(timezone, -5),
    set_kv(d1_deadline_local, {{2016,10,19},{18,0,0}}),
    set_kv(day_hours, 48),
    set_kv(night_hours, 24),
    set_kv(dst_normal, {{2016,11,06},{2,0,0}}),
    set_kv(thread_str, ?ThId),
    set_kv(page_to_read, 172),
    set_kv(page_complete, 0).

set_kv(Key, Value) ->
    mnesia:dirty_write(#kv_store{key=Key, value = Value}).

get_kv(Key) -> get_kv(Key, undefined).

get_kv(Key, Default) ->
    case mnesia:dirty_read(kv_store, Key) of
        [] -> Default;
        [#kv_store{value = Value}] -> Value
    end.

create_tables() ->
    create_table(kv_store),
    create_table(page_rec),
    create_table(message).

create_table(RecName) ->
    Opts = create_table_opts(RecName),
    io:format("mnesia:create_table(~p, ~p).", [RecName, Opts]),
    case mnesia:create_table(RecName, Opts) of
        {aborted,{already_exists,_Tab}} ->
            TI = mnesia:table_info(RecName, attributes),
            RI = rec_info(RecName),
            if TI /= RI ->
%%% HERE we should check for upgrade method and stop if upgrade does not exist
                    io:format("Delete table '~p' due to mismatching attribute list\n",[RecName]),
                    mnesia:delete_table(RecName),
                    create_table(RecName);
               true ->
                    io:format("Table '~p' is OK!\n",[RecName])
            end;
        {atomic, ok} ->
            io:format("Init create of table '~p'\n",[RecName])
    end.

create_table_opts(Table) ->
    [{disc_copies, [node()]},
     {attributes, rec_info(Table)}].

rec_info(kv_store) -> record_info(fields, kv_store);
rec_info(page_rec) -> record_info(fields, page_rec);
rec_info(message) -> record_info(fields, message).


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

-ifdef(EXCLUDE).
-define(XXX,
          <div style="clear:both"></div>
                                </div>
<div class="reply replyborder1 replyalternate1
                                 userID28372">
<a name="1410330"></a>
<div class="message-head replyalternate1 leftRule">
<strong>
  <a href="profile.php?userID=28372">goldfinger0303
  <img style="display:none;" class="userOnlineImg" userID="28372" src="images/icons/online.png" alt="Online" title="User currently logged on" /> 
  (817  <img src="images/icons/points.png" alt="D" title="webDiplomacy points" />)
  </a>
</strong>
  <br /><a style="display:none;" class="messageIconForum" threadID="1404320" messageID="1410330" href="forum.php?threadID=1404320#1410330">
<img src="images/icons/mail.png" alt="New" title="Unread messages!" /></a> <em>
   <span class="timestamp" unixtime="1477785794">12:03 AM UTC</span></em>
<br />
</div>
                                <div class="message-body replyalternate1">
                                        <div class="message-contents" fromUserID="28372">
                                                And not to a confirmed infected, like Vash or HR
                                        </div>
                                </div>

                                <div style="clear:both"></div>
                                </div>

<--! start -->
<div class="reply replyborder2 replyalternate2 userID72101">
   <a name="1410331"></a>
<div class="message-head replyalternate2 leftRule">
  <strong>
    <a href="profile.php?userID=72101">Vecna
       <img style="display:none;" class="userOnlineImg" userID="72101" src="images/icons/online.png" alt="Online" title="User currently logged on"
       />
       (788  <img src="images/icons/points.png" alt="D" title="webDiplomacy points" />
       )</a>
  </strong>
<br />
<a style="display:none;" class="messageIconForum" threadID="1404320" messageID="1410331" href="forum.php?threadID=1404320#1410331">
<img src="images/icons/mail.png" alt="New" title="Unread messages!" /></a> <em>
<span class="timestamp" unixtime="1477785928">12:05 AM UTC</span></em><br /></div>
                                <div class="message-body replyalternate2">
                                        <div class="message-contents" fromUserID="72101">
                                                Sup Vash, you the last hiding mafia? That why Ezio refused to shoot you to clear himself?
                                        </div>
                                </div>

                                <div style="clear:both"></div>
).
-endif.
