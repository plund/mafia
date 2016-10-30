-module(mafia).

-compile(export_all).

-define(ThId, "1404320").
-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").
%-define(URL, "http://webdiplomacy.net/forum.php?newsendtothread=1404320").
-define(Url2, "http://webdiplomacy.net/forum.php?threadID=1404320&page-thread=1#threadPager").
%-define(FulLURL, "http://webdiplomacy.net/forum.php?newsendtothread=1404320&viewthread=1404320&reply=success#postbox").

-record(kv_store,
        {key,
         value
        }).

-type thread_id() :: integer(). 
-type page_num() :: integer(). 
-type msg_id() :: integer(). 

-record(page,
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

t() ->
    t(1).
       
t(PageNum) ->
    setup_mnesia(),
    inets:start(),
    Url = make_url(PageNum),
    case httpc:request(Url) of
        {ok, {_StatusLine, _Headers, Body}} ->
            %io:format("~s\n", [Body]),
            print_usr_msgs(Body);
        {ok, {_StatusCode, Body}} ->
            print_usr_msgs(Body);
        {ok, _ReqId} -> unexp2;
        {error, _Reason} ->
            error
    end.

make_url(PageNum) ->
    ?UrlBeg ++ ?ThId ++ ?UrlMid ++ integer_to_list(PageNum) ++ ?UrlEnd.

print_usr_msgs(Body) ->
    B2 = get_thread_section(Body),
    print_usr_msgs2(B2).

print_usr_msgs2("") -> done;
print_usr_msgs2(Body) ->
    B3 = rm_to_after(Body, ["<div class=\"reply", "<div class=\"message-head", "profile.php?user", ">"]),
    {B4, UserRaw} = read_to_before(B3, "<"),
    User = strip(UserRaw),
    B4a = rm_to_after(B4, "messageID=\""),
    {B4a2, MsgId} = read_to_before(B4a, "\""),
    B4b = rm_to_after(B4a2, "unixtime=\""),
    {B4c, TimeRaw} = read_to_before(B4b, "\""),
    Time = fix_time(TimeRaw),
    B5 = rm_to_after(B4c, ["<div class=\"message-contents\"", ">"]),
    {B6, MsgRaw} = read_to_before(B5, "</div>"),
    Msg = strip_fix(MsgRaw),
    if User /= "" -> io:format("\n"
                               "User : ~s\n"
                               "MsgId: ~s\n"
                               "Time : ~s\n"
                               "Wrote: \"~s\"\n", [User, MsgId, Time, Msg]);
       true -> ok
    end,
    print_usr_msgs2(B6).

get_thread_section(Body) ->
    B2 = rm_to_after(Body, "<div class=\"thread threadID1404320"),
    {_, ThreadStr} = read_to_before(B2, "<div class=\"thread thread"),
    ThreadStr.

fix_time(TimeRaw) ->
    %% {17104,{0,3,2}}
    try 
        {Days1970, {HH,MM,SS}} = calendar:seconds_to_daystime(list_to_integer(TimeRaw)),
        {Y, M, D} = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days({1970,1,1}) + Days1970),
        io_lib:format("~s-~s-~sZ~s:~s:~s", [p(Y), p(M), p(D), p(HH), p(MM), p(SS)])
    catch _:_ -> ""
    end.

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

setup_mnesia() ->
    case mnesia:create_schema([node()]) of
        {error,{_,{already_exists,_}}} -> schema_existed_already;
        Other -> Other
    end,
    case mnesia:start() of
        ok -> mnesia_start_ok;
        Other2 -> Other2
    end,
    create_table(kv_store),
    create_table(page),
    create_table(message),
    mnesia:dirty_write(#kv_store{key=thread_str, value = ?ThId}),
    mnesia:dirty_write(#kv_store{key=last_page, value = 0}).

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
rec_info(page) -> record_info(fields, page);
rec_info(message) -> record_info(fields, message).
