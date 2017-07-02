-module(web_game_settings).

-export([game_settings/3,
         is_ready_to_go/2]).

-include("mafia.hrl").

-define(UNSET, "(unset)").
-define(BUpdate, "Update Game Settings").
-define(BReload, "Reload Game").
-define(BStart, "Start Game").
-define(BStartNow, "Start Game Now").

game_settings(Sid, _Env, In) ->
    PQ = httpd:parse_query(In),
    Button = proplists:get_value("button", PQ),
    GNum = proplists:get_value("game_num", PQ),
    if Button == ?undefined, GNum == ?undefined  ->
            game_settings_list(Sid);
       Button == ?undefined, GNum /= ?undefined  ->
            game_settings_update(Sid, Button, PQ);
       Button == ?BUpdate;
       Button == ?BReload ->
            game_settings_update(Sid, Button, PQ);
       Button == ?BStart;
       Button == ?BStartNow ->
            game_settings_start(Sid, Button, PQ)
    end.

game_settings_list(Sid) ->
    GNums = web_impl:game_nums_rev_sort(),
    GNumTitles =
        [case ?rgame(GNum) of
             [G] when G#mafia_game.name == ?undefined ->
                 {GNum, ""};
             [G] ->
                 {GNum, ?b2l(G#mafia_game.name)}
         end || GNum <- GNums],
    {A, Body} =
        {web_impl:del_start(Sid, "Game Settings", 0),
         [["<tr><td>",
           "<a href=\"?game_num=", ?i2l(GNum), "\">",
           ?i2l(GNum), " - ", Title, "</a>"
           "</td></tr>\r\n"]
          || {GNum, Title} <- GNumTitles]
        },
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + B + C, ?none}.

game_settings_update(Sid, Button, PQ) ->
    GNStr = web_impl:get_arg(PQ, "game_num"),
    GameSett = web_impl:get_arg(PQ, "game_settings"),
    User = web_impl:get_arg(PQ, "user"),
    Pass = web_impl:get_arg(PQ, "password"),
    ?dbg({'PQ', PQ}),
    {IsRunning, StartAllowed, Responses} =
        maybe_update_game(Button, GNStr, User, Pass, GameSett),
    ?dbg({'StartAllowed', StartAllowed}),
    GNum = ?l2i(GNStr),
    SettingsText =
        if Button == ?BReload ->
                get_game_settings(GNum);
           GameSett /= "" ->
                GameSett;
           true ->
                get_game_settings(GNum)
        end,
    ServerKeeperInfo =
        case ?getv(?server_keeper) of
            ?undefined -> "";
            SK -> [" and the Server Keeper (", SK, ")"]
        end,
    PwF =
        fun(user) -> User;
           (pass) -> Pass;
           (info) ->
                [
                 "<tr><td colspan=2 width=400>\r\n"
                 "<font size=-1>\r\n"
                 "Only GMs (listed in the unmodified settings "
                 "above)\r\n",
                 ServerKeeperInfo,
                 " may change the settings of a game.\r\n"
                 "<br>"
                 "To do so a user and password must be given. "
                 "<br>\r\n"
                 "It is the Server Keeper that PMs random "
                 "passwords to "
                 "the GMs.\r\n"
                 "</font>"
                 "</td></tr>"
                ];
           (buttons) ->
                [?BUpdate, ?BReload] ++
                    if StartAllowed -> [?BStart];
                       true -> []
                    end;
           (extra_fields) -> []
        end,
    {A, Body} =
        {web_impl:del_start(Sid, "M" ++ GNStr ++ " Settings", 0),
         ["<tr><td>\r\n"
          "<form action=\"/e/web/game_settings\" method=post>\r\n"
          "<input name=game_num type=hidden value=", GNStr, ">\r\n"
          "<textarea name=\"game_settings\" rows=13 cols=100 required>\r\n",
          SettingsText,
          "</textarea>\r\n",
          settings_info(),
          "<br>\r\n",
          if not IsRunning ->
                  enter_user_pw_box(PwF);
             true -> ""
          end,
          "\r\n</form>\r\n"
          "</td></tr>"]
        },
    RBody = present_responses(Responses),
    R = web:deliver(Sid, RBody),
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + R + B + C, ?none}.

present_responses(Responses) ->
    lists:foldl(fun pres_resp/2, "", Responses).

pres_resp({Type, Txt}, Acc) ->
    TextColour =
        case Type of
            info -> "black";
            error -> "red";
            warning -> "blue"
        end,
    Acc ++ ["<tr><td><font color=", TextColour, ">", Txt,
            "</font></td></tr>\r\n"].

enter_user_pw_box(F) ->
    [
     "<table align=center cellpadding=1 ", ?BG_TURQUOISE, ">\r\n",
     %% Info line
     F(info),
     "\r\n",
     %% Fields
     [["<tr><td>", Title,
       "</td><td>"
       "<input name=", Name, " type=text size=", Size, " value=\"", Value, "\">"
       "</td></tr>\r\n"]
      || {Title, Name, Value, Size} <- F(extra_fields)],

     "<tr><td>User"
     "</td><td>"
     "<input name=user type=text size=20 value=\"", F(user), "\">"
     "</td></tr>\r\n"

     "<tr><td>Password"
     "</td><td>"
     "<input name=password type=text size=20 value=\"", F(pass), "\">"
     "</td></tr>\r\n"

     "<tr><td colspan=2>",
     [["<input name=button value=\"", BStr, "\" type=submit>\r\n"]
      || BStr <- F(buttons)],
     "</td></tr></table>"
    ].

-define(GAME_FIELDS, [gms, name, day_hours, night_hours,
                      time_zone, start_time, dst_zone, players_orig]).

get_game_settings(G = #mafia_game{}, AddFields) ->
    As = ?GAME_FIELDS ++ AddFields,
    gen_text_settings(G, As).

get_game_settings(GNum) when is_integer(GNum) ->
    G = hd(?rgame(GNum)),
    get_game_settings(G);
get_game_settings(G = #mafia_game{}) ->
    As = ?GAME_FIELDS,
    gen_text_settings(G, As).

gen_text_settings(G, As) -> gts(G, As).

gts(_G, []) -> "";
gts(G, [A = gms | As]) ->
    tr_key(A, game_users(G#mafia_game.gms)) ++ gts(G, As);
gts(G, [A = name | As]) -> tr_key(A, game_name(G)) ++ gts(G, As);
gts(G, [A = thread_id | As]) ->
    tr_key(A, ?i2l(G#mafia_game.thread_id)) ++ gts(G, As);
gts(G, [A = day_hours | As]) ->
    tr_key(A, ?i2l(G#mafia_game.day_hours)) ++ gts(G, As);
gts(G, [A = night_hours | As]) ->
    tr_key(A, ?i2l(G#mafia_game.night_hours)) ++ gts(G, As);
gts(G, [A = time_zone | As]) -> tr_key(A, game_time_zone(G)) ++ gts(G, As);
gts(G, [A = start_time | As]) ->
    tr_key(A, game_start_time(G)) ++ gts(G, As);
gts(G, [A = dst_zone | As]) -> tr_key(A, game_dst_zone(G)) ++ gts(G, As);
gts(G, [A = players_orig | As]) ->
    tr_key(A, game_users(G#mafia_game.players_orig)) ++ gts(G, As).

%% translate key
tr_key(players_orig, Str) -> tr_key(players, Str);
tr_key(A, Str) -> ?a2l(A) ++ "=" ++ Str ++ "\n".

game_users([]) -> ?UNSET;
game_users(Users) -> string:join([?b2l(U) || U <- Users], ",").

game_name(#mafia_game{name = ?undefined}) -> ?UNSET;
game_name(#mafia_game{name = Name}) -> ?b2l(Name).

game_start_time(#mafia_game{start_time = ?undefined}) -> ?UNSET;
game_start_time(#mafia_game{start_time = Time}) ->
    mafia_print:print_time(Time, ?local).

game_time_zone(#mafia_game{time_zone = ?undefined}) -> ?UNSET;
game_time_zone(#mafia_game{time_zone = TZ}) -> ?i2l(TZ).

game_dst_zone(#mafia_game{dst_zone = ?undefined}) -> ?UNSET;
game_dst_zone(#mafia_game{dst_zone = DstZone}) -> ?a2l(DstZone).

maybe_update_game(Button, GNStr, User, Pass, GameSett) ->
    GNum = ?l2i(GNStr),
    G = hd(?rgame(GNum)),
    if G#mafia_game.thread_id == ?undefined ->
            {G2, Es2} =
                if Button == ?BUpdate ->
                        case mug0(G, User, Pass, GameSett) of
                            Acc = {#mafia_game{}, _} -> Acc;
                            Es -> {G, Es}
                        end;
                   true -> {G, []}
                end,
            {IsReady, G3, Es3} = is_ready_to_go(G, {G2, Es2}),

            UpdateInfo =
                if G3 /= G ->
                        %% remove generated deadlines
                        ?dwrite_game(G3#mafia_game{deadlines = []}),
                        [{info, "The game was updated"}];
                   Button == ?BUpdate ->
                        [{info, "The game was NOT updated"}];
                   true -> []
                end,
            Es4 = Es3 ++ UpdateInfo,
            {false, IsReady, Es4};
       true ->
            {true, false,
             [{info, "Game is running and cannot be edited"}]}
    end.

mug0(_G, User, Pass, _GameSett)
  when User == ""; Pass == "" ->
    Es = if User == "" -> [{warning, "No User Given."}];
            true -> []
         end,
    if Pass == "" -> Es ++ [{warning, "No Password Given."}];
       true -> Es
    end;
mug0(G, User, Pass, GameSett) ->
    ?dbg({user_pass, User, Pass}),
    mug1(G, ?ruserUB(User), GameSett,
         mafia_lib:check_password(User, Pass)).

mug1(G, [#user{name = Name}], GameSett, ok) -> %% User/PW matches
    ServerKeeper = case ?getv(?server_keeper) of
                       ?undefined -> [];
                       SK -> [?l2b(SK)]
                   end,
    Allowed = G#mafia_game.gms ++ ServerKeeper,
    mug2(G, GameSett, lists:member(Name, Allowed));
mug1(_, _, _, _) ->
    [{error, "This combination of user and password does not exist."}].

mug2(G, GameSett, IsAllowed) when IsAllowed ->  % User is allowed
    ?dbg({sett, GameSett}),
    NewConf = [list_to_tuple(string:tokens(P, "="))
               || P <- string:tokens(GameSett, "\r\n")],
    %% [{"gms","peterlund"}, ...]
    {Values, _Unset} =
        lists:partition(fun({_, ?UNSET}) -> false; (_) -> true end,
                        NewConf),
    Values2 = [{K, string:strip(V)} || {K, V} <- Values],
    ?dbg({values2, Values2}),
    process_input(Values2, {G, []});
mug2(_, _, _) ->
    [{warning, "You are not allowed to edit this game"}].

is_ready_to_go(G, {G3, Es3}) ->
    %% Check if ready to update and go
    IsNameOk = ?undefined /= G3#mafia_game.name,
    Es4 = if IsNameOk -> Es3;
             true -> Es3 ++ [{error, "Parameter 'dst_zone' must be set."}]
          end,
    {G4, Es5} =
        if [] /= G3#mafia_game.gms ->
                {G3, Es4};
           true ->
                {G3b, Type} =
                    if [] /= G#mafia_game.gms ->
                            {G3#mafia_game{gms = G#mafia_game.gms},
                             warning};
                       true ->
                            {G3, error}
                    end,
                {G3b,
                 Es4 ++
                     [{Type,
                       "There must be at least one GM left after update."}]}
        end,
    IsGmsOk = [] /= G4#mafia_game.gms,

    IsPsOk = [] /= G4#mafia_game.players_orig,
    Es6 = if IsPsOk -> Es5;
             true -> Es5 ++ [{error, "Parameter 'players' must be set."}]
          end,

    IsTimeOk = ?undefined /= G4#mafia_game.start_time,
    Es7 = if IsTimeOk -> Es6;
             true -> Es6 ++ [{error, "Parameter 'start_time' must be set."}]
          end,
    IsTzOk = ?undefined /= G4#mafia_game.time_zone,
    Es8 = if IsTzOk -> Es7;
             true -> Es7 ++ [{error, "Parameter 'time_zone' must be set."}]
          end,
    IsDstZoneOk = ?undefined /= G4#mafia_game.dst_zone,
    Es9 = if IsDstZoneOk -> Es8;
             true -> Es8 ++ [{error, "Parameter 'dst_zone' must be set"}]
          end,

    %% check not same user in both gms and players_orig
    %% if problem reset both fields to orignal before writing
    #mafia_game{gms = GMs, players_orig = Ps} = G4,
    {_Uniqs, Dupls} =
        lists:foldl(fun(E, {U, D}) ->
                            case lists:member(E, U) of
                                false -> {[E|U], D};
                                true -> {U, [E|D]}
                            end
                    end,
                    {[],[]},
                    GMs ++ Ps),
    IsDuplsOk = [] == Dupls,
    {G5, Es10} =
        if IsDuplsOk -> {G4, Es9};
           true ->
                DUsers =
                    string:join([?b2l(B) || B <- lists:usort(Dupls)], ", "),
                {G4#mafia_game{gms = G#mafia_game.gms,
                               players_orig = G#mafia_game.players_orig,
                               players_rem = G#mafia_game.players_rem},
                 Es9 ++
                     [{error,
                       "Users: " ++ DUsers ++ " exist(s) more than once in "
                       "the group of GMs and players"}]}
          end,
    IsReadyToGo =
        IsNameOk and IsGmsOk and IsPsOk and
        IsTimeOk and IsTzOk and IsDstZoneOk and IsDuplsOk,
    Es11 = Es10  ++
        [{info,
          if IsReadyToGo ->
                  "The game MAY BE STARTED now (but please review the "
                      "settings carefully before starting)";
             true ->
                  "The game CAN NOT BE started now."
          end}],
    {IsReadyToGo, G5, Es11}.

%% empty binary to undefined
-define(eb2ud(B), case B of
                      <<>> -> ?undefined;
                      _ -> B
                  end).

process_input(KeyValues, Acc) -> pri(KeyValues, Acc).

pri([F = {"gms", _} | T], Acc) -> pri(T, pr_user_list(F, Acc));
pri([{"name", Name} | T], {G, Es}) ->
    pri(T, {G#mafia_game{name = ?eb2ud(?l2b(Name))}, Es});
pri([F={"day_hours", _} | T], Acc) -> pri(T, pr_int(F, Acc));
pri([F={"night_hours", _} | T], Acc) ->  pri(T, pr_int(F, Acc));
pri([F={"time_zone", _} | T], Acc) ->  pri(T, pr_int(F, Acc));
pri([F={"start_time", _} | T], Acc) -> pri(T, pr_start_time(F, Acc));
pri([F={"dst_zone", _} | T], Acc) -> pri(T, pr_dst_zone(F, Acc));
pri([F={"players", _} | T], Acc) ->  pri(T, pr_user_list(F, Acc));
pri([{Par, Value} | T], {G, Es}) ->
    pri(T, {G, Es ++
               [{error,
                 "Unknown parameter: '" ++ Par ++ "' and value '" ++
                     Value ++ "'"}]});
pri([], Acc) -> Acc.

pr_user_list({Field, GmStr}, {G, Es}) ->
    ReadF = fun(U) -> case ?ruserUB(U) of
                          [#user{name = Name}] ->
                              {true, ?b2l(Name)};
                          _ ->
                              {false, U}
                      end
            end,
    Users = [ReadF(string:strip(U)) || U <- string:tokens(GmStr, ",")],
    {Good, Bad} = lists:partition(fun({Bool, _}) -> Bool end, Users),
    Users2 = [?l2b(U) || {_, U} <- Good],
    Es2 = Es ++ [{error, Field ++ ": User '" ++ U ++ "' does not exist" }
                 || {_, U} <- Bad],
    case Field of
        "gms" ->
            {G#mafia_game{gms = Users2}, Es2};
        "players" ->
            {G#mafia_game{players_orig = Users2,
                          players_rem = Users2}, Es2}
    end.

pr_int({Field, IntStr}, {G, Es}) ->
    case catch ?l2i(IntStr) of
        {'EXIT', _} ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is not and integer."}]};
        Int when ((Field == "day_hours")
                  or (Field == "night_hours")) and
                 (Int < 1) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is zero or negative"}]};
        Int when (Field == "time_zone") and
                 ((Int < -12) or (Int > 12)) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " must be an integer between -12 and +12"}]};
        Int ->
            G2 = case Field of
                     "day_hours" -> G#mafia_game{day_hours = Int};
                     "night_hours" -> G#mafia_game{night_hours = Int};
                     "time_zone" -> G#mafia_game{time_zone = Int}
                 end,
            {G2, Es}
    end.

pr_start_time({F, TimeStr}, {G, Es}) ->
    %% 2017-03-26 18:00:00
    %% [["2017","03","26"],["18","00","00"]]
    case string:tokens(TimeStr, " ") of
        [Date, Time] ->
            case [string:tokens(Date, "-"), string:tokens(Time, ":")] of
                [[YeS, MoS, DaS], [HoS, MiS, SeS]] ->
                    {Ye, Es2} = check_int({F, "Year", YeS}, Es, 4, 2017, 2100),
                    {Mo, Es3} = check_int({F, "Month", MoS}, Es2, 2, 0, 12),
                    {Da, Es4} = check_int({F, "Date", DaS}, Es3, 2, 0, 31),
                    {Ho, Es5} = check_int({F, "Hours", HoS}, Es4, 2, 0, 23),
                    {Mi, Es6} = check_int({F, "Minutes", MiS}, Es5, 2, 0, 59),
                    {Se, Es7} = check_int({F, "Seconds", SeS}, Es6, 2, 0, 59),
                    if Es7 == Es ->
                            Time2 = {{Ye, Mo, Da}, {Ho, Mi, Se}},
                            {G#mafia_game{start_time = Time2}, Es7};
                       true ->
                            {G, Es7}
                    end;
                _ ->
                    pr_start_time_error(G, Es, F)
            end;
        _ ->
            pr_start_time_error(G, Es, F)
    end.

pr_start_time_error(G, Es, F) ->
    {G,
     Es ++
         [{error,
           "Parameter '" ++ F ++ "' has wrong format. "
           "The correct format is: YYYY-MM-DD HH:MM:SS."}]}.

par_desc({F, Part, Str}) ->
    Part ++ " ('" ++ Str ++ "') in '" ++ F ++ "'".

check_int(Par = {_, _, Str}, Es, Size, _Min, _Max) when length(Str) /= Size ->
    {error,
     Es ++ [{error,
             par_desc(Par) ++ " must be " ++ ?i2l(Size) ++ " long."}]};
check_int(Par = {_, _, Str}, Es, _Size, Min, Max) ->
    case catch ?l2i(Str) of
        {'EXIT', _} ->
            {error,
             Es ++ [{error, par_desc(Par) ++ " should be an integer."}]};
        Int when Int < Min ->
            {error,
             Es ++ [{error,
                     par_desc(Par) ++ " should be minimum " ++ ?i2l(Min)}]};
        Int when Int > Max ->
            {error,
             Es ++ [{error,
                     par_desc(Par) ++ " should be maximum " ++ ?i2l(Max)}]};
        Int ->
            {Int, Es}
    end.

pr_dst_zone({Field, Zone}, {G, Es}) ->
    ZoneStrs = [?a2l(DZ) || DZ <- mafia_time:dst_change_date()],
    %% [?eu, ?usa, ?australia, ?new_zeeland].
    case lists:member(Zone, ZoneStrs) of
        true ->
            {G#mafia_game{dst_zone = ?l2a(Zone)}, Es};
        false ->
            {G, Es ++ [{error, Field ++ ": Only the following DST zones are "
                        "allowed: " ++ string:join(ZoneStrs, ", ")}]}
    end.

settings_info() ->
    "<ul>"
        "<li>"
        "Any values that are " ++ ?UNSET ++ " must be set before "
        "the game starts. "
        "</li><li>"
        "Please check the settings for previous games for a format template\r\n"
        "</li><li>"
        "'gms' and 'players' are comma separated lists of users "
        "found in the bot <a href=users>User DB</a> \r\n"
        "</li><li>"
        "'name' is your game title."
        "</li><li>"
        "'time_zone' is the normal time offset to Greenwich (no DST). "
        "1 for Sweden, -5 for New York and -8 for California.<br>\r\n"
        "</li><li>"
        "'start_time' is the local time in your time zone. "
        "The correct format is: YYYY-MM-DD HH:MM:SS.<br>\r\n"
        "</li><li>"
        "'dst_zone' is either 'eu', 'usa', 'australia' or 'new_zeeland'. "
        "See <a href=dst_changes>DST Changes</a>\r\n"
        "</li>"
        "</ul>".

game_settings_start(Sid, Button, PQ) ->
    GNStr = web_impl:get_arg(PQ, "game_num"),
    User = web_impl:get_arg(PQ, "user"),
    Pass = web_impl:get_arg(PQ, "password"),
    ThreadId = web_impl:get_arg(PQ, "thread_id"),
    [G] = ?rgame(?l2i(GNStr)),
    WasThreadSet = ?undefined /= G#mafia_game.thread_id,
    {IsThreadSet, Responses} = maybe_set_thread_id(G, User, Pass, ThreadId),
    ?dbg({button, Button, ThreadId}),
    Body =
        if WasThreadSet;
           IsThreadSet ->
                ["<tr><td><center>"
                 "GAME M", GNStr, " IS RUNNING NOW!"
                 "</center></td></tr>\r\n"
                ];
           not IsThreadSet ->
                F = fun(user) -> User;
                       (pass) -> Pass;
                       (info) ->
                            [
                             "<tr><td colspan=2 width=400>"
                             "<font size=-1>"
                             "Make sure this thread id is correct.<br>\r\n"
                             "Start cannot be undone!\r\n"
                             "</font>\r\n"
                             "</td></tr>"
                            ];
                       (buttons) ->
                            [?BStartNow];
                       (extra_fields) ->
                            [{"Thread Id", "thread_id", ThreadId, 20}]
                    end,
                [present_responses(Responses),
                 "<tr><td><table width=600>"
                 "<tr><td>After you have started the game thread on webdiplomacy.net "
                 "you need to tell the bot what thread id number your new thread "
                 "has.\r\n"
                 "<p>"
                 "Find out the game thread id this way: \r\n"
                 "<ol><li>"
                 "In a web browser, open the forum game thread so you can "
                 "read the game thread.<br>\r\n"
                 "</li><li>"
                 "In the location window in the top you find an URL that looks "
                 "like this:<br>\r\n"
                 "<pre>\r\n"
                 "   http://webdiplomacy.net/forum.php?threadID=1479977#1479977\r\n"
                 "</pre>\r\n"
                 "</li><li>"
                 "The thread id you are looking for is the first number you find "
                 "after \"?threadID=\"<br>\r\n"
                 "</li><li>"
                 "Insert this number in the below field, and press the Start "
                 "button. (Don't use the number shown in this example.)\r\n"
                 "</li></ol></td></tr>\r\n"
                 "</table></td></tr>\r\n"
                 "<tr><td>"
                 %% Form
                 "<form action=\"/e/web/game_settings\" method=post>\r\n"
                 "<input name=game_num type=hidden value=", GNStr, ">\r\n",
                 enter_user_pw_box(F),
                 "\r\n</form>\r\n"
                 "</td></tr>"
                ]
        end,
    A = web_impl:del_start(Sid, "Starting M" ++ GNStr, 0),
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + B + C, ?none}.

maybe_set_thread_id(_, _, _, "") ->
    {false, [{error, "No Thread Id Given."}]};
maybe_set_thread_id(_, User, Pass, _)
  when User == ""; Pass == "" ->
    Es = if User == "" -> [{warning, "No User Given."}];
            true -> []
         end,
    Es2 = if Pass == "" -> Es ++ [{warning, "No Password Given."}];
             true -> Es
          end,
    {false, Es2};
maybe_set_thread_id(G, User, Pass, ThreadId) ->
    msti2(G,
          mafia_lib:check_password(User, Pass),
          catch ?l2i(ThreadId)).

msti2(_G, {error, _}, _) ->
    {false, [{error, "This combination of user and password does not exist."}]};
msti2(_G, _PwRes, {'EXIT', _}) ->
    {false, [{error, "Thread id is not an integer"}]};
msti2(_G, _PwRes, ThId) when ThId =< 1479977 ->
    {false, [{error, "Thread id has a too low value"}]};
msti2(_G, _PwRes, ThId) when ThId >  3000000 ->
    {false, [{error, "Thread id has a too high value"}]};
msti2(G, _PwRes, ThId) ->
    {IsReady, G2, Es} = is_ready_to_go(G, {G, []}),
    if IsReady ->
            %% START GAME!
            G3 = G2#mafia_game{
                   thread_id = ThId,
                   players_orig = mafia_lib:to_bin_sort(
                                    G2#mafia_game.players_orig),
                   players_rem = mafia_lib:to_bin_sort(
                                   G2#mafia_game.players_rem),
                   page_to_read = 1
                   %% Can be removed after M30 has started
                   %% or I set it to 1 on the server
                  },
            G4 = mafia_time:initial_deadlines(G3),
            ?dwrite_game(G4),
            mafia:switch_to_game(G#mafia_game.game_num),
            SettingsFN = mafia_file:settings_fn(G4#mafia_game.game_num),
            Settings = get_game_settings(G4, [thread_id]),
            file:write_file(SettingsFN, Settings),
            {true, Es ++ [{info, "Thread Id was set"},
                          {info, "GAME STARTED!"}]};
       true ->
            {false, Es}
    end.
