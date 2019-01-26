-module(web_game_settings).

-export([game_settings/3,
         is_ready_to_go/2,
         write_settings_file/1,

         update_game_settings/2
        ]).

-include("mafia.hrl").

-define(UNSET, "(unset)").
-define(BInitGame, "Initiate New Game").
-define(BCreateGame, "Create New Game").
-define(BUpdate, "Update Game Settings").
-define(BReload, "Reload Game").
-define(BStart, "Start Game").
-define(BStartNow, "Start Game Now").

game_settings(Sid, Env, In) ->
    PQ = httpd:parse_query(In),
    Button = proplists:get_value("button", PQ),
    GNum = proplists:get_value("game_num", PQ),

    if GNum == ?undefined  ->
            game_settings_list(Sid);

       Button == ?BInitGame;
       Button == ?BCreateGame  ->
            init_new_game(Sid, Env, PQ);

       Button == ?BStart;
       Button == ?BStartNow ->
            game_settings_start(Sid, Env, Button, PQ);

       Button == ?undefined;
       Button == ?BUpdate;
       Button == ?BReload ->
            game_settings_update(Sid, Env, Button, PQ)
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
         [[["<tr><td>\r\n",
            "<a href=\"?game_num=", ?i2l(GNum), "\">",
            ?i2l(GNum), " - ", Title, "</a>"
            "</td></tr>\r\n"]
           || {GNum, Title} <- GNumTitles],
          "<tr><td align=center>"
          "<form action=\"/e/web/game_settings\" method=post>\r\n"
          "<input name=button value=\"", ?BInitGame, "\" type=submit>\r\n"
          "</form>\r\n"
          "</td></tr>\r\n"]
        },
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + B + C, ?none}.

init_new_game(Sid, Env, PQ) ->
    IsSecure = web:is_secure(Env),
    Pass = web_impl:get_arg(PQ, "password"),
    GNStr = web_impl:get_arg(PQ, "game_num"),
    Site = case web_impl:get_arg(PQ, "site") of
               ?undefined -> ?wd2;
               SiteStr -> ?l2a(SiteStr)
           end,
    Responses = init_game(IsSecure, Pass, catch ?l2i(GNStr), Site),
    GNums = web_impl:game_nums_rev_sort(),
    GNumsGrps = mafia_lib:split_into_groups(10, GNums),
    ExStrs = string:join(
               [string:join([?i2l(N) || N <- GNs], ", ")
                || GNs <- GNumsGrps], "<br>"),
    ServerKeeperInfo =
        case ?getv(?server_keeper) of
            {SK, _} ->
                ["Server Keeper (", SK, ")"];
            _ ->
                ""
        end,
    PwF =
        fun(user) -> "serverkeeper";
           (pass) -> Pass;
           (info) ->
                [
                 "<tr><td colspan=2 width=400>\r\n"
                 "<font size=-1>\r\n"
                 "Only the \r\n",
                 ServerKeeperInfo,
                 " may initiate the creation of a new game.<br>\r\n"
                 "</font>"
                 "</td></tr>"
                ];
           (buttons) -> [?BCreateGame];
           (extra_fields) -> []
        end,
    Sites = [?wd2, ?vDip, ?webDip],
    Attr = fun(S) when S == Site -> " selected";
              (_) -> ""
           end,
    POpt = fun(S) ->
                   ["<option value=\"", ?a2l(S), "\"", Attr(S),
                    ">", ?a2l(S), "</option>\r\n"]
           end,
    Opts = ["<select name=\"site\">>\r\n",
            [POpt(S) || S <- Sites],
            "</select>"],

    {A, Body} =
        {web_impl:del_start(Sid, ?BInitGame, 0),
         ["<tr><td align=center>"
          "<form action=\"/e/web/game_settings\" method=post>\r\n"
          "New Game Number: <input name=game_num type=text size=5 value=\"",
          GNStr, "\"> "
          "Site: ", Opts,
          "</td></tr>\r\n"
          "<tr><td align=center>"
          "<font size=-2>\r\n"
          "(Existing non-selectable game numbers: <br>", ExStrs, ")"
          "</font>"
          "</td></tr>\r\n"
          "<tr><td  align=center>\r\n",
          if not IsSecure ->
                  display_tls_info(Env, "?button=" ++ ?BInitGame);
             true ->
                  enter_user_pw_box(PwF)
          end,
          "\r\n</form>\r\n",
          "</td></tr>"
          "<tr><td  align=center>\r\n",
          "<a href=\"/\">Back to Front Page</a>"
          "</td></tr>"
         ]
        },
    RBody = present_responses(Responses),
    R = web:deliver(Sid, RBody),
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + R + B + C, ?none}.

init_game(IsSec, Pass, GNum, Site) ->
    Fun =
        fun({sec, false}) ->
                {true, {error, "You must use secure connection"}};
           ({gn, {'EXIT', _}}) ->
                {true, {error, "You must give a game number"}};
           ({gn, GN}) when is_integer(GN) ->
                case ?rgame(GN) of
                    [] -> false;
                    _ -> {true, {error, "Game exists already"}}
                end;
           (password) ->
                case is_serverkeeper_password_ok(Pass) of
                    true -> false;
                    false -> {true, {error, "Wrong password"}}
                end;
           (_) -> false
        end,
    case lists:filtermap(Fun, [{sec, IsSec}, {gn, GNum}, password]) of
        [] ->
            mafia:initiate_game(GNum, Site),
            [{info, "Game number " ++ ?i2l(GNum) ++ " created"}];
        Errors -> Errors
    end.

game_settings_update(Sid, Env, Button, PQ) ->
    IsSecure = web:is_secure(Env),
    GNStr = web_impl:get_arg(PQ, "game_num"),
    GameSett = web_impl:get_arg(PQ, "game_settings"),
    User = web_impl:get_arg(PQ, "user"),
    Pass = web_impl:get_arg(PQ, "password"),
    {IsRunning, TmpStartAllowed, TmpResponses} =
        maybe_update_game(GNStr, User, Pass, GameSett),
    {StartAllowed, Responses} =
        if IsSecure ->
                {TmpStartAllowed, TmpResponses};
           not IsSecure ->
                {?false, []}
        end,
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
            {SK, _} ->
                [" and the Server Keeper (", SK, ")"];
            _ ->
                ""
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
                 " may change the settings of a game.<br>\r\n"
                 "The GMs need a password to modify the game settings or to "
                 "start the game.<br>\r\n"
                 "A randomly generated password can be retrieved from the "
                 "Server Keeper.\r\n"
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
         ["<tr><td><center>\r\n"
          "<form action=\"/e/web/game_settings\" method=post>\r\n"
          "<input name=game_num type=hidden value=", GNStr, ">\r\n"
          "<textarea name=\"game_settings\" rows=15 cols=70 required>\r\n",
          SettingsText,
          "</textarea>\r\n",
          if not IsRunning, not IsSecure ->
                  display_tls_info(Env);
             not IsRunning ->
                  enter_user_pw_box(PwF);
             true -> ""
          end,
          "\r\n</form>\r\n"
          "</center>",
          settings_info(),
          "</td></tr>"]
        },
    RBody = present_responses(Responses),
    R = web:deliver(Sid, RBody),
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + R + B + C, ?none}.

present_responses(Responses) ->
    ["<tr><td><table align=center>",
     lists:foldl(fun pres_resp/2, "", Responses),
     "</table></td></tr>"
    ].

pres_resp({Type, Txt}, Acc) ->
    TextColour =
        case Type of
            info -> "black";
            error -> "red";
            warning -> "blue"
        end,
    Acc ++
        case Type of
            info ->
                ["<tr><td><i><font color=", TextColour, ">", Txt,
                 "</font></i></td></tr>\r\n"];
            _ -> ["<tr><td><font color=", TextColour, ">", Txt,
                  "</font></td></tr>\r\n"]
        end.

display_tls_info(Env) ->
    display_tls_info(Env, "").

display_tls_info(Env, Extra) ->
    {IsNumeric, Host, ScriptName} = web:host_info(Env),
    SecureUrl = "https://" ++ Host ++
        if IsNumeric ->
                ":" ++ ?i2l(?SECUREPORT);
           true -> ""
        end ++
        ScriptName ++ Extra,
    ["<table align=center cellpadding=1 ", ?BG_TURQUOISE, ">\r\n",
     "<tr><td><center>"
     "Warning: Your connection is <b>not encrypted!</b><br>"
     "(User and password must be encrypted)"
     "<p>"
     "If you are a GM and have a password and want to change the game settings "
     "of this game, then you need to reload this page by clicking <br>"
     "<a href=\"" ++ SecureUrl ++ "\">",
     SecureUrl, "</a>, <br>"
     "<p>"
     "When accessing this site using an encrypted connection "
     "first time with a new web browser, "
     "you will need to <b>accept the self-signed server certificate</b> "
     "(and make a security exemption). "
     "The certificate is self-signed since the server keeper does not want "
     "to pay money to a Certificate Authority."
     "</center></td></tr></table>"].

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

-define(GAME_FIELDS,
        [gms, name, thread_id, signup_thid, start_time, time_zone, dst_zone,
         players_orig, day_hours, night_hours, site, role_pm]).

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
    tr_key_val(A, G#mafia_game.thread_id) ++ gts(G, As);
gts(G, [A = signup_thid | As]) ->
    tr_key(A, game_thread(G#mafia_game.signup_thid)) ++ gts(G, As);
gts(G, [A = day_hours | As]) ->
    tr_key(A, ?i2l(G#mafia_game.day_hours)) ++ gts(G, As);
gts(G, [A = night_hours | As]) ->
    tr_key(A, ?i2l(G#mafia_game.night_hours)) ++ gts(G, As);
gts(G, [A = role_pm | As]) -> tr_key(A, role_pm(G)) ++ gts(G, As);
gts(G, [A = site | As]) -> tr_key(A, site(G)) ++ gts(G, As);
gts(G, [A = time_zone | As]) -> tr_key(A, game_time_zone(G)) ++ gts(G, As);
gts(G, [A = start_time | As]) ->
    tr_key(A, game_start_time(G)) ++ gts(G, As);
gts(G, [A = dst_zone | As]) -> tr_key(A, game_dst_zone(G)) ++ gts(G, As);
gts(G, [A = players_orig | As]) ->
    tr_key(A, game_users(G#mafia_game.players_orig)) ++ gts(G, As).

%% translate key
tr_key(players_orig, Str) -> tr_key(players, Str);
tr_key(A, Str) -> ?a2l(A) ++ "=" ++ Str ++ "\n".

tr_key_val(thread_id, ?undefined) -> "";
tr_key_val(thread_id, Id) -> tr_key(thread_id, game_thread(Id)).

game_users([]) -> ?UNSET;
game_users(Users) -> string:join([?b2l(U) || U <- Users], ",").

game_thread(?undefined) -> ?UNSET;
game_thread(Id) -> ?i2l(Id).

game_name(#mafia_game{name = ?undefined}) -> ?UNSET;
game_name(#mafia_game{name = Name}) -> ?b2l(Name).

role_pm(#mafia_game{role_pm = ?undefined}) -> ?UNSET;
role_pm(#mafia_game{role_pm = Url}) -> ?b2l(Url).

site(#mafia_game{site = Site}) -> ?a2l(Site).

game_start_time(#mafia_game{start_time = ?undefined}) -> ?UNSET;
game_start_time(#mafia_game{start_time = Time}) ->
    mafia_print:print_time(Time, ?local).

game_time_zone(#mafia_game{time_zone = ?undefined}) -> ?UNSET;
game_time_zone(#mafia_game{time_zone = TZ}) -> ?i2l(TZ).

game_dst_zone(#mafia_game{dst_zone = ?undefined}) -> ?UNSET;
game_dst_zone(#mafia_game{dst_zone = DstZone}) -> ?a2l(DstZone).

maybe_update_game(GNStr, User, Pass, GameSett) ->
    GNum = ?l2i(GNStr),
    G = hd(?rgame(GNum)),
    if G#mafia_game.thread_id == ?undefined ->
            maybe_update_game2(G, User, Pass, GameSett);
       true ->
            {?true, ?false,
             [{info, "Game is running and cannot be edited"}]}
    end.

maybe_update_game2(G, User, Pass, GameSett) ->
    {G2, Es2} =
        case mug0(G, User, Pass, GameSett) of
            Acc = {#mafia_game{}, _} -> Acc;
            Es -> {G, Es}
        end,
    {IsReady, G3, Es3} = is_ready_to_go(G, {G2, Es2}),

    %% signup thread polling requires these to be set:
    %% signup_thid, name, time_zone, start_time, dst_zone
    SignupDef =
        fun(#mafia_game{signup_thid = ?undefined}) -> ?false;
           (_) -> ?true
        end,
    CanDLsBeGenerated =
        fun(#mafia_game{start_time = ?undefined}) -> ?false;
           (#mafia_game{time_zone = ?undefined}) -> ?false;
           (#mafia_game{dst_zone = ?undefined}) -> ?false;
           (_) -> ?true
        end,
    SignupPolling =
        fun(_G) ->
                SignupDef(_G) and CanDLsBeGenerated(_G)
        end,
    UpdateInfo =
        if G3 /= G ->
                G4 =
                    if G#mafia_game.signup_thid /=
                       G3#mafia_game.signup_thid ->
                            G3#mafia_game{page_to_read = 1};
                       true -> G3
                    end,
                %% generate new deadlines if possible
                DoGenDLs = CanDLsBeGenerated(G4),
                G5 =
                    if DoGenDLs ->
                            mafia_time:initial_deadlines(G4);
                       true -> G4
                    end,
                ?dwrite_game(game_w1, G5),
                Res1 = [{info, "The game was updated"}],
                case not SignupPolling(G) and SignupPolling(G5) of
                    ?true ->
                        game:start_polling(G#mafia_game.game_num),
                        Res1 ++ [{info, "Thread polling started"}];
                    ?false -> Res1
                end;
           true ->
                [{info, "The game was NOT updated"}]
        end,
    Es4 = Es3 ++ UpdateInfo,
    {false, IsReady, Es4}.

mug0(_G, User, Pass, _GameSett)
  when User == ""; Pass == "" ->
    Es = if User == "" -> [{warning, "No User Given."}];
            true -> []
         end,
    if Pass == "" -> Es ++ [{warning, "No Password Given."}];
       true -> Es
    end;
mug0(G, User, Pass, GameSett) ->
    case is_user_and_password_ok(G, User, Pass) of
        true ->
            update_game_settings(G, GameSett);
        false ->
            [{error, "This combination of user and password does not exist."}]
    end.

is_user_and_password_ok(_, "serverkeeper", Pass) ->
    is_serverkeeper_password_ok(Pass);
is_user_and_password_ok(G, User, Pass) ->
    GSite = G#mafia_game.site,
    Allowed = [?b2l(U) || U <- G#mafia_game.gms],
    case lists:member(User, Allowed) of
        false -> false;
        true ->
            ok == mafia_lib:check_password(User, GSite, Pass)
    end.

is_serverkeeper_password_ok(Pass) ->
    case ?getv(?server_keeper) of
        {SkName, SkSite} ->
            ok == mafia_lib:check_password(SkName, SkSite, Pass);
        _ ->
            false
    end.

is_ready_to_go(CurG, {G, Es}) ->
    %% Check if ready to update and go
    AllChecks = ?GAME_FIELDS ++ [duplicates, start_info],
    IsOk = fun(false, {_, Go, Eso}) ->
                   {false, Go, Eso};
              (WasOk, {IsOko, Go, Eso}) ->
                   {WasOk andalso IsOko, Go, Eso}
           end,
    lists:foldl(
      fun(Field, Acc = {WasOk, _, _}) ->
              IsOk(WasOk, is_ready_to_go(Field, CurG, Acc))
      end,
      {true, G, Es},
      AllChecks).

is_ready_to_go(gms, CurG, {_, G, Es}) ->
    {G3, Es2} =
        if [] /= G#mafia_game.gms ->
                {G, Es};
           true ->
                {G2, Type} =
                    if [] /= G#mafia_game.gms ->
                            {G#mafia_game{gms = CurG#mafia_game.gms},
                             warning};
                       true ->
                            {G, error}
                    end,
                {G2,
                 Es ++
                     [{Type,
                       "There must be at least one GM left after update."}]}
        end,
    IsOk = [] /= G3#mafia_game.gms,
    {IsOk, G3, Es2};
is_ready_to_go(thread_id, _, {_, G, Es}) ->
    {true, G, Es};
is_ready_to_go(signup_thid, _, {_, G, Es}) ->
    {true, G, Es};
is_ready_to_go(name, _, {_, G, Es}) ->
    IsOk = ?undefined /= G#mafia_game.name,
    Es2 = if IsOk -> Es;
             true -> Es ++ [{error, "Parameter 'name' must be set."}]
          end,
    {IsOk, G, Es2};
is_ready_to_go(start_time, _, {_, G, Es}) ->
    IsOk = ?undefined /= G#mafia_game.start_time,
    Es2 = if IsOk -> Es;
             true -> Es ++ [{error, "Parameter 'start_time' must be set."}]
          end,
    {IsOk, G, Es2};
is_ready_to_go(time_zone, _, {_, G, Es}) ->
    IsOk = ?undefined /= G#mafia_game.time_zone,
    Es2 = if IsOk -> Es;
             true -> Es ++ [{error, "Parameter 'time_zone' must be set."}]
          end,
    {IsOk, G, Es2};
is_ready_to_go(dst_zone, _, {_, G, Es}) ->
    IsOk = ?undefined /= G#mafia_game.dst_zone,
    Es2 = if IsOk -> Es;
             true -> Es ++ [{error, "Parameter 'dst_zone' must be set"}]
          end,
    {IsOk, G, Es2};
is_ready_to_go(players_orig, _, {_, G, Es}) ->
    IsOk = [] /= G#mafia_game.players_orig,
    Es2 = if IsOk -> Es;
             true -> Es ++ [{error, "Parameter 'players' must be set."}]
          end,
    {IsOk, G, Es2};
is_ready_to_go(day_hours, _, Acc) -> Acc;
is_ready_to_go(night_hours, _, Acc) -> Acc;
is_ready_to_go(role_pm, _, Acc) -> Acc;
is_ready_to_go(site, _, Acc) -> Acc;
is_ready_to_go(duplicates, CurG, {_, G, Es}) ->
    %% check not same user in both gms and players_orig
    %% if problem reset both fields to orignal before writing
    #mafia_game{gms = GMs, players_orig = Ps} = G,
    {_Uniqs, Dupls} =
        lists:foldl(fun(E, {U, D}) ->
                            case lists:member(E, U) of
                                false -> {[E|U], D};
                                true -> {U, [E|D]}
                            end
                    end,
                    {[],[]},
                    GMs ++ Ps),
    IsOk = [] == Dupls,
    {G2, Es2} =
        if IsOk -> {G, Es};
           true ->
                DUsers =
                    string:join([?b2l(B) || B <- lists:usort(Dupls)], ", "),
                {G#mafia_game{gms = CurG#mafia_game.gms,
                              players_orig = CurG#mafia_game.players_orig,
                              players_rem = CurG#mafia_game.players_rem},
                 Es ++
                     [{error,
                       "Users: " ++ DUsers ++ " exist(s) more than once in "
                       "the group of GMs and players"}]}
        end,
    {IsOk, G2, Es2};
is_ready_to_go(start_info, _, {IsOk, G, Es}) ->
    InfoStr =
        if IsOk ->
                ["The game has currently ",
                 ?i2l(length(G#mafia_game.players_orig)),
                 " players and MAY BE STARTED now. "
                 "Please review the settings carefully before starting."];
           true ->
                "The game CAN NOT BE started now."
        end,
    Es2 = Es  ++ [{info, InfoStr}],
    {IsOk, G, Es2}.


%% empty binary to undefined
-define(eb2ud(B), case B of
                      <<>> -> ?undefined;
                      _ -> B
                  end).

%% @doc read in settings to #mafia_game{}
-type error_list() :: [{'error', string()}].
-spec update_game_settings(#mafia_game{}, string()
                          ) -> {#mafia_game{}, error_list()}.
update_game_settings(G, GameSett) ->
    NewConf = [split_on_first_equal_sign(P)
               || P <- string:tokens(GameSett, "\r\n")],
    {Values, _Unset} =
        lists:partition(fun({_, ?UNSET}) -> false; (_) -> true end,
                        NewConf),
    Values2 = [{K, string:strip(V)} || {K, V} <- Values],
    {G2, EL} = process_input(Values2, {G, []}),
    if G2#mafia_game.day_hours == 0,
       G2#mafia_game.night_hours == 0 ->
            {G2, EL ++
                 [{error, "Both day_hours and night_hours can not be 0"}]};
       true ->
            {G2, EL}
    end.

split_on_first_equal_sign(P) ->
    mafia_lib:split_on_first_char(P, $=).

process_input(KeyValues, Acc) -> pri(KeyValues, Acc).

pri([F = {"gms", _} | T], Acc) -> pri(T, pr_user_list(F, Acc));
pri([F = {"signup_thid", _} | T], Acc) -> pri(T, pr_int(F, Acc));
pri([{"name", Name} | T], {G, Es}) ->
    pri(T, {G#mafia_game{name = ?eb2ud(?l2b(Name))}, Es});
pri([F={"day_hours", _} | T], Acc) -> pri(T, pr_int(F, Acc));
pri([F={"night_hours", _} | T], Acc) ->  pri(T, pr_int(F, Acc));
pri([{"role_pm", Url} | T], {G, Es}) ->
    pri(T, {G#mafia_game{role_pm = ?eb2ud(?l2b(Url))}, Es});
pri([F={"site", _} | T], Acc) -> pri(T, pr_site(F, Acc));
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
    Site = G#mafia_game.site,
    ReadF = fun(U) -> case ?ruserUB(U, Site) of
                          [#user{name = {Name, _}}] ->
                              {true, ?b2l(Name)};
                          _ ->
                              {false, U}
                      end
            end,
    Users = [ReadF(string:strip(U)) || U <- string:tokens(GmStr, ",")],
    {Good, Bad} = lists:partition(fun({Bool, _}) -> Bool end, Users),
    Users2 = [?l2b(U) || {_, U} <- Good],
    Es2 = Es ++ [{error,
                  Field ++ ": User '" ++ U ++
                      "' does not exist on site " ++ ?a2l(Site) }
                 || {_, U} <- Bad],
    case Field of
        "gms" ->
            {G#mafia_game{gms = Users2}, Es2};
        "players" ->
            Players = mafia_lib:to_bin_sort(Users2),
            {G#mafia_game{players_orig = Players,
                          players_rem = Players}, Es2}
    end.

min_thid(#mafia_game{site = ?webDip}) -> ?MinThId;
min_thid(#mafia_game{site = ?vDip}) -> ?MinThIdvDip;
min_thid(#mafia_game{site = ?wd2}) -> 0.

max_thid(#mafia_game{site = ?webDip}) -> ?MaxThId;
max_thid(#mafia_game{site = ?vDip}) -> ?MaxThIdvDip;
max_thid(#mafia_game{site = ?wd2}) -> 99999.

pr_int({Field, IntStr}, {G, Es}) ->
    MinThId = min_thid(G),
    MaxThId = max_thid(G),
    case catch ?l2i(IntStr) of
        {'EXIT', _} ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is not and integer."}]};
        Int when (Field == "signup_thid") and
                 (Int < MinThId) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is too low."}]};
        Int when (Field == "signup_thid") and
                 (Int > MaxThId) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is too high."}]};
        Int when ((Field == "day_hours")
                  or (Field == "night_hours")) and
                 (Int < 0) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " is negative"}]};
        Int when (Field == "time_zone") and
                 ((Int < -12) or (Int > 12)) ->
            {G, Es ++ [{error, Field ++ ": " ++ IntStr ++
                            " must be an integer between -12 and +12"}]};
        Int ->
            G2 = case Field of
                     "signup_thid" -> G#mafia_game{signup_thid = Int};
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
                    {Ye, Es2} = check_int({F, "Year", YeS}, Es, 4, 2012, 2100),
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

pr_site({Field, Site}, {G, Es}) ->
    Sites = [?webDip, ?vDip, ?wd2],
    SiteStrs = [?a2l(St) || St <- Sites],
    case lists:member(Site, SiteStrs) of
        true ->
            {G#mafia_game{site = ?l2a(Site)}, Es};
        false ->
            {G, Es ++ [{error, Field ++ ": Only the following sites are "
                        "allowed: " ++ string:join(SiteStrs, ", ")}]}
    end.

pr_dst_zone({Field, Zone}, {G, Es}) ->
    DstZones = mafia_time:dst_change_date() ++ [?none],
    ZoneStrs = [?a2l(DZ) || DZ <- DstZones],
    %% [?none, ?eu, ?usa, ?australia, ?new_zeeland].
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
        "'start_time' is the local time in your time zone. "
        "The correct format is: YYYY-MM-DD HH:MM:SS.<br>\r\n"
        "</li><li>"
        "'time_zone' is the <i>normal time (without DST) offset</i> to "
        "Greenwich (UTC). "
        "Some examples: Sweden 1, UK 0, New York -5, California -8, Sydney 10 "
        "and Wellington 12.<br>\r\n"
        "</li><li>"
        "'dst_zone' is either 'eu', 'usa', 'australia', "
        "'new_zeeland' or 'none'. "
        "See <a href=dst_changes>DST Changes</a>. "
        "Please make a request for other DST zones if you need it.\r\n"
        "</li><li>"
        "When all these values are OK, you may set off the bot listen in "
        "on the game thread (after starting it), by also giving the bot "
        "the thread id of your webdiplomacy.net forum thread."
        "</li>"
        "</ul>".

game_settings_start(Sid, Env, _Button, PQ) ->
    IsSecure = web:is_secure(Env),
    GNStr = web_impl:get_arg(PQ, "game_num"),
    User = web_impl:get_arg(PQ, "user"),
    Pass = web_impl:get_arg(PQ, "password"),
    if IsSecure ->
            ?dbg({start, user_password, User, Pass});
       not IsSecure -> ok
    end,
    ThreadId = web_impl:get_arg(PQ, "thread_id"),
    [G] = ?rgame(?l2i(GNStr)),
    WasThreadSet = ?undefined /= G#mafia_game.thread_id,
    {IsThreadSet, Responses} =
        maybe_set_thread_id(G, User, Pass, catch ?l2i(ThreadId)),
    Body =
        if WasThreadSet;
           IsThreadSet ->
                ["<tr><td><center>"
                 "GAME M", GNStr, " IS RUNNING NOW!"
                 "<p>"
                 "<a href=/>Go back to the mafia front page</a>"
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
                 "<tr><td>After you have started the game thread on "
                 "webdiplomacy.net or on vdiplomacy.com you need to tell the "
                 "bot what thread id your new game thread has.\r\n"
                 "<p>"

                 "<ol><li>"
                 "Find the game thread id\r\n"

                 "<ol type=a><li>"
                 "For a webdiplomacy.net/New forum game do the following: "
                 "<ol type=i><li>"
                 "Go to the Game Forum by clicking "
                 "<a href=\""
                 "http://webdiplomacy.net/contrib/phpBB3/viewforum.php?f=4"
                 "\">"
                 "http://webdiplomacy.net/contrib/phpBB3/viewforum.php?f=4"
                 "</a>"
                 "</li><li>"
                 "Click on the <b>TITLE</b> of the game thread"
                 "\r\n"
                 "</li><li>"
                 "In the location window in the top you find an URL that looks "
                 "like this:<br>\r\n"
                 "<pre>\r\n"
                 "   http://webdiplomacy.net/contrib/phpBB3/viewtopic.php"
                 "?f=4&t=NNN\r\n"
                 "</li><li>"
                 "The thread id, that you are looking for, is the "
                 "number marked above as NNN that you find after "
                 "<code>\"?f=4&t=\"</code>\r\n"
                 "</li></ol>"

                 "<li>"
                 "For a vdiplomacy.com game do the following: "
                 "<ol type=i><li>"
                 "Open the newly created game thread,"
                 "</li><li>"
                 "In the location window in your browser you find an URL that "
                 "looks like shown below:<br>\r\n"
                 "<pre>\r\n"
                 "   http://vdiplomacy.com/forum.php?threadID=NNN\r\n"
                 "</pre>\r\n"
                 "</li><li>"
                 "The thread id, that you are looking for, is the "
                 "number marked above as NNN, that you find after "
                 "<code>\"?threadID=\"</code>\r\n"
                 "</li>"
                 "</ol></li>"

                 "</li></ol>"

                 "<li>"
                 "Start your game by inserting the game thread id in the "
                 "field below, and press the \"",
                 ?BStartNow,
                 "\" button. \r\n"
                 "</li></ol>"
                 "</ol>"

                 "</td></tr>\r\n"
                 "</table></td></tr>\r\n"
                 "<tr><td>"
                 %% Form
                 "<form action=\"/e/web/game_settings\" method=post>\r\n"
                 "<input name=game_num type=hidden value=", GNStr, ">\r\n",
                 if not IsSecure ->
                         display_tls_info(Env);
                    true ->
                         enter_user_pw_box(F)
                 end,
                 "\r\n</form>\r\n"
                 "</td></tr>"
                ]
        end,
    A = web_impl:del_start(Sid, "Starting M" ++ GNStr, 0),
    B = web:deliver(Sid, Body),
    C = web_impl:del_end(Sid),
    {A + B + C, ?none}.

maybe_set_thread_id(_, _, _, {'EXIT', _}) ->
    {false, [{error, "Thread id is not an integer"}]};
maybe_set_thread_id(_, User, Pass, _)
  when User == ""; Pass == "" ->
    Es = if User == "" -> [{warning, "No User Given."}];
            true -> []
         end,
    Es2 = if Pass == "" -> Es ++ [{warning, "No Password Given."}];
             true -> Es
          end,
    {false, Es2};
maybe_set_thread_id(#mafia_game{signup_thid = SuId}, _, _, _)
  when not is_integer(SuId) ->
    {false,
     [{error, "The signup thread id must be set before starting the game"}]};
maybe_set_thread_id(#mafia_game{signup_thid = SuId},
                    _, _, ThId)
  when ThId =< SuId ->
    {false,
     [{error, "Game thread is not a later thread than the sign-up thread"}]};
maybe_set_thread_id(G, User, Pass, ThreadId) ->
    case is_user_and_password_ok(G, User, Pass) of
        true ->
            msti2(G,
                  ThreadId,
                  min_thid(G),
                  max_thid(G));
        false ->
            [{error, "This combination of user and password does not exist."}]
    end.

msti2(_G, ThId, MinThId, _) when ThId =< MinThId ->
    {false, [{error, "Thread id has a too low value"}]};
msti2(_G, ThId, _, MaxThId) when ThId >  MaxThId ->
    {false, [{error, "Thread id has a too high value"}]};
msti2(G, ThId, _, _) ->
    {IsReady, G2, Es} = is_ready_to_go(G, {G, []}),
    if IsReady ->
            %% START GAME!
            G3 = G2#mafia_game{
                   thread_id = ThId,
                   page_to_read = 1
                   %% Can be removed after M30 has started
                   %% or I set it to 1 on the server
                  },
            G4 = mafia_time:initial_deadlines(G3),
            ?dwrite_game(game_w2, G4),
            write_settings_file(G4),
            game:start_polling(G4#mafia_game.game_num),
            {true, Es ++ [{info, "Thread Id was set"},
                          {info, "GAME STARTED!"}]};
       true ->
            {false, Es}
    end.

write_settings_file(GNum) when is_integer(GNum) ->
    write_settings_file(?rgame(GNum));
write_settings_file([]) -> {error, game_not_found};
write_settings_file([G]) ->
    write_settings_file(G);
write_settings_file(G = #mafia_game{}) ->
    SettingsFN = mafia_file:settings_fn(G#mafia_game.game_num),
    Settings = get_game_settings(G),
    file:write_file(SettingsFN, Settings).
