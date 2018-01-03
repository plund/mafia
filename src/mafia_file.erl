-module(mafia_file).

-export([manual_cmd_to_file/2,
         manual_cmd_from_file/2,
         dl_info_to_file/1,

         settings_fn/1,
         th_filenames/3, %% FN of thread file, relative run dir

         cmd_filename/1, %% FN of manual commands issued, relative run dir

         game_phase_full_fn/2, %% full FN to out text file
         game_phase_full_fn/3,

         game_link_and_text/2, %% link to out text file, relative to DOCROOT
         %%                       (for web page)
         game_link_and_text/3,
         cnt_filename/0,
         deadline_fn/1,

         get_path/1
        ]).

-include("mafia.hrl").

-define(CURRENT_GAME_FN, "game_status").

%% Add Cmd to file if missing
manual_cmd_to_file(G, Cmd) ->
    FN = cmd_filename(G),
    DoAppend =
        case file:consult(FN) of
            {error, enoent} -> true;
            {ok, CmdsOnFile} ->
                not lists:member(Cmd, CmdsOnFile)
        end,
    if DoAppend ->
            {ok, Fd} = file:open(FN, [append]),
            io:format(Fd, "~999p.\n", [Cmd]),
            file:close(Fd),
            added;
       not DoAppend ->
            not_added
    end.

manual_cmd_from_file(G, Cmd) ->
    FN = cmd_filename(G),
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

dl_info_to_file(PollInfo) ->
    FN = deadline_fn(PollInfo#dl_poll_info.game_num),
    DeadlineInfo = lists:flatten(io_lib:format("~999p.\n", [PollInfo])),
    file:write_file(FN, DeadlineInfo, [append]).

%% -----------------------------------------------------------------------------

settings_fn(GNum) ->
    TimeStamp = filename_timestamp_suffix(),
    "game_settings/m" ++ ?i2l(GNum) ++ "_" ++ TimeStamp ++ ".txt".

%% -----------------------------------------------------------------------------
%% Filename to thread store file, relative to run dir (src).
th_filenames(Game, ThId, PageNum) ->
    FileName = th_filename(Game, ThId, PageNum),
    TarBallName = FileName ++ ".tgz",
    {FileName, TarBallName}.

-define(THREAD_DIR, "thread_pages").

find_gameprefix_for_thread(ThId) ->
    {ok, GameDirs} = file:list_dir(?THREAD_DIR),
    %% ?dbg({gdirs, GameDirs}),
    ThIdStr = integer_to_list(ThId),
    GameDirTokens = [string:tokens(D, "_") || D <- GameDirs],
    Res = [X || X = [_, ThStr]  <- GameDirTokens,
                ThStr == ThIdStr],
    case Res of
        [[Pre, _]] -> Pre ++ "_";
        _ -> ""
    end.

th_filename(?undefined, ThId, Page) ->
    GamePrefix = find_gameprefix_for_thread(ThId),
    %% ?dbg({gprefix, GamePrefix}),
    th_filename2(GamePrefix, ThId, Page);
th_filename(G, _ThId, Page) ->
    GamePrefix = game_file_prefix(G),
    #mafia_game{thread_id = GThId, signup_thid = SuThId} = G,
    ThId = if is_integer(GThId) -> GThId;
              is_integer(SuThId) -> SuThId
           end,
    th_filename2(GamePrefix, ThId, Page).

th_filename2(GamePrefix, ThId, Page) ->
    DirName = ?THREAD_DIR,
    %% verify_exist(DirName),
    DirName2 = GamePrefix ++ ?i2l(ThId),
    verify_exist(filename:join(DirName, DirName2)),
    BaseName = ?i2l(Page) ++ ".txt",
    filename:join([DirName, DirName2, BaseName]).

%% -----------------------------------------------------------------------------

cmd_filename(G = #mafia_game{}) ->
    DirName =  "command_files",
    %% verify_exist(DirName),
    GamePrefix = game_file_prefix(G),
    BaseName = GamePrefix ++ "manual_cmds.txt",
    filename:join([DirName, BaseName]).

cnt_filename() ->
    DirName = "counters",
    %% verify_exist(DirName),
    Suffix = filename_timestamp_suffix(),
    FN = "counters_" ++ Suffix ++ ".txt",
    filename:join(DirName, FN).

%% "game_data/m31/dl_poll_info.txt".
deadline_fn(GNum) ->
    Dir1 = "game_data",
    verify_exist(Dir1),
    Dir2 = filename:join(Dir1, "m" ++ ?i2l(GNum)),
    verify_exist(Dir2),
    filename:join(Dir2, "dl_poll_info.txt").

filename_timestamp_suffix() ->
    %% down to secs, utc
    %% "170118Z090801"
    mafia_print:print_time([{?t_mode, ?file_suffix}]).

%% -----------------------------------------------------------------------------


game_link_and_text(G, Phase) ->
    game_link_and_text(?text, G, Phase).

%% {"/m28/current_game_status.txt", "current_game_status.txt"}
-spec game_link_and_text(?text | ?html,
                         thread_id() | #mafia_game{},
                         #phase{} | ?current)
                        -> {string(), string()}.
game_link_and_text(Mode, G, Phase) ->
    {GameDir, PhaseFN} = dir_and_filename(Mode, G, Phase),
    Href = filename:join(["/", GameDir, PhaseFN]),
    Link = PhaseFN,
    {Href, Link}.

game_phase_full_fn(G, Phase) ->
    game_phase_full_fn(?text, G, Phase).

-spec game_phase_full_fn(?text | ?html,
                         integer() | #mafia_game{},
                         #phase{} | ?current | ?game_ended)
                        -> string().
game_phase_full_fn(Mode, GNum, Phase) when is_integer(GNum) ->
    [G] = ?rgame(GNum),
    game_phase_full_fn(Mode, G, Phase);
game_phase_full_fn(Mode, G = #mafia_game{}, Phase) ->
    {GameDir, PhaseFN} = dir_and_filename(Mode, G, Phase),
    DirName = filename:join(get_path(h_doc_root), GameDir),
    verify_exist(DirName),
    filename:join(DirName, PhaseFN).

dir_and_filename(Mode, G, Phase) ->
    {GameDir, FilePrefix} = game_prefixes(G),
    PhaseBaseFN =
        case Phase of
            ?current -> ?CURRENT_GAME_FN;
            #phase{ptype = ?game_ended} -> ?CURRENT_GAME_FN;
            _ -> phase_base_fn(FilePrefix, Phase)
        end,
    PhaseFN = PhaseBaseFN ++ suffix(Mode),
    {GameDir, PhaseFN}.

suffix(?text) -> ".txt";
suffix(?html) -> ".html".

get_path(P) -> mafia_lib:get_path(P).

phase_base_fn(FilePrefix, #phase{ptype = ?game_start}) ->
    FilePrefix ++ "start";
phase_base_fn(FilePrefix, Phase = #phase{}) ->
    PhStr = case Phase#phase.ptype of
                ?day -> "d";
                ?night -> "n"
            end ++ ?i2l(Phase#phase.num),
    %% calculate "m25_d1"
    FilePrefix ++ PhStr.

game_prefixes(G) ->
    Pre = game_file_prefix2(G),
    {Pre, Pre ++ "_"}.

game_file_prefix(G) ->
    game_file_prefix2(G) ++ "_".

game_file_prefix2(G = #mafia_game{}) ->
    "m" ++ ?i2l(G#mafia_game.game_num).

verify_exist(DirName) ->
    case file:read_file_info(DirName) of
        {error, enoent} ->
            file:make_dir(DirName);
        _ -> ok
    end.

%% -----------------------------------------------------------------------------
%% EUNIT tests
%% -----------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% erlang crashed when trying to meck init
%% meck:expect(init, get_argument, 1, {ok, [["/my_mecked_path"]]}),
-define(DIR_LIST, ["m333_2345"]).

file_name_test() ->
    Mods = [file, mafia_lib],
    meck:new([mafia_lib, mafia_db], [passthrough]),
    meck:new(file, [passthrough, unstick]),
    meck:expect(mafia_lib, get_path, 1, "/my_mecked_path"),
    meck:expect(file, read_file_info, 1, {ok, file_info_data}),
    meck:expect(file, list_dir, 1, {ok, ?DIR_LIST}),
    meck:expect(mafia_db, getv, fun(time_offset) -> ?undefined end),
    ?assertEqual(
       "/my_mecked_path/m27/m27_d1.txt",
       game_phase_full_fn(?text,
                          #mafia_game{game_num = 27},
                          #phase{num = 1, ptype = ?day})
      ),
    ?assertEqual(
       "/my_mecked_path/m28/m28_n3.txt",
       game_phase_full_fn(?text,
                          #mafia_game{game_num = 28},
                          #phase{num = 3, ptype = ?night})
      ),
    ?assertEqual(
       "/my_mecked_path/m28/m28_n3.html",
       game_phase_full_fn(?html,
                          #mafia_game{game_num = 28},
                          #phase{num = 3, ptype = ?night})
      ),
    ?assertEqual(
       "/my_mecked_path/m28/game_status.html",
       game_phase_full_fn(
         ?html,
         #mafia_game{game_num = 28},
         ?current)
      ),
    ?assertEqual(
       "/my_mecked_path/m26/game_status.txt",
       game_phase_full_fn(
         ?text,
         #mafia_game{game_num = 26},
         #phase{ptype = ?game_ended})
      ),
    ?assertEqual({"/m29/m29_d2.txt", "m29_d2.txt"},
                 game_link_and_text(?text,
                                    #mafia_game{game_num = 29},
                                    #phase{num = 2, ptype = ?day})
                ),
    ?assertEqual({"/m29/game_status.txt", "game_status.txt"},
                 game_link_and_text(?text,
                                    #mafia_game{game_num = 29},
                                    ?current)
                ),
    ?assertEqual({"/m28/m28_n3.html", "m28_n3.html"},
                 game_link_and_text(?html,
                                    #mafia_game{game_num = 28},
                                    #phase{num = 3, ptype = ?night})
                ),
    ?assertEqual("command_files/m28_manual_cmds.txt",
                 cmd_filename(#mafia_game{game_num = 28, thread_id = 1234})
                ),
    ?assertEqual("thread_pages/1234/11.txt",
                 th_filename(?undefined, 1234, 11)
                ),
    ?assertEqual("thread_pages/m28_4567/11.txt",
                 th_filename(#mafia_game{game_num = 28, thread_id = 4567},
                             1234, 11)
                ),
    ?assertEqual({"thread_pages/m28_4567/11.txt",
                  "thread_pages/m28_4567/11.txt.tgz"},
                 th_filenames(#mafia_game{game_num = 28, thread_id = 4567},
                             1234, 11)
                ),
    ?assertEqual("thread_pages/m333_2345/11.txt",
                 th_filename(?undefined, 2345, 11)
                ),
    meck:unload(Mods).
