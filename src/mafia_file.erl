-module(mafia_file).

-export([manual_cmd_to_file/2,
         manual_cmd_from_file/2,

         th_filenames/2, %% FN of thread file, relative run dir

         cmd_filename/1, %% FN of manual commands issued, relative run dir

         game_phase_full_fn/2, %% full FN to out text file
         game_link_and_text/2, %% link to out text file, relative to DOCROOT
         %%                       (for web page)
         cnt_filename/0,

         get_path/1
        ]).

-include("mafia.hrl").

-define(CURRENT_GAME_FN, "current_game_status.txt").

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

%% Filename to thread store file, relative to run dir (src).
th_filenames(ThId, PageNum) ->
    FileName = th_filename(ThId, PageNum),
    TarBallName = FileName ++ ".tgz",
    {FileName, TarBallName}.

th_filename(Thread, Page) ->
    DirName = "thread_pages",
    %% verify_exist(DirName),
    GamePrefix = game_file_prefix(Thread),
    DirName2 = GamePrefix ++ ?i2l(Thread),
    verify_exist(filename:join(DirName, DirName2)),
    BaseName = ?i2l(Page) ++ ".txt",
    filename:join([DirName, DirName2, BaseName]).

%% -----------------------------------------------------------------------------

cmd_filename(ThId) ->
    DirName =  "command_files",
    %% verify_exist(DirName),
    GamePrefix = game_file_prefix(ThId),
    BaseName = GamePrefix ++ ?i2l(ThId) ++ "_manual_cmds.txt",
    filename:join([DirName, BaseName]).

cnt_filename() ->
    DirName = "counters",
    %% verify_exist(DirName),
    Suffix = filename_timestamp_suffix(),
    FN = "counters_" ++ Suffix ++ ".txt",
    filename:join(DirName, FN).

filename_timestamp_suffix() ->
    %% down to secs, utc
    %% "170118Z090801"
    mafia_print:print_time([{t_mode, file_suffix}]).

%% -----------------------------------------------------------------------------


%% For ref to text version
game_link_and_text(G, ?game_ended) ->
    %% move "current" to game dir
    {GameDir, _FilePrefix} = game_prefixes(G),
    Href = filename:join(["/", GameDir, ?CURRENT_GAME_FN]),
    Link = ?CURRENT_GAME_FN,
    {Href, Link};
game_link_and_text(G, Phase) ->
    {GameDir, FilePrefix} = game_prefixes(G),
    PhaseFN = phase_fn(FilePrefix, Phase),
    Href = filename:join(["/", GameDir, PhaseFN]),
    Link = PhaseFN,
    {Href, Link}.

game_phase_full_fn(G, Phase) ->
    {GameDir, FilePrefix} = game_prefixes(G),
    PhaseFN = if Phase == ?game_ended -> ?CURRENT_GAME_FN;
                 true -> phase_fn(FilePrefix, Phase)
              end,
    DirName = filename:join(get_path(h_doc_root), GameDir),
    verify_exist(DirName),
    filename:join(DirName, PhaseFN).

get_path(P) when P == h_srv_root;
                 P == h_doc_root;
                 P == h_log_root;
                 P == repo_dir ->
    {ok, [[Path]]} = init:get_argument(P),
    Path.

phase_fn(FilePrefix, Phase) ->
    {DNum, DoN} = Phase,
    PhStr = case DoN of
                ?day -> "d";
                ?night -> "n"
            end ++ ?i2l(DNum),
    %% calculate "m25_d1.txt"
    FilePrefix ++ PhStr ++ ".txt".

game_prefixes(G) ->
    Pre = game_prefix(G),
    {Pre, Pre ++ "_"}.

game_file_prefix(G) ->
    case game_prefix(G) of
        "" -> "";
        Pre -> Pre ++ "_"
    end.

game_prefix(ThId) when is_integer(ThId) ->
    game_prefix(?rgame(ThId));
game_prefix([]) -> "";
game_prefix([G]) ->
    game_prefix(G);
game_prefix(G) ->
    "m" ++ ?i2l(G#mafia_game.game_num).

verify_exist(DirName) ->
    case file:read_file_info(DirName) of
        {error, enoent} ->
            file:make_dir(DirName);
        _ -> ok
    end.
