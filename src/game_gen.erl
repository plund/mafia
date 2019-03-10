%%%-------------------------------------------------------------------
%%% @author Peter Lund <peter@liber>
%%% @copyright (C) 2017, Peter Lund
%%% @doc
%%%
%%% @end
%%% Created : 28 Oct 2017 by Peter Lund <peter@liber>
%%%-------------------------------------------------------------------
-module(game_gen).

%% API
-export([regenerate_history/2,
         regenerate_history_phase/2,
         update_current_txt/2,
         update_current_html/3,
         get_html/2
        ]).

-include("mafia.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generate history pages
%% @end
%%--------------------------------------------------------------------
regenerate_history(M, G) -> regen_history(M, G).

regenerate_history_phase(GNum, Phase = #phase{})
  when is_integer(GNum) ->
    regen_historyI2(GNum, Phase).

%%--------------------------------------------------------------------
%% @doc Generate current text page
%% @end
%%--------------------------------------------------------------------
update_current_txt(GameNum, Opts) when is_integer(GameNum) ->
    update_current_txt(?rgame(GameNum), Opts);
update_current_txt([G], Opts) ->
    %% update current html here too!
    FileName = mafia_file:game_phase_full_fn(G, ?current),
    write_text(FileName, Opts).

%%--------------------------------------------------------------------
%% @doc Generate current html page
%% @end
%%--------------------------------------------------------------------
update_current_html(GameNum, Phase, Opts) when is_integer(GameNum) ->
    update_current_html(?rgame(GameNum), Phase, Opts);
update_current_html([G], Phase, Opts) ->
    FileName = mafia_file:game_phase_full_fn(?html, G, ?current),
    Title = ["Game Status ", mafia_print:print_phase(Phase)],
    write_html(FileName, Title, Opts).

%%--------------------------------------------------------------------
%% @doc Return html
%% @end
%%--------------------------------------------------------------------
get_html(Title, Opts) ->
    {PrevLink, NextLink} = get_links(Opts),
    Body = mafia_print:print_votes(Opts),
    [?HTML_PAGE_START_LINKS(Title, " border=0", PrevLink, NextLink),
     Body,
     ?HTML_PAGE_END].

%%%===================================================================
%%% Internal functions
%%%===================================================================

regen_history(M, {G = #mafia_game{}, Phase}) ->
    regen_history(M, {G#mafia_game.game_num, Phase});
regen_history(M = #message{}, G) ->
    regen_history(M#message.time, G);
regen_history(Time, G = #mafia_game{}) ->
    regen_history(Time, G#mafia_game.game_num);
regen_history(Time, GNum) ->
    regen_historyI(Time, GNum).

%%--------------------------------------------------------------------

regen_historyI(Time, GNum)
  when is_integer(Time), is_integer(GNum) ->
    [G] = ?rgame(GNum),
    DL = mafia_time:get_prev_deadline(G, Time),
    regen_historyI(Time, GNum, DL#dl.phase, [G]);
regen_historyI(Time, {GKey, Phase = #phase{}}) ->
    regen_historyI(Time, GKey, Phase, ?rgame(GKey)).

regen_historyI(_, _, #phase{ptype = ?game_start}, _) -> ok;
regen_historyI(Time, GNum, Phase = #phase{}, [G]) ->
    ?dbg(Time, {"=== REGENERATE HISTORY ===", GNum, Phase}),
    regen_historyI2(GNum, Phase),
    case G#mafia_game.game_end of
        ?undefined -> ok;
        _ ->
            ?dbg(Time, {"=== REGENERATE GAME STATUS ===", GNum}),
            %% ?dbg(stacktrace()),
            regen_historyI2(GNum, #phase{ptype = ?game_ended})
    end,
    ok.

regen_historyI2(GNum, Phase) ->
    [G] = ?rgame(GNum),
    Opts = [{?game_key, GNum},
            {?phase, Phase}],
    FNTxt = mafia_file:game_phase_full_fn(G, Phase),
    FNHtml= mafia_file:game_phase_full_fn(?html, G, Phase),
    regen_hist_txt(FNTxt, Opts),
    regen_hist_html(FNHtml, Phase, Opts).

regen_hist_txt(FNTxt, Opts) ->
    write_text(FNTxt, Opts).

regen_hist_html(FNHtml, Phase = #phase{}, Opts) ->
    Title = ["History ", mafia_print:print_phase(Phase)],
    write_html(FNHtml, Title, Opts).

%%--------------------------------------------------------------------

write_text(FileName, Opts) ->
    {ok, Fd} = file:open(FileName, [write]),
    Opts2 = Opts ++ [{?dev, Fd}],
    mafia_print:print_votes(Opts2),
    file:close(Fd).

write_html(FileName, Title, Opts) ->
    Opts2 = Opts ++ [{?mode, ?html}],
    {ok, Fd} = file:open(FileName, [write]),
    io:format(Fd, "~s", [get_html(Title, Opts2)]),
    file:close(Fd),
    ok.

%%--------------------------------------------------------------------

get_links(Opts) ->
    GNum = proplists:get_value(?game_key, Opts),
    Phase = proplists:get_value(?phase, Opts),
    PrevPhase = mafia_time:decr_phase(Phase),
    PLinkF = fun(Ptype, Num) ->
                     ["&lt;&lt; ",
                      dptypestr(Ptype),
                      " ", ?i2l(Num)]
             end,
    NLinkF = fun(Ptype, Num) ->
                     [dptypestr(Ptype),
                      " ", ?i2l(Num),
                      " &gt;&gt;"]
             end,
    PrevLink =
        case PrevPhase of
            ?undefined -> "";
            #phase{num = 0} -> "";
            #phase{num = PDayNum, ptype = PPtype} ->
                web_impl:hist_link("game_status", GNum, PPtype, PDayNum, PLinkF)
        end,
    NextPhase = mafia_time:inc_phase(Phase),
    NextLink =
        case NextPhase of
            ?undefined -> "";
            #phase{num = NDayNum, ptype = NPtype} ->
                web_impl:hist_link("game_status", GNum, NPtype, NDayNum, NLinkF)
        end,
    {PrevLink, NextLink}.

dptypestr(?night) -> "Night";
dptypestr(?day) -> "Day".

%%--------------------------------------------------------------------

