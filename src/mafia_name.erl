-module(mafia_name).

-include("mafia.hrl").

-export([get_abbrevs/1]).

-import(mafia, [b2l/1, lrev/1]).

%% calculate_3letter_abbrevs_for_remaining_players
get_abbrevs(PlayerRem) ->
    Rems = ["INVALID", ?Unvote
            | ?Extra
            ++ [b2l(UserB) || UserB <- PlayerRem]],
    %%        Uniqu  Orig,  Head, Tail
    Rems2 = [{false, User, "", User} || User <- Rems],

    %% 1. read 3 for all non unique
    L = readN(Rems2, 3),
    c3l_i(L).


%% c3l() ->
%%     c3l(rgame()).

%% c3l([]) -> ok;
%% c3l([G]) ->
%%     Rems = ["INVALID", ?Unvote
%%             | ?Extra
%%             ++ [b2l(UserB) || UserB <- G#mafia_game.players_rem]],
%%     %%        Uniqu  Orig,  Head, Tail
%%     Rems2 = [{false, User, "", User} || User <- Rems],
%%     %% 1. read 3 for all non unique
%%     L = readN(Rems2, 3),
%%     c3l_i(L).

c3l_i(L) ->
    %% 2. mark_check uniques
    {L2, NonUniqs} = mark_check(L),
    %% 3. if not all unique then try fix and goto 2
    case NonUniqs of
        [] ->
            L2;
        NonUniqs ->
            %% split on A
            Probs = lists:usort([element(3,E) || E <- NonUniqs]),
            %% for each A not unique get next unique
            L3 = lists:foldl(
                   fun(Prob, CurL) ->
                           mod_prob(CurL, NonUniqs, Prob)
                   end,
                   L2,
                   Probs),
            c3l_i(L3)
    end.

mod_prob(L, _NonUniqs, Prob) ->
    Probs = [E || E <- L, element(3, E) == Prob],
    Probs2 = remove_until_next_uniq(Probs),
    Fixes = replace_last_read_w_next(Probs2),
    lists:foldl(fun(Fix, AccL) ->
                        Player = element(2, Fix),
                        lists:keyreplace(Player, 2, AccL, Fix)
                end,
                L,
                Fixes).

replace_last_read_w_next(L) ->
    lists:foldr(
      fun({U, O, A, [H|T]}, Acc) ->
              [_|AR] = lrev(A),
              NewA = lrev([H|AR]),
              [{U, O, NewA, T} | Acc];
         ({U, O, A, []}, Acc) ->
              [_|AR] = lrev(A),
              NewA = lrev([$.|AR]),
              [{U, O, NewA, []} | Acc]
      end,
      [],
      L).

remove_until_next_uniq(Probs) ->
    %% check next char on each
    NextChs = [case element(4, E) of
                   [] -> g_char;
                   [H|_] -> H
               end || E <- Probs],
    IsNextUniq = [] == (NextChs -- lists:usort(NextChs)),
    if IsNextUniq -> Probs;
       true ->
            remove_until_next_uniq(remove1(Probs))
    end.

remove1(L) ->
    %% [{U,O,A++[H],T}||{U,O,A,[H|T]}<-L].
    lists:foldr(
      fun({U, O, A, [_H|T]}, Acc) ->
              [{U, O, A, T} | Acc];
         ({U, O, A, []}, Acc) ->
              [{U, O, A, []} | Acc]
      end,
      [],
      L).

readN(L, 0) -> L;
readN(L, N) ->
    readN(read1(L), N-1).

read1(L) ->
    %% [{U,O,A++[H],T}||{U,O,A,[H|T]}<-L].
    lists:foldr(
      fun({U, O, A, [H|T]}, Acc) ->
              [{U, O, A ++ [H], T} | Acc];
         ({U, O, A, []}, Acc) ->
              [{U, O, A ++ [$.], []} | Acc]
      end,
      [],
      L).

mark_check(L) ->
    AllAs = [element(3,E)||E<-L],
    IsUnique = fun(A) -> not lists:member(A, AllAs -- [A]) end,
    L2 = [{IsUnique(A), O, A, T} || {_U, O, A, T} <- L],
    NonUniqs = [E || E <- L2, not element(1,E)],
    {L2, NonUniqs}.
