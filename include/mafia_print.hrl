-define(NumColsInGrp, 6).

%% print params: -record(pp,
-record(
   pp,
   {game  :: ?undefined | #mafia_game{},
    day   :: ?undefined | #mafia_day{},
    players_rem :: ?undefined | [player()], %% for vote-count, non-votes,
                                                % non-posts
    players_vote :: ?undefined | [player()], %% for vote tracker
    game_key :: ?undefined | thread_id(),
    phase :: ?undefined | #phase{} | ?total_stats,
    phase_type :: ?undefined | ?day | ?night | ?game_start
                | ?game_ended | ?total_stats,
    day_num :: ?undefined | integer(),
    message :: ?undefined | #message{},
    site :: ?undefined | site(),
    msg_id :: ?undefined | msg_id(),
    match_expr :: term(),
    dev = ?standard_io,
    mode = ?text :: ?text | ?html,
    t_mode = ?long :: ?file_suffix | ?short | ?long | ?local | ?human
                    | ?date_only | ?extensive,
    period :: ?undefined | number(),   %% Poll period
    use_time :: ?undefined | seconds1970(),
    %% use_time = time to next DL (current game status)
    time_zone = 0 :: integer(),
    dst = false :: boolean(),
    sort = ?sort_normal :: ?sort_normal
                         | ?sort_words
                         | ?sort_words_per_post
                         | ?sort_last_msg_time,
    dl_time_diff :: ?undefined | integer()
   }).
