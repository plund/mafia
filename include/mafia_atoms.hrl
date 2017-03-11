%% atom macros to avoid misspelling
-define(true, true).
-define(false, false).
-define(undefined, undefined).
-define(ok, ok).
-define(error, error).
-define(stop, stop).
-define(none, none).
-define(dummy, dummy).
-define(new_page, new_page).
-define(add_id, add_id).
-define(unchanged, unchanged).

-define(continue, continue).
-define(take_import, take_import).

-define(sort, sort).
-define(normal, normal).
-define(words, words).
-define(words_per_post, words_per_post).

-define(dbuser_ok, dbuser_ok).
-define(dbuser_unver, dbuser_unver).
-define(dbuser_wrong_case, dbuser_wrong_case).
-define(dbuser_none, dbuser_none).

-define(kv_store, kv_store).
-define(day, day).
-define(night, night).
-define(phase, phase).
-define(game_ended, game_ended).
-define(current, current).
-define(total_stats, total_stats).
-define(global, global).
-define(bytes, bytes).

-define(verified, verified).
-define(unverified, unverified).

-define(dev, dev).
-define(utc, utc).
-define(gmt, gmt).
-define(zulu, zulu).
-define(user, user).
-define(game, game).
-define(mode, mode).
-define(period, period).
-define(html, html).
-define(text, text).
-define(t_mode, t_mode).
-define(file_suffix, file_suffix).
-define(short, short).
-define(long, long).
-define(extensive, extensive).
%% -define(time, time).
-define(use_time, use_time).
-define(time_zone, time_zone).
-define(dst, dst).

-define(sorted, sorted).
-define(seconds, seconds).
-define(earlier, earlier).
-define(later, later).

-define(gm, gm).
-define(player, player).
-define(dead_player, dead_player).
-define(other, other).
-define(console, console).

-define(start_polling, start_polling).
-define(stop_polling, stop_polling).
-define(stopped, stopped).
-define(deadline, deadline).

%% KV keys
-define(console_tz, console_tz).
-define(dst_game, dst_game).
-define(dst_user, dst_user).
-define(game_key, game_key).
-define(mod_msg, mod_msg).
-define(page_to_read, page_to_read).
-define(reg_threads, reg_threads).
-define(thread_id, thread_id).
-define(time_offset, time_offset). %% integer in seconds
-define(timer_minutes, timer_minutes).
-define(timezone_game, timezone_game).
-define(timezone_user, timezone_user).
%% positive offset means that simulated time is in the past.
