%% atom macros to avoid misspelling
-define(undefined, undefined).
-define(true, true).
-define(false, false).
-define(success, success).
-define(failure, failure).
-define(ok, ok).
-define(in, in).
-define(out, out).
-define(error, error).
-define(valid, valid).
-define(invalid, invalid).
-define(positive, positive).
-define(negative, negative).

-define(webDip, webDip). % ":123"
-define(vDip, vDip).     % "v:123"
-define(wd2, wd2).       % "w:123"

-define(escape_sequence, escape_sequence).

-define(stop, stop).
-define(all, all).
-define(none, none).
-define(match, match).
-define(nomatch, nomatch).
-define(dummy, dummy).
-define(new_page, new_page).
-define(add_id, add_id).
-define(changed, changed).
-define(unchanged, unchanged).

-define(continue, continue).
-define(take_import, take_import).

-define(sort, sort).
-define(sort_normal, sort_normal).
-define(sort_words, sort_words).
-define(sort_words_per_post, sort_words_per_post).
-define(sort_last_msg_time, sort_last_msg_time).

-define(kv_store, kv_store).
-define(day, day).
-define(night, night).
-define(phase, phase).
-define(game_start, game_start).
-define(game_ended, game_ended).
-define(current, current).
-define(history, history).
-define(normal, normal).
-define(stats, stats).
-define(total_stats, total_stats).
-define(global, global).
-define(bytes, bytes).

-define(to_dst, to_dst).
-define(to_normal, to_normal).
-define(same, same).

-define(eu, eu).
-define(usa, usa).
-define(australia, australia).
-define(new_zeeland, new_zeeland).

-define(eu_str, "EU").
-define(usa_str, "USA").
-define(australia_str, "Australia").
-define(new_zeeland_str, "New Zeeland").

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
-define(standard_io, standard_io).
-define(return_text, return_text).
-define(html, html).
-define(text, text).
-define(t_mode, t_mode).
-define(file_suffix, file_suffix).
-define(short, short).
-define(long, long).
-define(local, local).
-define(date_only, date_only).
-define(human, human).
-define(extensive, extensive).
%% -define(time, time).
-define(use_time, use_time).
-define(time_zone, time_zone).
-define(dst, dst).
-define(dl_time_diff, dl_time_diff).

-define(sorted, sorted).
-define(seconds, seconds).
-define(earlier, earlier).
-define(later, later).

-define(gm, gm).
-define(player, player).
-define(dead_player, dead_player).
-define(other, other).
-define(console, console).
-define(use_full_line, use_full_line).
-define(noone_died, noone_died).

-define(start_polling, start_polling).
-define(stop_polling, stop_polling).
-define(stopped, stopped).
-define(deadline3min, deadline3min).
-define(deadline, deadline).

%% KV keys
-define(console_tz, console_tz).
-define(dst_game, dst_game).
-define(dst_user, dst_user).
-define(game_key, game_key).
-define(mod_msg, mod_msg).
-define(page_to_read, page_to_read).
-define(server_keeper, server_keeper).
-define(server_admins, server_admins).
-define(thread_id, thread_id).
-define(time_offset, time_offset). %% integer in seconds
-define(timezone_game, timezone_game).
-define(timezone_user, timezone_user).
-define(ntp_offset_secs, ntp_offset_secs).
%% positive offset means that simulated time is in the past.
-define(http_ip, http_ip).
-define(http_interface, http_interface).
