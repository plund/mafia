-define(WEBPORT, 50666).
-define(SECUREPORT, 50667).
-define(MINUTE_MS, 60000).

-define(SERVER_NAME, "MAFIA TRACKER").
-define(MED_GREEN_HEX, "afefaf").
-define(BG_MED_GREEN, "bgcolor=\"#" ++ ?MED_GREEN_HEX ++ "\"").
-define(TURQUOISE_HEX, "DFFFDF").
-define(BG_TURQUOISE, "bgcolor=\"#" ++ ?TURQUOISE_HEX ++ "\"").

-define(MinThId,    1000111).
-define(MaxThId,    3000000).
-define(MinThIdvDip,  72000).
-define(MaxThIdvDip, 299999).

-define(GSECS_1970, 62167219200).
-define(GDAYS_1970, 719528).
%% calendar:date_to_gregorian_days({1970,1,1}) -> 719528.

-define(DayHours, 24).
-define(MinuteSecs, 60).
-define(HourSecs, 3600).
-define(DaySecs, (?DayHours * ?HourSecs)).

-define(MAX_GM_DL_MINS, 20).
-define(MAX_GM_DL_SECS, (?MAX_GM_DL_MINS * ?MinuteSecs)).

-define(BotUrl, "http://mafia.peterlund.se/").

-define(UrlBeg, "http://webdiplomacy.net/forum.php?threadID=").
-define(UrlvDip, "http://vdiplomacy.com/forum.php?threadID=").
-define(UrlWd2, "http://webdiplomacy.net/contrib/phpBB3/viewtopic.php?f=4&t=").
-define(UrlMid, "&page-thread=").
-define(UrlEnd, "#threadPager").

-define(Unvote, "Unvote").
-define(END, "END").
-define(UNEND, "UNEND").
-define(NoLynch, "No-Lynch").
-define(Extra, [?END, ?UNEND, ?NoLynch]).

%% simple macros
-define(a2l(A), atom_to_list(A)).

-define(b2l(B), binary_to_list(B)).

-define(i2l(I), integer_to_list(I)).

-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).
-define(l2a(L), list_to_atom(L)).
-define(l2u(L), string:to_upper(L)).
-define(lrev(L), lists:reverse(L)).
-define(nbsp(B), mafia_print:nbsp(B)).

-define(IS_SITE_OK(Site),
        (Site == ?webDip orelse Site == ?vDip orelse Site == ?wd2)).

%% combined macros
-define(l2ub(L), ?l2b(?l2u(L))).
-define(b2ul(B), ?l2u(?b2l(B))).
-define(b2ub(B), ?l2b(?b2ul(B))).
-define(e1(T), element(1, T)).

-define(inc_cnt(CntName), mafia_lib:inc_cnt(CntName)).
-define(inc_cnt(CntName, Inc), mafia_lib:inc_cnt(CntName, Inc)).
-define(inc_cnt(CntName, Args, Inc), mafia_lib:inc_cnt(CntName, Args, Inc)).
-define(set(K, V), mafia_db:set(K, V)).
-define(getv(K), mafia_db:getv(K)).

%% -define(dwrite(Obj), mafia_lib:dwrite(gen, Obj)).
-define(dwrite_kv(Obj), mafia_lib:dwrite(kv, Obj)).
-define(dwrite_msg(Obj), mafia_lib:dwrite(msg, Obj)).
-define(dwrite_page(Obj), mafia_lib:dwrite(page, Obj)).
-define(dwrite_stat(Obj), mafia_lib:dwrite(stat, Obj)).
-define(dwrite_user(Obj), mafia_lib:dwrite(user, Obj)).
-define(dwrite_day(Obj), mafia_lib:dwrite(day, Obj)).
-define(dwrite_game(Obj), mafia_lib:dwrite(game, Obj)).

-define(dwrite_game(Tag, Obj), mafia_lib:dwrite(Tag, Obj)).

-define(rmess(MsgKey), mafia_lib:rmess(MsgKey)).
-define(rpage(ThId, Page, Site), mafia_lib:rpage(ThId, Page, Site)).
-define(rpage(Key), mafia_lib:rpage(Key)).
-define(ruser(User, Site), mafia_lib:ruser(User, Site)).
-define(ruserUB(User, Site), mafia_lib:ruserUB(User, Site)).
-define(ruserUB(UserSite), mafia_lib:ruserUB(UserSite)).

-define(rgame(ThId), mafia_lib:rgame(ThId)).
-define(rday(GK, Phase), mafia_lib:rday(GK, Phase)).

-define(regen_history(Term, M, G), game:regen_history(?MODULE, Term, M, G)).

-define(dbg(Term), mafia_lib:dbg(?MODULE, Term)).
-define(dbg(Time, Term), mafia_lib:dbg(?MODULE, Time, Term)).
-define(dbg_str(Str), mafia_lib:dbg_str(Str)).
-define(man(Time, Cmd), mafia_lib:man(Time, Cmd)).

%% background: #ffeeff;
-define(HTML_TAB_START_LINKS(Title, TabAttrStr, PrevL, NextL),
 "<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">
    <title>" ++ Title ++ "</title>
    <style>
       blockquote {
          display: block;
          margin-top: 8px;
          margin-bottom: 8px;
          margin-left: 30px;
          margin-right: 30px;
          padding: 6px;
          border-left: 6px solid #707070;
          quotes: \"\\201C\"\"\\201D\"\"\\2018\"\"\\2019\";

      }
  </style>
  </head>
  <body bgcolor=\"#cfffaf\">
    <center>
      <table" ++ TabAttrStr ++ ">
        <tr><td align=center>
           <table" ++ TabAttrStr ++ ">
              <tr><td align=left width=100><font size=-1>" ++ PrevL ++
            "</font></td>
                  <td align=center><h2>" ++ Title ++ "</h2></td>
                  <td align=right width=100><font size=-1>"++ NextL ++
            "</font></td>
              </tr>
           </table>
        </td></tr>\r\n").

-define(HTML_TAB_END, "
      </table>
    </center>
  </body>
</html>").

-define(HTML_TAB_START(Title, TabAttrStr),
        ?HTML_TAB_START_LINKS(Title, TabAttrStr, "", "")).
