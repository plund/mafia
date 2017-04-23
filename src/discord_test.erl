-module(discord_test).


%% someone else should log the discord channel too if the bot does it.

%% Base URL
%% --------
%% The base URL for all API requests is:
%% https://discordapp.com/api

%% Authentication
%% --------------
%% Authenticating with the Discord API can be done in one of two ways:
%%     Using a bot token gained by registering a bot.
%%     Using an OAuth2 bearer token gained through the OAuth2 API.
%% For all authentication types, authentication is performed with the
%% Authorization HTTP header in the following format:

%%    Authorization: TOKEN_TYPE TOKEN

%% This data can be found on peters discord account on in file
%% "discord_bot_registration_data.txt"
%% App Details
%% Client ID: <discord_client_id>
%% Client Secret: <discord_client_secret>

%% Discord also implements refresh tokens, which can be passed to the token
%% URL for valid authentication tokens.

%% Get Current Application Information
%%    GET/oauth2/applications/@me

%% Discord API use TLS 1.2.

%% HTTP API
%% User Agent
%% Clients using the HTTP API must provide a valid User Agent which specifies
%% information about the client library and version, in the following format:

%%    User-Agent: DiscordBot ($url, $versionNumber)

%% Adding Bots to Guilds
%% A URL can be generated that redirects authenticated users to the add-bot
%% flow, by using the following format (this utilizes the OAuth2 authentication
%% flow, without a callback URL):

%% Added bot to 'lunch' server with this url:
%% https://discordapp.com/api/oauth2/authorize?client_id=<discord_client_id>&scope=bot&permissions=0

%% This data can be found on peters discord account on in file
%% "discord_bot_registration_data.txt"
%% App Bot User
%% Username: <discord_client_user_name>
%% Token: <discord_client_token>
%% Generate a new token?

%% USE TOKEN to connect to server.

%%Authorization: Bot $discord_client_token
%%User-Agent: DiscordBot ($url, $versionNumber)
%%User-Agent: DiscordBot MafiaBot/1.0

%% ---------------------------------------------
%% OAuth2 is other method - not to be used:
%% OAuth2 Application URLs
%% URL                                               Description
%% https://discordapp.com/api/oauth2/authorize       Base authorization URL
%% https://discordapp.com/api/oauth2/token           Token URL
%% https://discordapp.com/api/oauth2/token/revoke    Revocation URL
%% ---------------------------------------------

%% GET https://discordapp.com/api/gateway ->
%% {"url": "wss://gateway.discord.gg"}

%% nslookup gateway.discord.gg
%% Non-authoritative answer:
%% Name:	gateway.discord.gg
%% Address: 104.16.60.37
%% Name:	gateway.discord.gg
%% Address: 104.16.59.37

%% server
%% ssl:start().
%% {ok, ListenSocket} =
%% ssl:listen(9999, [{certfile, "cert.pem"}, {keyfile, "key.pem"},{reuseaddr, true}]).

%% Host https://discordapp.com/api
%% client

-compile(export_all).

-export([discord_client/0]).

discord_client() ->
    ssl:start(),
    %% {ok, Socket} = ssl:connect("localhost", 9999,  [], infinity).
    {ok, Socket} = connect(),
    %% Host should it be same as in connect?
    GetReq = get_req(""),
    ssl:send(Socket, GetReq).

connect() ->
    %% ssl:connect(gateway(), 443, [], infinity).
    ssl:connect(api_host(), 443, [], infinity).

close(Socket) ->
    ssl:close(Socket).

get_req(Args) ->
    "GET /api" ++ Args ++ " HTTP/1.1\r\n"
        "Host: " ++ api_host() ++  "\r\n"
    %% "Token: " ++ token() ++ "\r\n".
        "Authorization: Bot " ++ token() ++ "\r\n"
        "User-Agent: DiscordBot MafiaBot/1.0\r\n"
        "\r\n".

%% GET /guilds/{guild.id}
guild() -> "/guilds/" ++ channel_id().

-define(
   guild_resp,
   "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 16:15:41 GMT\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\nConnection: keep-alive\r\nSet-Cookie: __cfduid=dda1ece30187d8a86900a7bdf888aed8f1492877740; expires=Sun, 22-Apr-18 16:15:40 GMT; path=/; domain=.discordapp.com; HttpOnly\r\nStrict-Transport-Security: max-age=31536000; includeSubDomains\r\nVia: 1.1 google\r\nAlt-Svc: clear\r\nServer: cloudflare-nginx\r\nCF-RAY: 3539e2184b0d761e-ARN\r\n"
   "\r\n"
   "320\r\n"
   "{\"mfa_level\": 0, \"emojis\": [], \"application_id\": null, "
   "\"name\": \"lunch\", "
   "\"roles\": ["
   "{\"hoist\": false, \"name\": \"bots\", \"mentionable\": false, "
   "\"color\": 15158332, \"position\": 1, \"id\": \"305245411260628992\", "
   "\"managed\": false, \"permissions\": 104324161}, "
   "{\"hoist\": false, \"name\": \"@everyone\", \"mentionable\": false, "
   "\"color\": 0, \"position\": 0, \"id\": \"303952298948821003\", "
   "\"managed\": false, \"permissions\": 104324161}], "
   "\"afk_timeout\": 300, \"widget_channel_id\": null, \"region\": \"eu-west\", "
   "\"default_message_notifications\": 0, \"embed_channel_id\": null, "
   "\"explicit_content_filter\": 0, \"splash\": null, \"features\": [], "
   "\"afk_channel_id\": null, \"widget_enabled\": false, "
   "\"verification_level\": 0, \"owner_id\": \"246176108368625664\", "
   "\"embed_enabled\": false, \"id\": \"303952298948821003\", "
   "\"icon\": \"bd569675be5536c93ffcc822d08dd572\"}\r\n" ++
       "0\r\n\r\n"
  ).

%% GET /guilds/{guild.id}/channels
guild_chan() -> "/guilds/" ++ channel_id() ++ "/channels".

-define(
   guild_chan_resp,
   "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 16:26:35 GMT\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\nConnection: keep-alive\r\nSet-Cookie: __cfduid=d5e9c303c873ded1557a1e5daa69c02171492878395; expires=Sun, 22-Apr-18 16:26:35 GMT; path=/; domain=.discordapp.com; HttpOnly\r\nStrict-Transport-Security: max-age=31536000; includeSubDomains\r\nVia: 1.1 google\r\nAlt-Svc: clear\r\nServer: cloudflare-nginx\r\nCF-RAY: 3539f2113e3175ca-ARN\r\n"
   "\r\n"
   "2ba\r\n"
   "[{\"guild_id\": \"303952298948821003\", \"name\": \"generalhhh\", "
   "\"permission_overwrites\": [{\"deny\": 8192, \"type\": \"role\", \"id\": \"303952298948821003\", \"allow\": 0}], "
   "\"topic\": \"\", \"position\": 0, \"last_message_id\": \"305358921336356867\", "
   "\"type\": \"text\", \"id\": \"303952298948821003\", \"is_private\": false},"
   " {\"permission_overwrites\": [], \"name\": \"General\", \"user_limit\": 0, "
   "\"type\": \"voice\", \"position\": 0, \"guild_id\": \"303952298948821003\", \"bitrate\": 64000, \"id\": \"303952298948821004\", \"is_private\": false},"
   " {\"guild_id\": \"303952298948821003\", \"name\": \"testkanal2\", \"permission_overwrites\": [], \"topic\": null, \"position\": 1, \"last_message_id\": null, "
   "\"type\": \"text\", \"id\": \"305247791461040130\", \"is_private\": false}"
   "]\r\n" ++
       "0\r\n\r\n"
  ).

%% GET /channels/{channel.id}
cha_arg() -> "/channels".  %% does not work: HTTP/1.1 404 NOT FOUND

channel_arg() -> "/channels/" ++ channel_id().

resp1() ->
    ""
        "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 13:09:39 GMT\r\n"
        "Content-Type: application/json\r\n"
        "Transfer-Encoding: chunked\r\n"
        "Connection: keep-alive\r\n"
        "Set-Cookie: __cfduid=d508e693738ab630034bc5ecbbe4edcde1492866578; "
        "expires=Sun, 22-Apr-18 13:09:38 GMT; path=/; domain=.discordapp.com; HttpOnly\r\n"
        "Strict-Transport-Security: max-age=31536000; includeSubDomains\r\n"
        "Via: 1.1 google\r\nAlt-Svc: clear\r\nServer: cloudflare-nginx\r\nCF-RAY: 3538d1961d2b7606-ARN\r\n"
        "\r\n"
        "121\r\n"
        "{\"guild_id\": \"303952298948821003\", "
        "\"name\": \"generalhhh\","
        "\"permission_overwrites\":"
        "   [{\"deny\": 8192,"
        "     \"type\": \"role\", "
        "     \"id\": \"303952298948821003\", "
        "     \"allow\": 0"
        "    }], "
        "\"topic\": \"\", "
        "\"position\": 0, "
        "\"last_message_id\": \"305217296786259969\", "
        "\"type\": \"text\", "
        "\"id\": \"303952298948821003\", "
        "\"is_private\": false"
        "}\r\n".


%% GET/channels/{channel.id}/messages
msgs_arg() -> "/channels/" ++ channel_id() ++ "/messages".

-define(
   MSG_RESP1,
   "HTTP/1.1 200 OK\r\n"
   "Date: Sat, 22 Apr 2017 14:38:57 GMT\r\n"
   "Content-Type: application/json\r\n"
   "Transfer-Encoding: chunked\r\n"
   "Connection: keep-alive\r\n"
   "Set-Cookie: __cfduid=da51509938c8dfe38ec86cfe060391b681492871937;"
   " expires=Sun, 22-Apr-18 14:38:57 GMT; path=/; domain=.discordapp.com; HttpOnly\r\n"
   "Strict-Transport-Security: max-age=31536000; includeSubDomains\r\n"
   "Via: 1.1 google\r\n"
   "Alt-Svc: clear\r\n"
   "Server: cloudflare-nginx\r\n"
   "CF-RAY: 35395469ab3d75d6-ARN\r\n"
   "\r\n"
   "41b\r\n"
   "[{\"reactions\":"
   "  [{\"count\": 1,"
   "    \"me\": false,"
   "    \"emoji\": {\"id\": null, \"name\": \"\\ud83d\\ude29\"}},"
   "   {\"count\": 1, \"me\": false,"
   "    \"emoji\": {\"id\": null, \"name\": \"\\ud83d\\udc4c\"}}"
   "  ],"
   "  \"attachments\": [],"
   "  \"tts\": false,"
   "  \"embeds\": [],"
   "  \"timestamp\": \"2017-04-22T05:43:30.241000+00:00\","
   "  \"mention_everyone\": false,"
   "  \"id\": \"305217028451729409\","
   "  \"pinned\": false,"
   "  \"edited_timestamp\": null,"
   "  \"author\":"
   "    {\"username\": \"PeterLund\","
   "     \"discriminator\": \"9570\","
   "     \"id\": \"246176108368625664\","
   "     \"avatar\": \"37ca74bcad0c0d83a5da08c7217b73f4\""
   "    },"
   "  \"mention_roles\": [],"
   "  \"content\": \"test mess f\\u00f6r delete\","
   "  \"channel_id\": \"303952298948821003\","
   "  \"mentions\": [],"
   "  \"type\": 0"
   " },"
   " {\"attachments\": [],"
   "  \"tts\": false,"
   "  \"embeds\": [],"
   "  \"timestamp\": \"2017-04-18T18:43:34.027000+00:00\","
   "  \"mention_everyone\": false,"
   "  \"id\": \"303963786295902208\","
   "  \"pinned\": false,"
   "  \"edited_timestamp\": null,"
   "  \"author\":"
   "    {\"username\": \"PeterLund\","
   "     \"discriminator\": \"9570\","
   "     \"id\": \"246176108368625664\","
   "     \"avatar\": \"37ca74bcad0" ++
   %% CHUNK 2
   "     c0d83a5da08c7217b73f4\"},"
   "     \"mention_roles\": [],"
   "     \"content\": \".\","
   "     \"channel_id\": \"303952298948821003\","
   "     \"mentions\": [], \"type\": 0"
   " }"
   "]\r\n" ++
   %% Chunk 3, messages in reversed order, Last chunk "0" at end
   "0\r\n"
   "\r\n"
  ).

msgs_arg2() -> "/channels/" ++ channel_id() ++
                  "/messages?after=305217028451729409".

-define(
   MSG_RESP2,
   "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 15:11:44 GMT\r\n"
   "Content-Type: application/json\r\nTransfer-Encoding: chunked\r\n"
   "Connection: keep-alive\r\n"
   "Set-Cookie: __cfduid=db8e6de4a23e7ba9a19ec0ae5c83614041492873904; expires=Sun, 22-Apr-18 15:11:44 GMT; path=/; domain=.discordapp.com; HttpOnly\r\n"
   "Strict-Transport-Security: max-age=31536000; includeSubDomains\r\nVia: 1.1 google\r\nAlt-Svc: clear\r\n"
   "Server: cloudflare-nginx\r\nCF-RAY: 3539846e9a8f7618-ARN\r\n"
   "\r\n"
   "1bb\r\n"
   "[{\"attachments\": [], \"tts\": false, \"embeds\": [],"
   "  \"timestamp\": \"2017-04-22T15:07:20.141000+00:00\","
   "  \"mention_everyone\": false, \"id\": \"305358921336356867\","
   "  \"pinned\": false, \"edited_timestamp\": null,"
   "  \"author\":"
   "    {\"username\": \"PeterLund\", \"discriminator\": \"9570\","
   "     \"id\": \"246176108368625664\","
   "     \"avatar\": \"37ca74bcad0c0d83a5da08c7217b73f4\"},"
   "     \"mention_roles\": [],"
   "     \"content\": \"meddelande 3\","
   "     \"channel_id\": \"303952298948821003\","
   "     \"mentions\": [],"
   "     \"type\": 0}]\r\n",
   %% End Chunk
   "0\r\n\r\n"
  ).

channel_id() -> "303952298948821003".

api_host() -> "discordapp.com".
%%Authorization: Bot MTk4NjIyNDgzNDcxOTI1MjQ4.Cl2FMQ.ZnCjm1XVW7vRze4b7Cq4se7kKWs
%%User-Agent: DiscordBot MafiaBot/1.0

gateway() -> "gateway.discord.gg".

token() -> discord:get_reg_val(discord_client_token).

post() ->
    "POST /test/demo_form.php HTTP/1.1\r\n"
        "Host: w3schools.com\r\n"
        "name1=value1&name2=value2\r\n".

-define(
   MSG_HEAD1,
   "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 14:38:57 GMT\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\nConnection: keep-alive\r\nSet-Cookie: __cfduid=da51509938c8dfe38ec86cfe060391b681492871937; expires=Sun, 22-Apr-18 14:38:57 GMT; path=/; domain=.discordapp.com; HttpOnly\r\nStrict-Transport-Security: max-age=31536000; includeSubDomains\r\nVia: 1.1 google\r\nAlt-Svc: clear\r\nServer: cloudflare-nginx\r\nCF-RAY: 35395469ab3d75d6-ARN\r\n\r\n"
  ).
-define(
   MSG_BODY1,
   "41b\r\n[{\"reactions\": [{\"count\": 1, \"me\": false, \"emoji\": {\"id\": null, \"name\": \"\\ud83d\\ude29\"}}, {\"count\": 1, \"me\": false, \"emoji\": {\"id\": null, \"name\": \"\\ud83d\\udc4c\"}}], \"attachments\": [], \"tts\": false, \"embeds\": [], \"timestamp\": \"2017-04-22T05:43:30.241000+00:00\", \"mention_everyone\": false, \"id\": \"305217028451729409\", \"pinned\": false, \"edited_timestamp\": null, \"author\": {\"username\": \"PeterLund\", \"discriminator\": \"9570\", \"id\": \"246176108368625664\", \"avatar\": \"37ca74bcad0c0d83a5da08c7217b73f4\"}, \"mention_roles\": [], \"content\": \"test mess f\\u00f6r delete\", \"channel_id\": \"303952298948821003\", \"mentions\": [], \"type\": 0}, {\"attachments\": [], \"tts\": false, \"embeds\": [], \"timestamp\": \"2017-04-18T18:43:34.027000+00:00\", \"mention_everyone\": false, \"id\": \"303963786295902208\", \"pinned\": false, \"edited_timestamp\": null, \"author\": {\"username\": \"PeterLund\", \"discriminator\": \"9570\", \"id\": \"246176108368625664\", \"avatar\": \"37ca74bcad0" ++
       "c0d83a5da08c7217b73f4\"}, \"mention_roles\": [], \"content\": \".\", \"channel_id\": \"303952298948821003\", \"mentions\": [], \"type\": 0}]\r\n" ++
       "0\r\n\r\n").

-define(
   MSG_HEAD2,
   "HTTP/1.1 200 OK\r\nDate: Sat, 22 Apr 2017 15:11:07 GMT\r\nContent-Type: application/json\r\nTransfer-Encoding: chunked\r\nConnection: keep-alive\r\nSet-Cookie: __cfduid=d1a4741494486c3feaae0504e3ca6bb841492873866; expires=Sun, 22-Apr-18 15:11:06 GMT; path=/; domain=.discordapp.com; HttpOnly\r\nStrict-Transport-Security: max-age=31536000; includeSubDomains\r\nVia: 1.1 google\r\nAlt-Svc: clear\r\nServer: cloudflare-nginx\r\nCF-RAY: 35398383c80a7618-ARN\r\n\r\n"
  ).
-define(
   MSG_BODY2,
   "1bb\r\n"
   "[{\"attachments\": [], \"tts\": false, \"embeds\": [], \"timestamp\": \"2017-04-22T15:07:20.141000+00:00\", "
   "\"mention_everyone\": false, \"id\": \"305358921336356867\", "
   "\"pinned\": false, \"edited_timestamp\": null, "
   "\"author\": {\"username\": \"PeterLund\", \"discriminator\": \"9570\", "
   "\"id\": \"246176108368625664\", \"avatar\": \"37ca74bcad0c0d83a5da08c7217b73f4\"}, "
   "\"mention_roles\": [], \"content\": \"meddelande 3\", "
   "\"channel_id\": \"303952298948821003\", \"mentions\": [], \"type\": 0}]\r\n"
   ++
       "0\r\n\r\n"
  ).

lenghts() ->
    {length(?MSG_BODY1), length(?MSG_BODY2)}.

decode() ->
    discord:decode_json(?MSG_HEAD2 ++ ?MSG_BODY2).
