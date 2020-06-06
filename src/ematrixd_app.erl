%%%-------------------------------------------------------------------
%%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%% @doc ematrix public API
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% This file is part of ematrixd.
%%
%% ematrixd is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or  (at your option) any later version.
%%
%% ematrixd is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public
%% License along with ematrixd.
%% If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(ematrixd_app).

-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(ematrixd, http_port),
    {ok, Ssl} = application:get_env(ematrixd, ssl),
    Routes = [
        {'_', [
               {"/_matrix/client/versions",                                           emd_http_api_versions, []},
               {"/.well-known/matrix/client",                                         emd_http_serverdisco, []},
               {"/_matrix/client/r0/login",                                           emd_http_auth_login, []},
               {"/_matrix/client/r0/logout",                                          emd_http_auth_logout, []},
               {"/_matrix/client/r0/logout/all",                                      emd_http_auth_logout_all, []},
               {"/_matrix/client/r0/register",                                        emd_http_acct_register, []},
               {"/_matrix/client/r0/register/email/requestToken",                     emd_http_acct_register_email_reqtoken, []},
               {"/_matrix/client/r0/register/msisdn/requestToken",                    emd_http_acct_register_msisdn_reqtoken, []},
               {"/_matrix/client/r0/account/password",                                emd_http_acct_password, []},
               {"/_matrix/client/r0/account/password/email/requestToken",             emd_http_acct_password_email_reqtoken, []},
               {"/_matrix/client/r0/account/password/msisdn/requestToken",            emd_http_acct_password_msisdn_reqtoken, []},
               {"/_matrix/client/r0/account/deactivate",                              emd_http_acct_deactivate, []},
               {"/_matrix/client/r0/register/available",                              emd_http_acct_available, []},
               {"/_matrix/client/r0/account/whoami",                                  emd_http_acct_whoami, []},
               {"/_matrix/client/r0/capabilities",                                    emd_http_capabilities, []},
               {"/_matrix/client/r0/user/:user_id/filter",                            emd_http_filter_set, []},
               {"/_matrix/client/r0/user/:user_id/filter/:filter_id",                 emd_http_filter_get, []},
               {"/_matrix/client/r0/sync",                                            emd_http_sync, []},
               {"/_matrix/client/r0/events",                                          emd_http_events, []},
               {"/_matrix/client/r0/initialSync",                                     emd_http_initialsync, []},
               {"/_matrix/client/r0/events/:event_id",                                emd_http_events_get, []},
               {"/_matrix/client/r0/rooms/:room_id/event/:event_id",                  emd_http_room_event_get, []},
               {"/_matrix/client/r0/rooms/:room_id/state/:event_type/:state_key",     emd_http_room_event_state, []},
               {"/_matrix/client/r0/rooms/:room_id/members",                          emd_http_room_members, []},
               {"/_matrix/client/r0/rooms/:room_id/joined_members",                   emd_http_room_joined_members, []},
               {"/_matrix/client/r0/rooms/:room_id/messages",                         emd_http_room_messages, []},
               {"/_matrix/client/r0/rooms/:room_id/initialSync",                      emd_http_room_initialsync, []},
               {"/_matrix/client/r0/rooms/:room_id/send/:event_type/:tx_id",          emd_http_room_send, []},
               {"/_matrix/client/r0/rooms/:room_id/redact/:event_type/:tx_id",        emd_http_room_redact, []},
               {"/_matrix/client/r0/createRoom",                                      emd_http_room_create, []},
               {"/_matrix/client/r0/directory/room/:room_alias",                      emd_http_room_alias, []},
               {"/_matrix/client/r0/joined_rooms",                                    emd_http_user_joined_rooms, []},
               {"/_matrix/client/r0/rooms/:room_id/invite",                           emd_http_room_invite, []},
               {"/_matrix/client/r0/rooms/:room_id/join",                             emd_http_room_join, []},
               {"/_matrix/client/r0/join/:room_id_or_alias",                          emd_http_room_join_with_id_or_alias, []},
               {"/_matrix/client/r0/rooms/:room_id/leave",                            emd_http_room_leave, []},
               {"/_matrix/client/r0/rooms/:room_id/forget",                           emd_http_room_forget, []},
               {"/_matrix/client/r0/rooms/:room_id/kick",                             emd_http_room_kick, []},
               {"/_matrix/client/r0/rooms/:room_id/ban",                              emd_http_room_ban, []},
               {"/_matrix/client/r0/rooms/:room_id/unban",                            emd_http_room_unban, []},
               {"/_matrix/client/r0/directory/list/room/:room_id",                    emd_http_room_visibility, []},
               {"/_matrix/client/r0/publicRooms",                                     emd_http_room_list_public, []},
               {"/_matrix/client/r0/user_directory/search",                           emd_http_user_search, []},
               {"/_matrix/client/r0/profile_directory/:user_id/displayname",          emd_http_profile_displayname, []},
               {"/_matrix/client/r0/profile_directory/:user_id/avatar_url",           emd_http_profile_avatar_url, []},
               {"/_matrix/client/r0/profile_directory/:user_id",                      emd_http_profile_userid, []},
               {"/_matrix/client/r0/voip/turnServer",                                 emd_http_voip_turnserver, []},
               {"/_matrix/client/r0/rooms/:room_id/typing/:user_id",                  emd_http_room_typing, []},
               {"/_matrix/client/r0/rooms/:room_id/receipt/:receipt_type/:user_id",   emd_http_room_receipt_user, []},
               {"/_matrix/client/r0/rooms/:room_id/receipt/:receipt_type/:event_id",  emd_http_room_receipt_event, []},
               {"/_matrix/client/r0/rooms/:room_id/read_markers",                     emd_http_room_read_markers, []},
               {"/_matrix/client/r0/presence/:user_id/status",                        emd_http_user_presence, []},
               {"/_matrix/media/r0/upload",                                           emd_http_media_upload, []},
               {"/_matrix/media/r0/download/:server_name/:media_id",                  emd_http_media_download_mediaid, []},
               {"/_matrix/media/r0/download/:server_name/:media_id/:file_name",       emd_http_media_download_filename, []},
               {"/_matrix/media/r0/thumbnail/:server_name/:media_id/:file_name",      emd_http_media_thumbnail, []},
               {"/_matrix/media/r0/preview_url",                                      emd_http_media_preview_url, []},
               {"/_matrix/media/r0/config",                                           emd_http_media_config, []},
               {"/_matrix/client/r0/sendToDevice/:event_type/:tx_id",                 emd_http_devices_send_to_device, []},
               {"/_matrix/client/r0/devices",                                         emd_http_devices_list, []},
               {"/_matrix/client/r0/devices/:device_id",                              emd_http_devices_rud, []},
               {"/_matrix/client/r0/delete_devices",                                  emd_http_devices_delete, []},
               {"/_matrix/client/r0/keys/upload",                                     emd_http_keys_upload, []},
               {"/_matrix/client/r0/keys/query",                                      emd_http_keys_query, []},
               {"/_matrix/client/r0/keys/claim",                                      emd_http_keys_claim, []},
               {"/_matrix/client/r0/keys/changes",                                    emd_http_keys_changes, []},
               {"/_matrix/client/r0/pushers",                                         emd_http_pushers_get, []},
               {"/_matrix/client/r0/pushers/set",                                     emd_http_pushers_set, []},
               {"/_matrix/client/r0/notifications",                                   emd_http_notifications_list, []},
               {"/_matrix/client/r0/pushrules/",                                      emd_http_pushrules_list, []},
               {"/_matrix/client/r0/pushrules/:scope/:kind/:rule_id",                 emd_http_pushrules_crud, []},
               {"/_matrix/client/r0/pushrules/:scope/:kind/:rule_id/enabled",         emd_http_pushrules_enable, []},
               {"/_matrix/client/r0/pushrules/:scope/:kind/:rule_id/actions",         emd_http_pushrules_actions, []},
               {"/_matrix/client/r0/search",                                          emd_http_search, []},
               {"/_matrix/client/r0/events",                                          emd_http_room_preview_events, []},
               {"/_matrix/client/r0/user/:user_id/rooms/:room_id/tags",               emd_http_user_room_tags_list, []},
               {"/_matrix/client/r0/user/:user_id/rooms/:room_id/tags/:tag",          emd_http_user_room_tags_crud, []},
               {"/_matrix/client/r0/user/:user_id/account_data/:type",                emd_http_user_account_data, []},
               {"/_matrix/client/r0/user/:user_id/rooms/:room_id/account_data/:type", emd_http_user_room_account_data, []},
               {"/_matrix/client/r0/admin/whois/:user_id",                            emd_http_admin_whois, []},
               {"/_matrix/client/r0/rooms/:room_id/context/:event_id",                emd_http_room_event_context, []},
               {"/_matrix/client/r0/login/sso/redirect",                              emd_http_login_sso_redirect, []},
               {"/_matrix/client/r0/rooms/:room_id/report/:event_id",                 emd_http_room_report, []},
               {"/_matrix/client/r0/thirdparty/protocols",                            emd_http_thirdparty_, []},
               {"/_matrix/client/r0/thirdparty/protocol/:protocol",                   emd_http_thirdparty_, []},
               {"/_matrix/client/r0/thirdparty/location/:protocol",                   emd_http_thirdparty_, []},
               {"/_matrix/client/r0/thirdparty/user/:protocol",                       emd_http_thirdparty_, []},
               {"/_matrix/client/r0/thirdparty/user",                                 emd_http_thirdparty_, []},
               {"/_matrix/client/r0/rooms/:room_id/upgrade",                          emd_http_room_upgrade, []}
              ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = case Ssl of
                  true ->
                      {ok, _} = cowboy:start_tls(http,
                                                 [{port, Port}],
                                                 #{env => #{dispatch => Dispatch},
                                                   middlewares => [cowboy_router,
                                                                   emd_cowboy_logger,
                                                                   emd_cowboy_cors,
                                                                   cowboy_handler]});
                  false ->
                      {ok, _} = cowboy:start_clear(http,
                                                   [{port, Port}],
                                                   #{env => #{dispatch => Dispatch},
                                                     middlewares => [cowboy_router,
                                                                     emd_cowboy_logger,
                                                                     emd_cowboy_cors,
                                                                     cowboy_handler]})
              end,
    ematrixd_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
