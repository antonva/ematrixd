%% This file is part of ematrixd.
%%
%% ematrixd is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or  (at your option) any later version.
%%
%% ematrixd is distributed in the hope that it will be usefu,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public
%% License along with ematrixd.
%% If not, see <https://www.gnu.org/licenses/>.
%%
%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%% @doc
%% Register for an account on this homeserver.
%%
%% 5.5.1 POST /_matrix/client/r0/register
%%
%% [https://matrix.org/docs/spec/client_server/latest#id204]
%%
%% This API endpoint uses the User-Interactive-Authentication API,
%% except in the cases where a guest account is being registered.
%%
%% @end

-module(emd_http_acct_register).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([handle_register/2]).

%% API
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"POST">>, <<"OPTIONS">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, handle_register}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_register}
     ], Req, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% 5.6.1 POST /_matrix/client/r0/register
%%% This API endpoint uses the User-Interactive Authentication API,
%%% except in the cases where a guest account is being registered.
%%% @end
%%%-------------------------------------------------------------------
handle_register(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"flows\": [{\"type\": \"m.login.password\"}]}">>,
    QueryParams = cowboy_req:parse_qs(Req),
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    Map = jiffy:decode(Data, [return_maps]),
    Kind = lists:keyfind(<<"kind">>, 1, QueryParams),

    % Decide to register or return login flows based on the existence
    % of the json body `auth` parameter.
    FindAuth = maps:find(<<"auth">>, Map),
    case parse_reg_request(FindAuth, Kind, Map) of
        {user, Params} ->
            foo;
        {guest, InitialDeviceDisplayName} ->
            % TODO: Not implemented yet
            % When registering a guest account, all parameters in the request body
            % with the exception of `initial_display_name` MUST BE ignored by the
            % server. The server MUST pick a `device_id` for the account regardless
            % of input.
            cowboy_req:reply(403, #{<<"content-type">> => <<"application/json">>}, [], Req),
            {stop, Req, State};
        {error} ->
            % User interactive thingamabob, send login flows
            % TODO: Figure out what this should look like / diff between reg/login
            {ok, AuthTypes} = emd_login:get_auth_types(),
            Types = [{[{type,X}]} || X <- AuthTypes],
            Flows = {[{flows,Types}]},
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Flows), Req),
            {stop, Req, State}
    end;
handle_register(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

%%%-------------------------------------------------------------------
%%% Parse the registration request, returning a unique atom based on
%%% the kind of registration request is being made.
%%%
%%% A request that has an `auth` parameter but the `kind` is neither
%%% `user` or `guest` is undefined and will be handled as if it had
%%% no `auth` parameter.
%%%-------------------------------------------------------------------
parse_reg_request({ok, Auth}, {ok,  <<"user">>}, Map) ->
    #{<<"username">> := Username,
      <<"password">> := Password,
      <<"device_id">> := DeviceId,
      <<"initial_device_display_name">> := InitialDeviceDisplayName,
      <<"inhibit_login">> := InhibitLogin
     } = Map,
    {user, {Auth, Username, Password, DeviceId, InitialDeviceDisplayName, InhibitLogin}};
parse_reg_request({ok, Auth}, {ok, <<"guest">>}, Map) ->
    #{<<"initial_device_display_name">> := InitialDeviceDisplayName} = Map,
    {guest, InitialDeviceDisplayName};
parse_reg_request(_, _, Map) ->
    {error}.
