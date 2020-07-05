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
%%
%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%% @doc Login request handler for the Matrix Client-Server Spec.

-module(emd_http_auth_login).
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([handle_login/2]).

%% API
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"OPTIONS">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, handle_login}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_login}
     ], Req, State}.

%%%-------------------------------------------------------------------
%% 5.4.1 GET /_matrix/client/r0/login
%% Gets the homeserver's supported login types to authenticate users.
%% Clients should pick one of these and supply it as the type when
%% logging in.
%%%-------------------------------------------------------------------
handle_login(Req=#{method := <<"GET">>}, State) ->
    {ok, AuthTypes} = emd_login:get_auth_types(),
    Types = [{[{type,X}]} || X <- AuthTypes],
    Flows = {[{flows,Types}]},
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Flows), Req),
    {stop, Req, State};

%%%-------------------------------------------------------------------
%% 5.4.2 POST /_matrix/client/r0/login
%% Authenticates the user, and issues an access token they can use to
%% authorize themself in subsequent requests.
%%
%% If the client does not supply a device_id, the server must
%% auto-generate one.
%%
%% The returned access token must be associated with the device_id
%% supplied by the client or generated by the server. The server may
%% invalidate any access token previously associated with that device.
%% See https://matrix.org/docs/spec/client_server/latest#relationship-between-access-tokens-and-devices
%%%-------------------------------------------------------------------
handle_login(Req0=#{method := <<"POST">>}, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    Map = jiffy:decode(Data, [return_maps]),
    #{<<"type">> := Type,
      <<"device_id">> := DeviceId,
      <<"initial_device_display_name">> := InitialDevName} = Map,
    case Type of
        <<"m.login.password">> ->
            #{<<"password">> := Pass, <<"identifier">> := Id} = Map,
            #{<<"type">> := IdType} = Id,
            case IdType of
                <<"m.id.user">> ->
                    #{<<"user">> := User} = Id,
                    io:format("~s, ~s, ~s, ~s", [User, Pass, IdType, Type]),
                    emd_login:login(password, user, User, Pass, DeviceId, InitialDevName),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req),
                    {stop, Req, State};
                <<"m.id.thirdparty">> ->
                    % TODO: Not implemented yet
                    cowboy_req:reply(403, #{<<"content-type">> => <<"application/json">>}, [], Req),
                    {stop, Req, State};
                <<"m.id.phone">> ->
                    % TODO: Not implemented yet
                    cowboy_req:reply(403, #{<<"content-type">> => <<"application/json">>}, [], Req),
                    {stop, Req, State};
                _ ->
                    Msg = erlang:iolist_to_binary([<<"Unknown identifier type: ">>, IdType]),
                    Body = #{<<"errcode">> => <<"M_UNKNOWN">>, <<"error">> => Msg},
                    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req),
                    {stop, Req, State}
            end;
       _  ->
            Msg = erlang:iolist_to_binary([<<"Unknown login type: ">>, Type]),
            Body = #{<<"errcode">> => <<"M_UNKNOWN">>, <<"error">> => Msg},
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req),
            {stop, Req, State}
    end;
handle_login(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

