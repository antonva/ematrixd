%%%-------------------------------------------------------------------
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
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
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
%%%-------------------------------------------------------------------

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
%%% 5.6.1 POST /_matrix/client/r0/register
%%% This API endpoint uses the User-Interactive Authentication API,
%%% except in the cases where a guest account is being registered.
%%%
%%% TODO: As with login, figure out a better way to represent
%%%       implemented login/reg flows.
%%%-------------------------------------------------------------------
handle_register(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"flows\": [{\"type\": \"m.login.password\"}]}">>,
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    case jiffy:decode(Data) of
        {[{Auth, Username, Password, DeviceId, InitialDeviceDisplayName, InhibitLogin}]} ->
            io:write(Username),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
            {stop, Req, State};
        {[{_,InitialDeviceDisplayName}, {_,Auth}]} ->
            io:format("~s~n", [InitialDeviceDisplayName]),
            cowboy_req:reply(401, #{<<"content-type">> => <<"application/json">>}, Body, Req),
            {stop, Req, State}
    end;
handle_register(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.
