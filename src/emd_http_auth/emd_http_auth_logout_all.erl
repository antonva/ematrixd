%%%-------------------------------------------------------------------
%%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%% @doc Logout all request handler for the Matrix Client-Server Spec.
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


-module(emd_http_auth_logout_all).
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([handle_logout_all/2]).

%% API
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"POST">>, <<"OPTIONS">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, handle_logout_all}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_logout_all}
     ], Req, State}.

%% Internal
handle_request(Req, State) ->
    Method = cowboy_req:method(Req),
    {Body, Req1, State1} = case Method of
                               <<"POST">> ->
                                   post_logout_all(Req, State);
                               _ ->
                                   wrong_method(Req, State)
                           end,
    {Body, Req1, State1}.

%%%-------------------------------------------------------------------
%% 5.4.4 POST /_matrix/client/r0/logout/all
%% Invalidates all access tokens for a user, so that they can no
%% longer be used for authorization. This includes the access token
%% that made this request. All devices for the user are also deleted.
%% Device keys for the device are deleted alongside the device.
%%
%% This endpoint does not require UI authorization because UI
%% authorization is designed to protect against attacks where the
%% someone gets hold of a single access token then takes over the
%% account.This endpoint invalidates all access tokens for the user,
%% including the token used in the request, and therefore the attacker
%% is unable to take over the account in this way.
%%%-------------------------------------------------------------------
post_logout_all(Req=#{method := <<"POST">>}, State) ->
    % Need to handle POST here and disallow GET
    Body = <<"{\"logout_all_implement\": \"me\"}">>,
    {Body, Req, State};
post_logout_all(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

