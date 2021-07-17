%%%-------------------------------------------------------------------
%%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%% @doc Logout request handler for the Matrix Client-Server Spec.
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


-module(emd_http_auth_logout).
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([
         post_logout/2
        ]).

%% API
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, post_logout}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_logout}
     ], Req, State}.

%%%-------------------------------------------------------------------
%% 5.4.3 POST /_matrix/client/r0/logout
%% Invalidates an existing access token, so that it can no longer be
%% used for authorization. The device associated with the access token
%% is also deleted. Device keys for the device are deleted alongside
%% the device.
%%%-------------------------------------------------------------------
post_logout(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State};
post_logout(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

