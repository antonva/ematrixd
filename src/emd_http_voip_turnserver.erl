%%%-------------------------------------------------------------------
%%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%% @doc
%%% The homeserver MAY provide a TURN server which clients can use to
%%% contact a remote party.
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


-module(emd_http_voip_turnserver).
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([handle_turnserver/2]).

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
%% 13.3.3.1 GET /_matrix/client/r0/voip/turnServer
%% This API provides credentials for the client to use when initiating
%% calls.
%% TODO: Implement properly, currently just propped up to get riot
%%       desktop to get further into the login process.
%%%-------------------------------------------------------------------
handle_turnserver(Req=#{method := <<"GET">>}, State) ->
    Body = <<"{\"username\": \"\",\"password\": \"\", \"uris\": [], \"ttl\": 86400}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State};
handle_turnserver(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

