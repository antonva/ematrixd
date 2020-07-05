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
%% Validate the email address that is being registered.
%%
%% 5.5.2 POST /_matrix/client/r0/register/email/requestToken
%%
%% [https://matrix.org/docs/spec/client_server/latest#id205]
%%
%% The homeserver must check that the given email address is not
%% already associated with an account on this homeserver.
%% The homeserver should validate the email itself, either by sending
%% a validation email itself or by using a service it has control over.
%% @end

-module(emd_http_acct_register_email_reqtoken).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-export([handle_email_reqtoken/2]).

%% API
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"POST">>, <<"GET">>, <<"OPTIONS">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, handle_email_reqtoken}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_email_reqtoken}
     ], Req, State}.

handle_email_reqtoken(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"email_reqtoken_implement\": \"me\"}">>,
    {Body, Req, State};
handle_email_reqtoken(Req, State) ->
    Body = <<"{\"error\": \"emd_method_not_allowed\"}">>,
    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.
