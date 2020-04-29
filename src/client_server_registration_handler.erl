%%%-------------------------------------------------------------------
%% This file is part of ematrixd.
%%
%% ematrixd is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% ematrixd is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with ematrixd.  If not, see <https://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc Account registration and management methods of the Matrix
%% Client-Server specification
%% @end
%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%%-------------------------------------------------------------------

-module(client_server_registration_handler).
-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-record(state, {op}).

init(Req, Opts) ->
    [Op | _] = Opts,
    State = #state{op=Op},
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"OPTIONS">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, handle_registration_request}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, handle_registration_request}
     ], Req, State}.

handle_login_request(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
                               register ->
                                   handle_register(Req, State);
                               reg_request_email_token ->
                                   handle_reg_request_email_token(Req, State);
                               reg_request_msisdn_token ->
                                   handle_reg_request_msisdn_token(Req, State);
                               acct_password ->
                                   handle_password(Req, State);
                               acct_request_email_token ->
                                   handle_acct_request_email_token(Req, State);
                               acct_request_msisdn_token ->
                                   handle_acct_request_msisdn_token(Req, State);
                               acct_deactivate ->
                                   handle_deactivate(Req,State);
                               reg_available ->
                                   handle_available(Req,State)
                           end,
    {Body, Req1, State1}.

handle_register(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_reg_request_email_token(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_reg_request_msisdn_token(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_password(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_acct_request_email_token(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_acct_request_msisdn_token(Req=#{method := <<"POST">>}, State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_deactivate(Req=#{method := <<"POST">>},State) ->
    Body = <<"{\"logout_implement\": \"me\"}">>,
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    {stop, Req, State}.

handle_available(Req=#{method := <<"GET">>},State) ->
    ok.
