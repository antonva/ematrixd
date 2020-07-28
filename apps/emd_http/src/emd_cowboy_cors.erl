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
%% @doc ematrixd cowboy cors middleware.
%%
%% Ensures that all requests have the appropriate CORS headers as
%% defined in the spec.
%%
%% This is coincidentally also the place where the server header is
%% updated. TODO: move that somewhere else?
%% @end

-module(emd_cowboy_cors).

-behaviour(cowboy_middleware).

-export([execute/2]).


execute(Req0, Env) ->
    Req = cowboy_req:set_resp_headers(
             #{
               <<"server">> => <<"ematrixd">>,
               <<"access-control-allow-origin">>  => <<"*">>,
               <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
               <<"access-control-allow-headers">> => <<"Origin, X-Requested-With, Content-Type, Accept, Authorization">>
              }, Req0),

    Method = cowboy_req:method(Req),
    case Method of
        <<"OPTIONS">> ->
            ReqFinal = cowboy_req:reply(200, Req),
            {stop, ReqFinal};
        _ ->
            {ok, Req, Env}
    end.
