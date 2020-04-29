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
%% @doc ematrixd public API
%% @end
%%%-------------------------------------------------------------------

-module(ematrixd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [
              {'_', [
                     {"/_matrix/client/versions", client_server_version_handler, []},
                     {"/_matrix/client/r0/login", login, []},
                     {"/_matrix/client/r0/logout", client_server_login_handler, [logout]},
                     {"/_matrix/client/r0/logout/all", client_server_login_handler, [logout_all]}
                    ]}
             ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
                                                         env => #{dispatch => Dispatch},
                                                         middlewares => [cowboy_router, emd_cowboy_cors, cowboy_handler]
                                                        }),
    ematrixd_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
