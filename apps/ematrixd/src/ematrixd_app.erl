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
%% @doc ematrix public API

-module(ematrixd_app).

-behaviour(application).
-export([start/2, stop/1, init_mnesia_db/0]).

-record(emd_users, {user_id, user_name, password}).
-record(emd_devices, {device_id, user_id, device_name, access_token}).


start(_StartType, _StartArgs) ->
    mnesia:start(),
    mnesia:wait_for_tables([emd_users, emd_devices], 20000),
    ematrixd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%% @doc Initialize mnesia, should only be ran once during setup.
init_mnesia_db() ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(emd_users, [{attributes, record_info(fields, emd_users)}]),
    mnesia:create_table(emd_devices, [{attributes, record_info(fields, emd_devices)}]),
    application:stop(mnesia).
