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
%% @doc HTTP handling of the versions supported API call

-module(emd_http_api_versions).
-compile(export_all).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, json_version}
     ], Req, State}.

json_version(Req, State) ->
    %% TODO: store the actual versions implemented somewhere else and fetch them
    Versions = [<<"r0.5.0">>, <<"r0.6.0">>],
    UnstableFeatures = [],
    Body = jiffy:encode({[{versions, Versions}, {unstable_features, UnstableFeatures}]}),
    {Body, Req, State}.
