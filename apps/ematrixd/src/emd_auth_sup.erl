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
%% @doc The ematrixd authentication server supervisor.
%% TODO: decide on splitting out different processes into different
%% sub supervisors

-module(emd_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Session = #{id => 'session',
               start => {'emd_auth_session', start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => ['emd_auth_session']},

    {ok, {SupFlags, [Session]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
