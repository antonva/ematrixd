%%%-------------------------------------------------------------------
%%% @author  Anton Vilhelm Ásgeirsson <anton.v.asgeirsson@gmail.com>
%%% @copyright (C) 2020, Anton Vilhelm Ásgeirsson
%%% @doc Test cases for the Client Authentication part of the Matrix
%%%      Client-Server API.
%%% @end
%%%-------------------------------------------------------------------
-module(client_authentication_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(ematrixd),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(ranch),
    ok = application:stop(cowlib),
    ok = application:stop(cowboy),
    ok = application:stop(ematrixd),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [
     {login, [shuffle, sequence], [
                  get_matrix_client_r0_login,
                  post_matrix_client_r0_login,
                  get_matrix_client_r0_logout,
                  get_matrix_client_r0_logout_all
                 ]},
     {account_reg, [shuffle, sequence], [
                        post_matrix_client_r0_register,
                        post_matrix_client_r0_register_email_request_token,
                        post_matrix_client_r0_register_msisdn_request_token,
                        post_matrix_client_r0_password,
                        post_matrix_client_r0_password_email_request_token,
                        post_matrix_client_r0_password_msisdn_request_token,
                        post_matrix_client_r0_register_deactivate,
                        get_matrix_client_r0_register_available
                       ]},
     {admin_contact, [shuffle, sequence], [
                          get_matrix_client_r0_account_3pid,
                          post_matrix_client_r0_account_3pid_add,
                          post_matrix_client_r0_account_3pid_bind,
                          post_matrix_client_r0_account_3pid_delete,
                          post_matrix_client_r0_account_3pid_unbind,
                          post_matrix_client_r0_account_3pid_email_request_token,
                          post_matrix_client_r0_account_3pid_msisdn_request_token
                         ]},
     {whoami, [], [ get_matrix_client_r0_whoami ]}
    ].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, login}, {group, account_reg}, {group, admin_contact}, {group, whoami}].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
my_test_case(_Config) ->
    ok.

get_matrix_client_r0_login() ->
    [].
get_matrix_client_r0_login(_Config) ->
    {ok, Result} = httpc:request(get, {"http://localhost:8080/_matrix/client/r0/login", []}, [], []),
    {{_,200,_}, _Headers, Body} = Result,
    {[{<<"login_implement">>, <<"me">>}]} = jiffy:decode(Body),
    ok.

post_matrix_client_r0_login() ->
    [].
post_matrix_client_r0_login(_Config) ->
    {ok, Result} = httpc:request(post, {"http://localhost:8080/_matrix/client/r0/login", []}, [], []),
    {{_,200,_}, _Headers, Body} = Result,
    {[{<<"login_implement">>, <<"me">>}]} = jiffy:decode(Body),
    ok.

get_matrix_client_r0_logout() ->
    [].
get_matrix_client_r0_logout(_Config) ->
    {ok, Result} = httpc:request(post, {"http://localhost:8080/_matrix/client/r0/logout", [], "application/json", ""}, [], []),
    {{_,200,_}, _Headers, Body} = Result,
    {[{<<"logout_implement">>, <<"me">>}]} = jiffy:decode(Body),
    ok.

get_matrix_client_r0_logout_all() ->
    [].
get_matrix_client_r0_logout_all(_Config) ->
    {ok, Result} = httpc:request(post, {"http://localhost:8080/_matrix/client/r0/logout/all", [], "application/json",""}, [], []),
    {{_,200,_}, _Headers, Body} = Result,
    {[{<<"logout_all_implement">>, <<"me">>}]} = jiffy:decode(Body),
    ok.

post_matrix_client_r0_register() ->
    [].
post_matrix_client_r0_register(_Config) ->
    Auth = #{
             type => "m.login.password",
             session => "1"
            },
    Req = #{
            auth => Auth,
            username => "cheeky_monkey",
            password => "ilovebanananas",
            device_id => "GHTYAJCE",
            initial_device_display_name => "Jungle Phone",
            inhibit_login => false
           },
    {ok, Result} = httpc:request(post,
        {"http://localhost:8080/_matrix/client/r0/register?kind=user",
            "",
            "application/json",
            jiffy:encode(Req)
        }, [], []
    ),
    {{_, 200, _}, _Headers, _Body } = Result,
    ok.

post_matrix_client_r0_register_email_request_token() ->
    [].
post_matrix_client_r0_register_email_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_register_msisdn_request_token() ->
    [].
post_matrix_client_r0_register_msisdn_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_password() ->
    [].
post_matrix_client_r0_password(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_password_email_request_token() ->
    [].
post_matrix_client_r0_password_email_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_password_msisdn_request_token() ->
    [].
post_matrix_client_r0_password_msisdn_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_register_deactivate() ->
    [].
post_matrix_client_r0_register_deactivate(_Config) ->
    {skip, <<"Unimplemented">>}.

get_matrix_client_r0_register_available() ->
    [].
get_matrix_client_r0_register_available(_Config) ->
    {skip, <<"Unimplemented">>}.

get_matrix_client_r0_account_3pid() ->
    [].
get_matrix_client_r0_account_3pid(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_add() ->
    [].
post_matrix_client_r0_account_3pid_add(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_bind() ->
    [].
post_matrix_client_r0_account_3pid_bind(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_delete() ->
    [].
post_matrix_client_r0_account_3pid_delete(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_unbind() ->
    [].
post_matrix_client_r0_account_3pid_unbind(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_email_request_token() ->
    [].
post_matrix_client_r0_account_3pid_email_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

post_matrix_client_r0_account_3pid_msisdn_request_token() ->
    [].

post_matrix_client_r0_account_3pid_msisdn_request_token(_Config) ->
    {skip, <<"Unimplemented">>}.

get_matrix_client_r0_whoami() ->
    [].

get_matrix_client_r0_whoami(_Config) ->
    {skip, <<"Unimplemented">>}.
