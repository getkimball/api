-module(features_counter_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_counter).


all() -> [{group, test_count}].

groups() -> [{test_count, [
                aa_test_single_user,
                ab_test_single_user_multiple_times,
                ac_test_multiple_users,
                ba_test_different_key_types
              ]}
            ].

init_per_testcase(_, Config) ->
    meck:new(features_count_router),
    meck:expect(features_count_router, register_counter, ['_', '_'], ok),
    {ok, Pid} = ?MUT:start_link(<<"test">>),
    [{pid, Pid}|Config].

end_per_testcase(_, Config) ->
    Pid = ?config(pid, Config),
    ok = gen_server:stop(Pid),
    ?assert(meck:validate(features_count_router)),
    meck:unload(features_count_router).

aa_test_single_user(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num).

ab_test_single_user_multiple_times(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),
    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num).

ac_test_multiple_users(Config) ->
    Pid = ?config(pid, Config),

    User1 = <<"user_id_1">>,
    User2 = <<"user_id_2">>,

    ?MUT:add(User1, Pid),
    ?MUT:add(User2, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(2, Num).

ba_test_different_key_types(Config) ->
    Pid = ?config(pid, Config),

    KeyBin = <<"42">>,
    KeyInt = 42,

    ?MUT:add(KeyBin, Pid),
    ?MUT:add(KeyInt, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num).
