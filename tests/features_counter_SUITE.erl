-module(features_counter_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_counter).
-define(STORE_LIB, fake_store_lib).


all() -> [{group, test_count}].

groups() -> [{test_count, [
                aa_test_single_user,
                ab_test_single_user_multiple_times,
                ac_test_multiple_users,
                ba_test_different_key_types,
                ca_test_storage_lib_loading_data,
                cb_test_storing_with_storage_lib
              ]}
            ].

init_per_testcase(ca_test_storage_lib_loading_data, Config) ->
    meck:new(features_count_router),
    meck:expect(features_count_router, register_counter, ['_', '_'], ok),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', '_'], StoreLibState),
    meck:expect(features_store_lib, store, ['_', '_'], {#{}, {ok, StoreLibState}}),

    Name = <<"test">>,

    [{store_lib_state, StoreLibState},
     {name, Name}|Config];
init_per_testcase(_, Config) ->
    meck:new(features_count_router),
    meck:expect(features_count_router, register_counter, ['_', '_'], ok),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', '_'], StoreLibState),
    meck:expect(features_store_lib, get, ['_'], {#{}, StoreLibState}),
    meck:expect(features_store_lib, store, ['_', '_'], {#{}, {ok, StoreLibState}}),

    Name = <<"test">>,

    {ok, Pid} = ?MUT:start_link(?STORE_LIB, Name),
    [{pid, Pid},
     {store_lib_state, StoreLibState},
     {name, Name}|Config].

end_per_testcase(_, Config) ->
    ?assert(meck:validate(features_store_lib)),
    meck:unload(features_store_lib),

    ?assert(meck:validate(features_count_router)),
    meck:unload(features_count_router),

    Pid = ?config(pid, Config),
    ok = gen_server:stop(Pid).

aa_test_single_user(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num),
    Config.

ab_test_single_user_multiple_times(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),
    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num),
    Config.

ac_test_multiple_users(Config) ->
    Pid = ?config(pid, Config),

    User1 = <<"user_id_1">>,
    User2 = <<"user_id_2">>,

    ?MUT:add(User1, Pid),
    ?MUT:add(User2, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(2, Num),
    Config.

ba_test_different_key_types(Config) ->
    Pid = ?config(pid, Config),

    KeyBin = <<"42">>,
    KeyInt = 42,

    ?MUT:add(KeyBin, Pid),
    ?MUT:add(KeyInt, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num),
    Config.

ca_test_storage_lib_loading_data(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Name = ?config(name, Config),

    BF = etbloom:sbf(1000000),
    Stored = #{bloom=>BF},
    meck:expect(features_store_lib, get, ['_'], {Stored, StoreLibState}),
    {ok, Pid} = ?MUT:start_link(?STORE_LIB, Name),

    Key = <<"42">>,

    ?MUT:add(Key, Pid),

    Name = ?config(name, Config),

    ?assertEqual(?STORE_LIB, meck:capture(first, features_store_lib, init, '_', 1)),
    ?assertEqual(Name, meck:capture(first, features_store_lib, init, '_', 2)),

    ?assertEqual(StoreLibState, meck:capture(first, features_store_lib, get, '_', 1)),

    Num = ?MUT:count(Pid),

    ?assertEqual(1, Num),
    [{pid, Pid}|Config].

cb_test_storing_with_storage_lib(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Pid = ?config(pid, Config),

    Key = <<"42">>,
    ?MUT:add(Key, Pid),

    meck:wait(features_store_lib, store, ['_', '_'], 1000),
    ?assertEqual(StoreLibState, meck:capture(first, features_store_lib, store, '_', 2)),

    Config.
