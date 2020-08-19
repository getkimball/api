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
                cb_test_storing_with_storage_lib,
                cc_test_store_with_no_new_data,
                da_test_count_with_tags,
                db_test_multiple_count_with_tags,
                dc_test_multiple_count_single_user_with_tags,
                dd_test_multiple_count_single_user_with_different_tags,
                de_test_with_multiple_tags_and_mismatched_ordering,
                ea_test_includes_key
              ]}
            ].

init_meck(Config) ->
    meck:new(features_count_router),
    meck:expect(features_count_router, register_counter, ['_', '_'], ok),

    meck:new(timer, [unstick]),
    meck:expect(timer, apply_interval, ['_', '_', '_', '_'], {ok, tref}),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', '_'], StoreLibState),
    meck:expect(features_store_lib, store, ['_', '_'], {#{}, {ok, StoreLibState}}),

    Name = <<"test">>,

    [{store_lib_state, StoreLibState},
     {name, Name}|Config].

init_per_testcase(ca_test_storage_lib_loading_data, Config) ->
    init_meck(Config);

init_per_testcase(_, Config) ->
    NewConfig = init_meck(Config),
    StoreLibState = ?config(store_lib_state, NewConfig),

    meck:expect(features_store_lib, get, ['_'], {#{}, StoreLibState}),
    Name = ?config(name, NewConfig),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB, Name),
    [{pid, Pid}|NewConfig].

end_per_testcase(_, _Config) ->
    ?assert(meck:validate(features_store_lib)),
    meck:unload(features_store_lib),

    ?assert(meck:validate(timer)),
    meck:unload(timer),

    ?assert(meck:validate(features_count_router)),
    meck:unload(features_count_router),
    ok.

aa_test_single_user(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(counts(#{count => 1,
                          single_tag_counts => #{},
                          tag_counts => #{[] => 1}}), Num),
    Config.

ab_test_single_user_multiple_times(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,

    ?MUT:add(User, Pid),
    ?MUT:add(User, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(counts(#{count => 1,
                          single_tag_counts => #{},
                          tag_counts => #{[] => 1}}), Num),
    Config.

ac_test_multiple_users(Config) ->
    Pid = ?config(pid, Config),

    User1 = <<"user_id_1">>,
    User2 = <<"user_id_2">>,

    ?MUT:add(User1, Pid),
    ?MUT:add(User2, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(counts(#{count => 2,
                          single_tag_counts => #{},
                          tag_counts => #{[] => 2}}), Num),
    Config.

ba_test_different_key_types(Config) ->
    Pid = ?config(pid, Config),

    KeyBin = <<"42">>,
    KeyInt = 42,

    ?MUT:add(KeyBin, Pid),
    ?MUT:add(KeyInt, Pid),

    Num = ?MUT:count(Pid),

    ?assertEqual(counts(#{count => 1,
                          single_tag_counts => #{},
                          tag_counts => #{[] => 1}}), Num),
    Config.

ca_test_storage_lib_loading_data(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Name = ?config(name, Config),

    BF = etbloom:sbf(1000000),
    Stored = #{bloom=>BF},
    meck:expect(features_store_lib, get, ['_'], {Stored, StoreLibState}),
    {ok, Pid} = ?MUT:start_link(?STORE_LIB, Name),
    meck:wait(features_store_lib, get, '_', 1000),

    Key = <<"42">>,

    ?MUT:add(Key, Pid),

    Name = ?config(name, Config),

    ?assertEqual(?STORE_LIB, meck:capture(first, features_store_lib, init, '_', 1)),
    ?assertEqual({"counter", Name}, meck:capture(first, features_store_lib, init, '_', 2)),

    ?assertEqual(StoreLibState, meck:capture(first, features_store_lib, get, '_', 1)),

    Num = ?MUT:count(Pid),

    ?assertEqual(counts(#{count => 1,
                          single_tag_counts => #{},
                          tag_counts => #{[] => 1}}), Num),
    [{pid, Pid}|Config].

cb_test_storing_with_storage_lib(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Pid = ?config(pid, Config),

    ?assertEqual(?MUT, meck:capture(first, timer, apply_interval, '_', 2)),
    ?assertEqual(persist, meck:capture(first, timer, apply_interval, '_', 3)),
    ?assertEqual([Pid], meck:capture(first, timer, apply_interval, '_', 4)),

    Key = <<"42">>,
    ?MUT:add(Key, Pid),
    ?MUT:persist(Pid),

    meck:wait(features_store_lib, store, ['_', '_'], 1000),
    ?assertEqual(StoreLibState, meck:capture(first, features_store_lib, store, '_', 2)),

    Config.

cc_test_store_with_no_new_data(Config) ->
    Pid = ?config(pid, Config),

    ?MUT:persist(Pid),
    ?assertError(not_found, meck:capture(first, features_store_lib, store, '_', 1)),

    Config.

da_test_count_with_tags(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,
    Tag = <<"foo">>,
    Tags = [Tag],

    ?MUT:add(User, Tags, Pid),

    Counts = ?MUT:count(Pid),

    ExpectedCounts = #{
        count => 1,
        tag_counts => #{Tags => 1},
        single_tag_counts => #{Tag => 1}
    },

    ?assertEqual(ExpectedCounts, Counts),
    Config.

db_test_multiple_count_with_tags(Config) ->
    Pid = ?config(pid, Config),

    User1 = <<"user_id1">>,
    User2 = <<"user_id2">>,
    Tag = <<"foo">>,
    Tags = [Tag],

    ?MUT:add(User1, Tags, Pid),
    ?MUT:add(User2, Tags, Pid),

    Counts = ?MUT:count(Pid),

    ExpectedCounts = counts(#{
        count => 2,
        tag_counts => #{Tags => 2},
        single_tag_counts => #{Tag => 2}
    }),

    ?assertEqual(ExpectedCounts, Counts),
    Config.

dc_test_multiple_count_single_user_with_tags(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,
    Tag = <<"foo">>,
    Tags = [Tag],

    ?MUT:add(User, Tags, Pid),
    ?MUT:add(User, Tags, Pid),

    Counts = ?MUT:count(Pid),

    ExpectedCounts = counts(#{
        count => 1,
        tag_counts => #{Tags => 1},
        single_tag_counts => #{Tag => 1}
    }),

    ?assertEqual(ExpectedCounts, Counts),
    Config.

dd_test_multiple_count_single_user_with_different_tags(Config) ->
    Pid = ?config(pid, Config),

    User = <<"user_id">>,
    Tag1 = <<"foo">>,
    Tag2 = <<"bar">>,
    Tags1 = [Tag1],
    Tags2 = [Tag2 | Tags1],

    ?MUT:add(User, Tags1, Pid),
    ?MUT:add(User, Tags2, Pid),

    Counts = ?MUT:count(Pid),

    ExpectedCounts = counts(#{
        count => 1,
        tag_counts => #{Tags1 => 1},
        single_tag_counts => #{Tag1 => 1}
    }),

    ?assertEqual(ExpectedCounts, Counts),
    Config.

de_test_with_multiple_tags_and_mismatched_ordering(Config) ->
    Pid = ?config(pid, Config),

    User1 = <<"user_id1">>,
    User2 = <<"user_id2">>,
    Tag1 = <<"foo">>,
    Tag2 = <<"bar">>,
    Tags = [Tag1, Tag2],
    ReversedTags = lists:reverse(Tags),
    SortedTags = lists:sort(Tags),

    ?MUT:add(User1, Tags, Pid),
    ?MUT:add(User2, ReversedTags, Pid),

    Counts = ?MUT:count(Pid),

    ExpectedCounts = counts(#{
        count => 2,
        tag_counts => #{SortedTags => 2},
        single_tag_counts => #{
            Tag1 => 2,
            Tag2 => 2
        }
    }),

    ?assertEqual(ExpectedCounts, Counts),
    Config.

ea_test_includes_key(Config) ->
    Pid = ?config(pid, Config),

    Key = <<"test_key">>,

    IsIncluded0 = ?MUT:includes_key(Key, Pid),
    ?MUT:add(Key, Pid),
    IsIncluded1 = ?MUT:includes_key(Key, Pid),

    ?assertEqual(false, IsIncluded0),
    ?assertEqual(true, IsIncluded1),
    Config.


counts(C) ->
    Default = #{count => 0,
                single_tag_counts => #{},
                tag_counts => #{}},
    maps:merge(Default, C).
