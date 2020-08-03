-module(features_count_router_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_count_router).
-define(COUNTER_MOD, features_counter).
-define(STORE_LIB, test_store_lib).

all() -> [{group, test_count}].

groups() -> [{test_count, [
                aa_test_new_counter,
                ab_test_existing_counter,
                ac_test_counter_registration_race,
                ba_test_counter_counts,
                ca_test_start_with_existing_counters,
                cb_test_counter_registration_persists,
                da_test_new_goal,
                db_test_existing_goal
              ]}
            ].

init_meck(Config) ->
    meck:new(?COUNTER_MOD),
    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, [features_store_lib_s3, "count_router"], StoreLibState),
    meck:expect(features_store_lib, get, ['_'], {#{}, StoreLibState}),
    meck:expect(features_store_lib, store, ['_', '_'], {ok, StoreLibState}),

    meck:new(supervisor, [unstick]),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, self()}),
    [{store_lib_state, StoreLibState}|Config].

init_per_testcase(ca_test_start_with_existing_counters, Config) ->
    init_meck(Config);
init_per_testcase(db_test_existing_goal, Config) ->
    init_meck(Config);
init_per_testcase(_, Config) ->
    Config1 = init_meck(Config),
    {ok, Pid} = ?MUT:start_link(),
    [{pid, Pid}|Config1].

end_per_testcase(_, Config) ->
    ?assert(meck:validate(?COUNTER_MOD)),
    meck:unload(?COUNTER_MOD),

    ?assert(meck:validate(features_store_lib)),
    meck:unload(features_store_lib),

    ?assert(meck:validate(supervisor)),
    meck:unload(supervisor),

    Config.

aa_test_new_counter(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    StoreMod = features_store_lib_s3,
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [StoreMod, Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    Config.

ab_test_existing_counter(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    StoreMod = features_store_lib_s3,
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),
    ?MUT:register_counter(Feature, Pid),
    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [StoreMod, Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertError(not_found, meck:capture(2, supervisor, start_child, ['_', Spec], 2)),

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    ?assertEqual(User, meck:capture(2, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(2, ?COUNTER_MOD, add, '_', 2)),

    Config.

ac_test_counter_registration_race(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    SupResp = {error, {already_started, Pid}},
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], SupResp),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),
    Config.

ba_test_counter_counts(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    Count = 1,

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(Feature, Pid),


    Counts = ?MUT:counts(),

    ?assertEqual([#{name => Feature, count => Count}], Counts),
    Config.


ca_test_start_with_existing_counters(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Feature = <<"feature_name">>,
    Count = 1,
    Spec = spec_for_feature(Feature),
    StoredData = #{counters => [Feature]},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),
    meck:expect(features_counter, count, ['_'], Count),

    {ok, Pid} = ?MUT:start_link(),
    Config1 = [{pid, Pid}|Config],

    meck:wait(features_store_lib, get, '_', 1000),
    timer:sleep(500),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),

    Config1.

cb_test_counter_registration_persists(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    Count = 1,

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(Feature, Pid),
    meck:wait(features_store_lib, store, '_', 1000),


    ExpectedData = expected_stored_data(#{counters=>[Feature]}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    Counts = ?MUT:counts(),

    ?assertEqual([#{name => Feature, count => Count}], Counts),

    Config.

spec_for_feature(Feature) ->
    #{id => {features_counter, Feature},
      start => {features_counter, start_link, [features_store_lib_s3, Feature]}}.


da_test_new_goal(Config) ->
    Goal = <<"goal_name">>,

    ?MUT:add_goal(Goal),
    Goals = ?MUT:goals(),

    ExpectedGoals = [Goal],

    ExpectedData = expected_stored_data(#{goals=>ExpectedGoals}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    ?assertEqual(ExpectedGoals, Goals),

    Config.

db_test_existing_goal(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Goal = <<"feature_name">>,
    StoredData = #{goals => [Goal]},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(),
    Config1 = [{pid, Pid}|Config],

    meck:wait(features_store_lib, get, '_', 1000),
    Goals = ?MUT:goals(),

    ExpectedGoals = [Goal],

    ?assertEqual(ExpectedGoals, Goals),

    % Ensure that this doesn't write to the store again when adding the goal again
    ?MUT:add_goal(Goal),
    ?assertError(not_found, meck:capture(first, features_store_lib, store, '_', 1)),

    Config1.


expected_stored_data(Data) ->
    #{
        counters => maps:get(counters, Data, []),
        goals => maps:get(goals, Data, [])
    }.
