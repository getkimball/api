-module(features_count_router_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MUT, features_count_router).
-define(COUNTER_MOD, features_counter).
-define(STORE_LIB, test_store_lib).

all() -> [{group, test_count}].

groups() ->
    [
        {test_count, [
            aa_test_new_counter,
            ab_test_existing_counter,
            ac_test_new_counter_as_goal,
            ad_test_existing_counter_as_goal,
            ae_test_new_counter_as_explictely_not_ensuring_goal,
            af_test_existing_counter_as_existing_goal,
            ag_test_counter_registration_race,
            ah_test_multiple_counts_added_at_once,
            ai_test_existing_counter_in_different_namespace,
            ba_test_counter_counts,
            bb_test_counter_pids,
            bc_test_counter_count_map,
            bd_test_namespaced_counter_count_map,
            be_test_namespaced_counter_counts,
            ca_test_start_with_existing_counters,
            cb_test_counter_registration_persists,
            cc_test_weekly_cohort_counter_created,
            cd_test_weekly_cohort_counter_created_and_added_again,
            da_test_new_goal,
            db_test_existing_goal,
            dc_test_goals_are_namespaced,
            ea_test_triggering_a_goal,
            eb_test_triggering_a_goal_registered_after_goal_added,
            ec_test_triggering_a_goal_from_another_namespace,
            fa_test_event_no_persistence,
            ga_test_with_value,
            ha_test_namespaces,
            ia_stop_counter,
            ib_stop_unregistered_counter,
            ic_stop_counter_not_found
        ]}
    ].

init_meck(Config) ->
    test_utils:meck_load_prometheus(),
    meck:new(?COUNTER_MOD),
    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, count, ['_'], counts(#{count => -1})),
    meck:expect(?COUNTER_MOD, includes_key, ['_', '_'], false),

    meck:new(features_counter_config),
    meck:expect(features_counter_config, config_for_counter, ['_', init], undefined),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', <<"count_router">>], StoreLibState),
    meck:expect(features_store_lib, get, ['_'], {#{}, StoreLibState}),
    meck:expect(features_store_lib, store, ['_', '_'], {ok, StoreLibState}),

    meck:new(supervisor, [unstick]),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, self()}),
    meck:expect(supervisor, terminate_child, [features_counter_sup, '_'], ok),
    meck:expect(supervisor, delete_child, [features_counter_sup, '_'], ok),
    [{store_lib_state, StoreLibState} | Config].

init_per_testcase(ca_test_start_with_existing_counters, Config) ->
    init_meck(Config);
init_per_testcase(cc_test_weekly_cohort_counter_created, Config) ->
    init_meck(Config);
init_per_testcase(cd_test_weekly_cohort_counter_created_and_added_again, Config) ->
    init_meck(Config);
init_per_testcase(db_test_existing_goal, Config) ->
    init_meck(Config);
init_per_testcase(fa_test_event_no_persistence, Config) ->
    init_meck(Config);
init_per_testcase(_, Config) ->
    Config1 = init_meck(Config),
    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    [{pid, Pid} | Config1].

end_per_testcase(_, Config) ->
    test_utils:meck_unload_prometheus(),
    ?assert(meck:validate(?COUNTER_MOD)),
    meck:unload(?COUNTER_MOD),

    ?assert(meck:validate(features_counter_config)),
    meck:unload(features_counter_config),

    ?assert(meck:validate(features_store_lib)),
    meck:unload(features_store_lib),

    ?assert(meck:validate(supervisor)),
    meck:unload(supervisor),

    Config.

aa_test_new_counter(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, '_', Pid])),

    Config.

ab_test_existing_counter(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),
    ?MUT:register_counter(CounterID, Pid),

    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),

    ?MUT:add(Feature, User),

    Spec = #{
        id => {features_counter, CounterID},
        start => {features_counter, start_link, [?STORE_LIB, CounterID]}
    },

    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec])),
    ?assertEqual(4, meck:num_calls(?COUNTER_MOD, add, [User, '_', Pid])),

    Config.

ac_test_new_counter_as_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(<<"default">>, Feature, User, #{ensure_goal => true}),

    Spec = spec_for_feature(Feature),

    ExpectedOtherCounters = [],

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, '_', Pid])),

    Config.

ad_test_existing_counter_as_goal(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(<<"default">>, Feature, User, #{ensure_goal => true}),
    ?MUT:register_counter(CounterID, Pid),

    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),

    io:format("Calls ~p~n", [meck:history(?COUNTER_MOD)]),
    ?MUT:add(Feature, User),

    Spec = #{
        id => {features_counter, CounterID},
        start => {features_counter, start_link, [?STORE_LIB, CounterID]}
    },
    ExpectedOtherCounters = [],

    io:format("Calls ~p~n", [meck:history(?COUNTER_MOD)]),
    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec])),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, '_', Pid])),

    Config.

ae_test_new_counter_as_explictely_not_ensuring_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(<<"default">>, Feature, User, #{ensure_goal => false}),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, '_', Pid])),

    Config.

af_test_existing_counter_as_existing_goal(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add_goal(<<"default">>, Feature),
    ?MUT:register_counter(CounterID, Pid),
    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),
    ?MUT:add(<<"default">>, Feature, User, #{ensure_goal => true}),
    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),

    ExpectedOtherCounters = [],

    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, '_', Pid])),

    Config.

ag_test_counter_registration_race(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    SupResp = {error, {already_started, Pid}},
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], SupResp),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),

    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, '_', Pid])),
    Config.

ah_test_multiple_counts_added_at_once(Config) ->
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    Adds = [
        {<<"default">>, <<"event_1">>, <<"user_1">>, #{}},
        {<<"default">>, <<"event_2">>, <<"user_2">>, #{ensure_goal => true}}
    ],

    ?MUT:add(Adds),

    io:format("Calls ~p~n", [meck:history(?COUNTER_MOD)]),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [<<"user_1">>, '_', Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, '_', Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, [], '_', Pid])),

    Config.

ai_test_existing_counter_in_different_namespace(Config) ->
    Feature = <<"feature_name">>,
    CounterID1 = features_counter_id:create(Feature),
    CounterID2 = features_counter_id:create(<<"not default">>, Feature, named),
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(<<"default">>, Feature, User, #{}),
    ?MUT:register_counter(CounterID1, Pid),

    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),

    ?MUT:add(<<"not default">>, Feature, User, #{}),

    Spec1 = #{
        id => {features_counter, CounterID1},
        start => {features_counter, start_link, [?STORE_LIB, CounterID1]}
    },
    Spec2 = #{
        id => {features_counter, CounterID2},
        start => {features_counter, start_link, [?STORE_LIB, CounterID2]}
    },

    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec1])),
    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec2])),
    ?assertEqual(4, meck:num_calls(?COUNTER_MOD, add, [User, '_', Pid])),

    Config.

ba_test_counter_counts(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    Num = 1,
    Count = counts(#{count => Num}),

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(CounterID, Pid),

    % Run to synchronize/handle all messages
    ?MUT:goals(<<"default">>),

    Counts = ?MUT:counts(<<"default">>),

    ?assertEqual([counts(#{id => CounterID, count => Num})], Counts),
    Config.

bb_test_counter_pids(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),

    ?MUT:register_counter(CounterID, Pid),

    % Run to synchronize/handle all messages
    ?MUT:goals(<<"default">>),

    Pids = ?MUT:counter_pids(),

    ?assertEqual([Pid], Pids),
    Config.

bc_test_counter_count_map(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    Num = 1,
    Count = counts(#{count => Num}),

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(CounterID, Pid),

    % Run to synchronize/handle all messages
    ?MUT:goals(<<"default">>),

    Counts = ?MUT:count_map(<<"default">>),

    ?assertEqual(#{CounterID => Count}, Counts),
    Config.

bd_test_namespaced_counter_count_map(Config) ->
    Feature = <<"feature_name">>,
    CounterID1 = features_counter_id:create(Feature),
    CounterID2 = features_counter_id:create(<<"not default">>, Feature, named),
    Pid1 = erlang:list_to_pid("<0.0.0>"),
    Pid2 = erlang:list_to_pid("<0.0.1>"),
    Count1 = counts(#{count => 1}),
    Count2 = counts(#{count => 2}),

    meck:expect(features_counter, count, [
        {[Pid1], Count1},
        {[Pid2], Count2}
    ]),

    ?MUT:register_counter(CounterID1, Pid1),
    ?MUT:register_counter(CounterID2, Pid2),

    % Run to synchronize/handle all messages
    ?MUT:goals(<<"default">>),

    Counts1 = ?MUT:count_map(<<"default">>),
    Counts2 = ?MUT:count_map(<<"not default">>),

    ?assertEqual(#{CounterID1 => Count1}, Counts1),
    ?assertEqual(#{CounterID2 => Count2}, Counts2),
    Config.

be_test_namespaced_counter_counts(Config) ->
    Feature = <<"feature_name">>,
    CounterID1 = features_counter_id:create(Feature),
    CounterID2 = features_counter_id:create(<<"not default">>, Feature, named),
    Pid1 = erlang:list_to_pid("<0.0.0>"),
    Pid2 = erlang:list_to_pid("<0.0.1>"),
    Count1 = counts(#{count => 1}),
    Count2 = counts(#{count => 2}),

    meck:expect(features_counter, count, [
        {[Pid1], Count1},
        {[Pid2], Count2}
    ]),

    ?MUT:register_counter(CounterID1, Pid1),
    ?MUT:register_counter(CounterID2, Pid2),

    % Run to synchronize/handle all messages
    ?MUT:goals(<<"default">>),

    Counts1 = ?MUT:counts(<<"default">>),
    Counts2 = ?MUT:counts(<<"not default">>),

    ?assertEqual([counts(#{id => CounterID1, count => 1})], Counts1),
    ?assertEqual([counts(#{id => CounterID2, count => 2})], Counts2),
    Config.

ca_test_start_with_existing_counters(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Num = 1,
    Count = #{count => Num},
    Spec = spec_for_feature(CounterID),
    StoredData = #{counters => [CounterID]},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),
    meck:expect(features_counter, count, ['_'], Count),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid} | Config],

    meck:wait(features_store_lib, get, '_', 1000),
    timer:sleep(500),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),

    Config1.

cb_test_counter_registration_persists(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),
    Num = 1,
    Count = counts(#{count => Num}),

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(CounterID, Pid),
    meck:wait(features_store_lib, store, '_', 1000),

    ExpectedData = expected_stored_data(#{counters => [CounterID]}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    Counts = ?MUT:counts(<<"default">>),

    ?assertEqual([counts(#{id => CounterID, count => Num})], Counts),

    Config.

cc_test_weekly_cohort_counter_created(Config) ->
    CounterConfig = #{date_cohort => weekly},
    meck:expect(features_counter_config, config_for_counter, ['_', init], CounterConfig),
    StoreLibState = ?config(store_lib_state, Config),
    {Year, Week} = calendar:iso_week_number(),
    Name = <<"cc_feature">>,
    WeeklyCounterID = features_counter_id:create(<<"default">>, Name, weekly, {Year, Week}),
    Num = 1,
    Count = #{count => Num},

    Spec = spec_for_feature(Name),
    WeeklySpec = spec_for_feature(WeeklyCounterID),
    StoredData = #{},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),
    meck:expect(features_counter, count, ['_'], Count),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid} | Config],

    ?MUT:add(Name, <<"user_id">>),

    meck:wait(supervisor, start_child, [features_counter_sup, WeeklySpec], 1000),

    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec])),
    ?assertEqual(1, meck:num_calls(supervisor, start_child, [features_counter_sup, WeeklySpec])),

    Config1.

cd_test_weekly_cohort_counter_created_and_added_again(Config) ->
    CounterConfig = #{date_cohort => weekly},
    meck:expect(features_counter_config, config_for_counter, ['_', init], CounterConfig),
    StoreLibState = ?config(store_lib_state, Config),
    {Year, Week} = calendar:iso_week_number(),
    Name = <<"cc_feature">>,
    WeeklyCounterID = features_counter_id:create(<<"default">>, Name, weekly, {Year, Week}),
    Num = 1,
    Count = #{count => Num},

    Spec = spec_for_feature(Name),
    WeeklySpec = spec_for_feature(WeeklyCounterID),
    StoredData = #{},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),
    meck:expect(features_counter, count, ['_'], Count),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid} | Config],

    ?MUT:add(Name, <<"user_id">>),
    ?MUT:add(Name, <<"user_id">>),

    meck:wait(supervisor, start_child, [features_counter_sup, WeeklySpec], 1000),

    ?assertEqual(2, meck:num_calls(supervisor, start_child, [features_counter_sup, Spec])),
    ?assertEqual(2, meck:num_calls(supervisor, start_child, [features_counter_sup, WeeklySpec])),

    Config1.

da_test_new_goal(Config) ->
    Goal = <<"goal_name">>,

    ?MUT:add_goal(<<"default">>, Goal),
    Goals = ?MUT:goals(<<"default">>),

    ExpectedStoredGoals = #{<<"default">> => #{Goal => undefined}},
    ExpectedGoals = [Goal],

    ExpectedData = expected_stored_data(#{goals => ExpectedStoredGoals}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    ?assertEqual(ExpectedGoals, Goals),

    Config.

db_test_existing_goal(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Goal = <<"feature_name">>,
    StoredData = #{goals => #{<<"default">> => #{Goal => undefined}}},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid} | Config],

    meck:wait(features_store_lib, get, '_', 1000),
    Goals = ?MUT:goals(<<"default">>),

    ExpectedGoals = [Goal],

    ?assertEqual(ExpectedGoals, Goals),

    % Ensure that this doesn't write to the store again when adding the goal again
    ?MUT:add_goal(<<"default">>, Goal),
    ?assertError(not_found, meck:capture(first, features_store_lib, store, '_', 1)),

    Config1.

dc_test_goals_are_namespaced(Config) ->
    Goal1 = <<"goal1">>,
    Goal2 = <<"goal2">>,

    ?MUT:add_goal(<<"default">>, Goal1),
    ?MUT:add_goal(Goal2, Goal2),
    Goals1 = ?MUT:goals(<<"default">>),
    Goals2 = ?MUT:goals(Goal2),

    ExpectedStoredGoals = #{
        <<"default">> => #{Goal1 => undefined},
        Goal2 => #{Goal2 => undefined}
    },

    ExpectedData = expected_stored_data(#{goals => ExpectedStoredGoals}),
    ?assertEqual(ExpectedData, meck:capture(last, features_store_lib, store, '_', 1)),

    ?assertEqual([Goal1], Goals1),
    ?assertEqual([Goal2], Goals2),

    Config.

ea_test_triggering_a_goal(Config) ->
    CounterConfig = #{date_cohort => weekly},
    User = <<"user_id">>,

    NonGoalFeature = <<"non_goal">>,
    GoalFeature = <<"goal">>,
    {Year, Week} = calendar:iso_week_number(),

    meck:expect(features_counter_config, config_for_counter, [
        {[GoalFeature, init], CounterConfig},
        {['_', init], undefined}
    ]),

    GlobalCounterPid = erlang:list_to_pid("<0.0.0>"),
    NonGoalCounterPid = erlang:list_to_pid("<0.0.1>"),
    GoalCounterPid = erlang:list_to_pid("<0.0.2>"),
    WeeklyGoalCounterPid = erlang:list_to_pid("<0.0.3>"),

    GlobalCounterID = features_counter_id:global_counter_id(<<"default">>),
    NonGoalCounterID = features_counter_id:create(NonGoalFeature),
    GoalCounterID = features_counter_id:create(GoalFeature),
    WeeklyGoalCounterID = features_counter_id:create(
        <<"default">>,
        GoalFeature,
        weekly,
        {Year, Week}
    ),

    meck:expect(
        supervisor,
        start_child,
        [features_counter_sup, '_'],
        meck:raise(error, should_not_hit_this)
    ),

    ?MUT:register_counter(GlobalCounterID, GlobalCounterPid),
    ?MUT:register_counter(NonGoalCounterID, NonGoalCounterPid),
    ?MUT:register_counter(GoalCounterID, GoalCounterPid),
    ?MUT:register_counter(WeeklyGoalCounterID, WeeklyGoalCounterPid),

    ?MUT:add_goal(<<"default">>, GoalFeature),

    meck:expect(?COUNTER_MOD, add, ['_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, includes_key, [
        {['_', NonGoalCounterPid], true},
        {['_', GlobalCounterPid], true},
        {['_', GoalCounterPid], false},
        {['_', WeeklyGoalCounterPid], false}
    ]),

    meck:expect(?COUNTER_MOD, count, [
        {[NonGoalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GlobalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GoalCounterPid], #{count => 1, tag_counts => #{[NonGoalFeature] => 1}}},
        {[WeeklyGoalCounterPid], #{count => 1, tag_counts => #{[NonGoalFeature] => 1}}}
    ]),
    % synchronize call
    _Goals = ?MUT:goals(<<"default">>),

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    Counts = ?MUT:counts(<<"default">>),

    io:format("Adds ~p~n", [meck:history(?COUNTER_MOD)]),
    ExpectedEvents = lists:sort([NonGoalFeature]),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', GoalCounterPid], 1)),
    ?assertEqual(
        ExpectedEvents,
        lists:sort(meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', GoalCounterPid], 2))
    ),

    ?assertEqual(
        User,
        meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', WeeklyGoalCounterPid], 1)
    ),
    ?assertEqual(
        ExpectedEvents,
        lists:sort(meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', WeeklyGoalCounterPid], 2))
    ),

    ExpectedCounts = [
        #{count => 1, id => NonGoalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GlobalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GoalCounterID, tag_counts => #{[NonGoalFeature] => 1}},
        #{count => 1, id => WeeklyGoalCounterID, tag_counts => #{[NonGoalFeature] => 1}}
    ],

    ?assertEqual(
        lists:sort(ExpectedCounts),
        lists:sort(Counts)
    ),

    Config.

eb_test_triggering_a_goal_registered_after_goal_added(Config) ->
    User = <<"user_id">>,

    NonGoalFeature = <<"non_goal">>,
    GoalFeature = <<"goal">>,

    GlobalCounterPid = erlang:list_to_pid("<0.0.0>"),
    NonGoalCounterPid = erlang:list_to_pid("<0.0.1>"),
    GoalCounterPid = erlang:list_to_pid("<0.0.2>"),

    GlobalCounterID = features_counter_id:global_counter_id(<<"default">>),
    NonGoalCounterID = features_counter_id:create(NonGoalFeature),
    GoalCounterID = features_counter_id:create(GoalFeature),

    meck:expect(
        supervisor,
        start_child,
        [features_counter_sup, '_'],
        meck:raise(error, should_not_hit_this)
    ),

    ?MUT:register_counter(GlobalCounterID, GlobalCounterPid),
    ?MUT:register_counter(NonGoalCounterID, NonGoalCounterPid),

    ?MUT:add_goal(<<"default">>, GoalFeature),

    % synchronize call
    _Goals0 = ?MUT:goals(<<"default">>),

    ?MUT:register_counter(GoalCounterID, GoalCounterPid),

    meck:expect(?COUNTER_MOD, add, ['_', '_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_', GoalCounterPid], ok),
    meck:expect(?COUNTER_MOD, includes_key, [
        {['_', NonGoalCounterPid], true},
        {['_', GlobalCounterPid], true},
        {['_', GoalCounterPid], false}
    ]),

    meck:expect(?COUNTER_MOD, count, [
        {[NonGoalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GlobalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GoalCounterPid], #{count => 1, tag_counts => #{[NonGoalFeature] => 1}}}
    ]),
    % synchronize call
    _Goals1 = ?MUT:goals(<<"default">>),

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    % synchronize call
    _Goals2 = ?MUT:goals(<<"default">>),

    Counts = ?MUT:counts(<<"default">>),

    ExpectedEvents = lists:sort([NonGoalFeature]),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 1)),
    ?assertEqual(
        ExpectedEvents,
        lists:sort(meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', '_'], 2))
    ),
    ?assertEqual(GoalCounterPid, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', '_'], 4)),

    ExpectedCounts = [
        #{count => 1, id => NonGoalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GlobalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GoalCounterID, tag_counts => #{[NonGoalFeature] => 1}}
    ],

    ?assertEqual(
        lists:sort(ExpectedCounts),
        lists:sort(Counts)
    ),

    Config.

ec_test_triggering_a_goal_from_another_namespace(Config) ->
    User = <<"user_id">>,

    NonGoalFeature = <<"non_goal">>,
    GoalFeature = <<"goal">>,

    GlobalCounterPid = erlang:list_to_pid("<0.0.0>"),
    NSGlobalCounterPid = erlang:list_to_pid("<0.1.0>"),
    NonGoalCounterPid = erlang:list_to_pid("<0.0.1>"),
    GoalCounterPid = erlang:list_to_pid("<0.0.2>"),

    GlobalCounterID = features_counter_id:global_counter_id(<<"default">>),
    NSGlobalCounterID = features_counter_id:global_counter_id(<<"not default">>),
    NonGoalCounterID = features_counter_id:create(NonGoalFeature),
    GoalCounterID = features_counter_id:create(<<"not default">>, GoalFeature, named),

    meck:expect(
        supervisor,
        start_child,
        [features_counter_sup, '_'],
        meck:raise(error, should_not_hit_this)
    ),

    ?MUT:register_counter(GlobalCounterID, GlobalCounterPid),
    ?MUT:register_counter(NSGlobalCounterID, NSGlobalCounterPid),
    ?MUT:register_counter(NonGoalCounterID, NonGoalCounterPid),
    ?MUT:register_counter(GoalCounterID, GoalCounterPid),

    ?MUT:add_goal(<<"not default">>, GoalFeature),

    meck:expect(?COUNTER_MOD, add, ['_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, includes_key, [
        {['_', NonGoalCounterPid], true},
        {['_', GlobalCounterPid], true},
        {['_', GoalCounterPid], false}
    ]),

    meck:expect(?COUNTER_MOD, count, [
        {[NonGoalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GlobalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[NSGlobalCounterPid], #{count => 1, tag_counts => #{[] => 1}}},
        {[GoalCounterPid], #{count => 1, tag_counts => #{[] => 1}}}
    ]),
    % synchronize call
    _Goals = ?MUT:goals(<<"default">>),

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(<<"not default">>, GoalFeature, User, #{}),

    Counts = ?MUT:counts(<<"default">>),
    NamespaceCounts = ?MUT:counts(<<"not default">>),

    io:format("Adds ~p~n", [meck:history(?COUNTER_MOD)]),
    ExpectedEvents = lists:sort([]),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', GoalCounterPid], 1)),
    ?assertEqual(
        ExpectedEvents,
        lists:sort(meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_', GoalCounterPid], 2))
    ),

    ExpectedCounts = [
        #{count => 1, id => NonGoalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GlobalCounterID, tag_counts => #{[] => 1}}
    ],

    ExpectedNamespaceCounts = [
        #{count => 1, id => NSGlobalCounterID, tag_counts => #{[] => 1}},
        #{count => 1, id => GoalCounterID, tag_counts => #{[] => 1}}
    ],

    ?assertEqual(
        lists:sort(ExpectedCounts),
        lists:sort(Counts)
    ),

    ?assertEqual(
        lists:sort(ExpectedNamespaceCounts),
        lists:sort(NamespaceCounts)
    ),

    Config.

fa_test_event_no_persistence(Config) ->
    Feature = <<"feature_name">>,
    CounterPid = self(),
    CounterID = features_counter_id:create(Feature),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, CounterPid}),
    User = <<"user_id">>,

    StoreLibState = ?config(store_lib_state, Config),

    meck:expect(features_store_lib, get, [StoreLibState], {not_supported, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(undefined),
    Config1 = [{pid, Pid} | Config],

    meck:wait(features_store_lib, get, '_', 1000),

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(CounterID, undefined),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, '_', CounterPid])),
    Config1.

ga_test_with_value(Config) ->
    Feature = <<"feature_name">>,
    Value = 1,
    User = <<"user_id">>,

    ?MUT:add(<<"default">>, Feature, User, #{ensure_goal => true, value => Value}),

    Spec = spec_for_feature(Feature),

    ExpectedOtherCounters = [],

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, Value, '_'])),

    Config.

ha_test_namespaces(Config) ->
    NS1 = <<"default">>,
    NS2 = <<"not default">>,
    CID1 = features_counter_id:create(NS1, <<"feature">>, named),
    CID2 = features_counter_id:create(NS2, <<"feature">>, named),
    ?MUT:register_counter(CID1, self()),
    ?MUT:register_counter(CID2, self()),

    % Used for syncronization / processing messages
    ?MUT:goals(<<"default">>),

    RoutedNamespaces = lists:sort(?MUT:namespaces()),
    ExpectedNamespaces = lists:sort([NS1, NS2]),

    ?assertEqual(ExpectedNamespaces, RoutedNamespaces),

    Config.

ia_stop_counter(Config) ->
    Namespace = <<"default">>,
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Namespace, Feature, named),
    Pid = self(),

    ?MUT:register_counter(CounterID, Pid),
    % Used for syncronization / processing messages
    ?MUT:goals(Namespace),

    InitialCounts = ?MUT:counts(Namespace),
    ?assertNotEqual([], InitialCounts),

    ?MUT:stop_counter(CounterID),

    ID = {features_counter, CounterID},
    meck:wait(supervisor, delete_child, '_', 1000),
    test_utils:assertNCalls(1, supervisor, terminate_child, [features_counter_sup, ID]),
    test_utils:assertNCalls(1, supervisor, delete_child, [features_counter_sup, ID]),

    ExpectedData = expected_stored_data(#{counters => [CounterID]}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    CountsAfterStopping = ?MUT:counts(Namespace),

    ?assertEqual([], CountsAfterStopping),

    Config.

ib_stop_unregistered_counter(Config) ->
    Namespace = <<"default">>,
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Namespace, Feature, named),

    ?MUT:goals(Namespace),

    InitialCounts = ?MUT:counts(Namespace),
    ?assertEqual([], InitialCounts),

    ?MUT:stop_counter(CounterID),

    ID = {features_counter, CounterID},
    meck:wait(supervisor, delete_child, '_', 1000),
    test_utils:assertNCalls(1, supervisor, terminate_child, [features_counter_sup, ID]),
    test_utils:assertNCalls(1, supervisor, delete_child, [features_counter_sup, ID]),

    CountsAfterStopping = ?MUT:counts(Namespace),

    ?assertEqual([], CountsAfterStopping),

    Config.

ic_stop_counter_not_found(Config) ->
    Namespace = <<"default">>,
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Namespace, Feature, named),
    Pid = self(),
    meck:expect(supervisor, terminate_child, [features_counter_sup, '_'], {error, not_found}),
    meck:expect(supervisor, delete_child, [features_counter_sup, '_'], {error, not_found}),

    ?MUT:register_counter(CounterID, Pid),
    % Used for syncronization / processing messages
    ?MUT:goals(Namespace),

    InitialCounts = ?MUT:counts(Namespace),
    ?assertNotEqual([], InitialCounts),

    ?MUT:stop_counter(CounterID),

    ID = {features_counter, CounterID},
    meck:wait(supervisor, delete_child, '_', 1000),
    test_utils:assertNCalls(1, supervisor, terminate_child, [features_counter_sup, ID]),
    test_utils:assertNCalls(1, supervisor, delete_child, [features_counter_sup, ID]),

    ExpectedData = expected_stored_data(#{counters => [CounterID]}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    CountsAfterStopping = ?MUT:counts(Namespace),

    ?assertEqual([], CountsAfterStopping),

    Config.

expected_stored_data(Data) ->
    #{
        counters => maps:get(counters, Data, []),
        goals => maps:get(goals, Data, #{})
    }.

spec_for_feature(Feature) when is_binary(Feature) ->
    ID = features_counter_id:create(Feature),
    spec_for_feature(ID, ?STORE_LIB);
spec_for_feature(ID) ->
    spec_for_feature(ID, ?STORE_LIB).

spec_for_feature(Feature, StoreLibMod) ->
    #{
        id => {features_counter, Feature},
        start => {features_counter, start_link, [StoreLibMod, Feature]}
    }.

counts(C) ->
    Default = #{
        count => 0,
        single_tag_counts => #{},
        tag_counts => #{}
    },
    maps:merge(Default, C).
