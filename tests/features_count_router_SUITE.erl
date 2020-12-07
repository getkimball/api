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
            ba_test_counter_counts,
            bb_test_counter_pids,
            bc_test_counter_count_map,
            ca_test_start_with_existing_counters,
            cb_test_counter_registration_persists,
            cc_test_weekly_cohort_counter_created,
            cd_test_weekly_cohort_counter_created_and_added_again,
            da_test_new_goal,
            db_test_existing_goal,
            ea_test_triggering_a_goal,
            eb_test_triggering_a_goal_registered_after_goal_added,
            fa_test_event_no_persistence,
            ga_test_with_value
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
    ?MUT:goals(),

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

    ?MUT:add(Feature, User, #{ensure_goal => true}),

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

    ?MUT:add(Feature, User, #{ensure_goal => true}),
    ?MUT:register_counter(CounterID, Pid),

    % Used for syncronization / processing messages
    ?MUT:goals(),

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

    ?MUT:add(Feature, User, #{ensure_goal => false}),

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

    ?MUT:add_goal(Feature),
    ?MUT:register_counter(CounterID, Pid),
    % Used for syncronization / processing messages
    ?MUT:goals(),
    ?MUT:add(Feature, User, #{ensure_goal => true}),
    % Used for syncronization / processing messages
    ?MUT:goals(),

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
        {<<"event_1">>, <<"user_1">>, #{}},
        {<<"event_2">>, <<"user_2">>, #{ensure_goal => true}}
    ],

    ?MUT:add(Adds),

    io:format("Calls ~p~n", [meck:history(?COUNTER_MOD)]),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [<<"user_1">>, '_', Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, '_', Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, [], '_', Pid])),

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
    ?MUT:goals(),

    Counts = ?MUT:counts(),

    ?assertEqual([counts(#{id => CounterID, count => Num})], Counts),
    Config.

bb_test_counter_pids(Config) ->
    Feature = <<"feature_name">>,
    CounterID = features_counter_id:create(Feature),
    Pid = self(),

    ?MUT:register_counter(CounterID, Pid),

    % Run to synchronize/handle all messages
    ?MUT:goals(),

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
    ?MUT:goals(),

    Counts = ?MUT:count_map(),

    ?assertEqual(#{CounterID => Count}, Counts),
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

    Counts = ?MUT:counts(),

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

    ?MUT:add_goal(Goal),
    Goals = ?MUT:goals(),

    ExpectedGoals = [Goal],

    ExpectedData = expected_stored_data(#{goals => ExpectedGoals}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    ?assertEqual(ExpectedGoals, Goals),

    Config.

db_test_existing_goal(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Goal = <<"feature_name">>,
    StoredData = #{goals => [Goal]},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid} | Config],

    meck:wait(features_store_lib, get, '_', 1000),
    Goals = ?MUT:goals(),

    ExpectedGoals = [Goal],

    ?assertEqual(ExpectedGoals, Goals),

    % Ensure that this doesn't write to the store again when adding the goal again
    ?MUT:add_goal(Goal),
    ?assertError(not_found, meck:capture(first, features_store_lib, store, '_', 1)),

    Config1.

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

    ?MUT:add_goal(GoalFeature),

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
    _Goals = ?MUT:goals(),

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    Counts = ?MUT:counts(),

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

    ?MUT:add_goal(GoalFeature),

    % synchronize call
    _Goals0 = ?MUT:goals(),

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
    _Goals1 = ?MUT:goals(),

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    % synchronize call
    _Goals2 = ?MUT:goals(),

    Counts = ?MUT:counts(),

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

    ?MUT:add(Feature, User, #{ensure_goal => true, value => Value}),

    Spec = spec_for_feature(Feature),

    ExpectedOtherCounters = [],

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, Value, '_'])),

    Config.

expected_stored_data(Data) ->
    #{
        counters => maps:get(counters, Data, []),
        goals => maps:get(goals, Data, [])
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
