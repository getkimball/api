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
                ac_test_new_counter_as_goal,
                ad_test_existing_counter_as_goal,
                ae_test_new_counter_as_explictely_not_ensuring_goal,
                af_test_existing_counter_as_existing_goal,
                ag_test_counter_registration_race,
                ah_test_multiple_counts_added_at_once,
                ba_test_counter_counts,
                bb_test_counter_pids,
                ca_test_start_with_existing_counters,
                cb_test_counter_registration_persists,
                da_test_new_goal,
                db_test_existing_goal,
                ea_test_triggering_a_goal,
                eb_test_triggering_a_goal_registered_after_goal_added,
                fa_test_event_no_persistence
              ]}
            ].

init_meck(Config) ->
    test_utils:meck_load_prometheus(),
    meck:new(?COUNTER_MOD),
    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', '_'], ok),
    meck:expect(?COUNTER_MOD, count, ['_'], counts(#{count => -1})),
    meck:expect(?COUNTER_MOD, includes_key, ['_', '_'], false),

    StoreLibState = {store_lib_state, make_ref()},
    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', "count_router"], StoreLibState),
    meck:expect(features_store_lib, get, ['_'], {#{}, StoreLibState}),
    meck:expect(features_store_lib, store, ['_', '_'], {ok, StoreLibState}),

    meck:new(supervisor, [unstick]),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, self()}),
    [{store_lib_state, StoreLibState}|Config].

init_per_testcase(ca_test_start_with_existing_counters, Config) ->
    init_meck(Config);
init_per_testcase(db_test_existing_goal, Config) ->
    init_meck(Config);
init_per_testcase(fa_test_event_no_persistence, Config) ->
    init_meck(Config);
init_per_testcase(_, Config) ->
    Config1 = init_meck(Config),
    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    [{pid, Pid}|Config1].

end_per_testcase(_, Config) ->
    test_utils:meck_unload_prometheus(),
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
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    Config.

ab_test_existing_counter(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(Feature, User),
    ?MUT:register_counter(Feature, Pid),

    ?MUT:goals(), % Used for syncronization / processing messages

    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [?STORE_LIB, Feature]}},

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertError(not_found, meck:capture(2, supervisor, start_child, ['_', Spec], 2)),

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    ?assertEqual(User, meck:capture(2, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(2, ?COUNTER_MOD, add, '_', 2)),

    Config.

ac_test_new_counter_as_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User, #{ensure_goal=>true}),

    Spec = spec_for_feature(Feature),

    ExpectedOtherCounters = [],

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, Pid])),

    Config.

ad_test_existing_counter_as_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add(Feature, User, #{ensure_goal=>true}),
    ?MUT:register_counter(Feature, Pid),

    ?MUT:goals(), % Used for syncronization / processing messages

    ?MUT:add(Feature, User),

    Spec = #{id => {features_counter, Feature},
             start => {features_counter, start_link, [?STORE_LIB, Feature]}},
    ExpectedOtherCounters = [],

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertError(not_found, meck:capture(2, supervisor, start_child, ['_', Spec], 2)),

    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, Pid])),

    Config.

ae_test_new_counter_as_explictely_not_ensuring_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),
    User = <<"user_id">>,

    ?MUT:add(Feature, User, #{ensure_goal=>false}),

    Spec = spec_for_feature(Feature),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [User, Pid])),

    Config.

af_test_existing_counter_as_existing_goal(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    User = <<"user_id">>,

    ?MUT:add_goal(Feature),
    ?MUT:register_counter(Feature, Pid),
    ?MUT:goals(), % Used for syncronization / processing messages
    ?MUT:add(Feature, User, #{ensure_goal=>true}),
    ?MUT:goals(), % Used for syncronization / processing messages

    ExpectedOtherCounters = [],

    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [User, ExpectedOtherCounters, Pid])),

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

    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(Pid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),
    Config.

ah_test_multiple_counts_added_at_once(Config) ->
    Pid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, Pid}),

    Adds = [
        {<<"event_1">>, <<"user_1">>, #{}},
        {<<"event_2">>, <<"user_2">>, #{ensure_goal => true}}
    ],

    ?MUT:add(Adds),

    ?assertEqual(2, meck:num_calls(?COUNTER_MOD, add, [<<"user_1">>, Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, Pid])),
    ?assertEqual(1, meck:num_calls(?COUNTER_MOD, add, [<<"user_2">>, [], Pid])),

    Config.

ba_test_counter_counts(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    Num = 1,
    Count = counts(#{count => Num}),

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(Feature, Pid),

    ?MUT:goals(), % Run to synchronize/handle all messages

    Counts = ?MUT:counts(),

    ?assertEqual([counts(#{name => Feature, count => Num})], Counts),
    Config.

bb_test_counter_pids(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),

    ?MUT:register_counter(Feature, Pid),

    ?MUT:goals(), % Run to synchronize/handle all messages

    Pids = ?MUT:counter_pids(),

    ?assertEqual([Pid], Pids),
    Config.

ca_test_start_with_existing_counters(Config) ->
    StoreLibState = ?config(store_lib_state, Config),
    Feature = <<"feature_name">>,
    Num = 1,
    Count = #{count => Num},
    Spec = spec_for_feature(Feature),
    StoredData = #{counters => [Feature]},

    meck:expect(features_store_lib, get, [StoreLibState], {StoredData, StoreLibState}),
    meck:expect(features_counter, count, ['_'], Count),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid}|Config],

    meck:wait(features_store_lib, get, '_', 1000),
    timer:sleep(500),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),

    Config1.

cb_test_counter_registration_persists(Config) ->
    Feature = <<"feature_name">>,
    Pid = self(),
    Num = 1,
    Count = counts(#{count => Num}),

    meck:expect(features_counter, count, [Pid], Count),

    ?MUT:register_counter(Feature, Pid),
    meck:wait(features_store_lib, store, '_', 1000),

    ExpectedData = expected_stored_data(#{counters=>[Feature]}),
    ?assertEqual(ExpectedData, meck:capture(first, features_store_lib, store, '_', 1)),

    Counts = ?MUT:counts(),

    ?assertEqual([counts(#{name => Feature, count => Num})], Counts),

    Config.


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

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    Config1 = [{pid, Pid}|Config],

    meck:wait(features_store_lib, get, '_', 1000),
    Goals = ?MUT:goals(),

    ExpectedGoals = [Goal],

    ?assertEqual(ExpectedGoals, Goals),

    % Ensure that this doesn't write to the store again when adding the goal again
    ?MUT:add_goal(Goal),
    ?assertError(not_found, meck:capture(first, features_store_lib, store, '_', 1)),

    Config1.

ea_test_triggering_a_goal(Config) ->
    User = <<"user_id">>,

    NonGoalFeature = <<"non_goal">>,
    GoalFeature = <<"goal">>,

    GlobalCounterPid = erlang:list_to_pid("<0.0.0>"),
    NonGoalCounterPid = erlang:list_to_pid("<0.0.1>"),
    GoalCounterPid = erlang:list_to_pid("<0.0.2>"),

    meck:expect(supervisor, start_child, [features_counter_sup, '_'], meck:raise(error, should_not_hit_this)),

    ?MUT:register_counter(global_counter, GlobalCounterPid),
    ?MUT:register_counter(NonGoalFeature, NonGoalCounterPid),
    ?MUT:register_counter(GoalFeature, GoalCounterPid),

    ?MUT:add_goal(GoalFeature),


    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', GoalCounterPid], ok),
    meck:expect(?COUNTER_MOD, includes_key, [{['_', NonGoalCounterPid], true},
                                             {['_', GlobalCounterPid], true},
                                             {['_', GoalCounterPid], false}]),


    meck:expect(?COUNTER_MOD, count, [{[NonGoalCounterPid], #{count=>1, tag_counts=>#{[] => 1}}},
                                      {[GlobalCounterPid], #{count=>1, tag_counts=>#{[] => 1}}},
                                      {[GoalCounterPid], #{count=>1, tag_counts=>#{[NonGoalFeature] => 1}}}]),
    _Goals = ?MUT:goals(), % synchronize call

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    Counts = ?MUT:counts(),

    ExpectedEvents = [global_counter, NonGoalFeature],
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedEvents, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 2)),
    ?assertEqual(GoalCounterPid, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 3)),

    ExpectedCounts = [
        #{count => 1, name => NonGoalFeature, tag_counts => #{[] => 1}},
        #{count => 1, name => global_counter, tag_counts => #{[] => 1}},
        #{count => 1, name => GoalFeature, tag_counts => #{[NonGoalFeature] => 1}}
    ],

    ?assertEqual(lists:sort(ExpectedCounts),
                 lists:sort(Counts)),

    Config.

eb_test_triggering_a_goal_registered_after_goal_added(Config) ->
    User = <<"user_id">>,

    NonGoalFeature = <<"non_goal">>,
    GoalFeature = <<"goal">>,

    GlobalCounterPid = erlang:list_to_pid("<0.0.0>"),
    NonGoalCounterPid = erlang:list_to_pid("<0.0.1>"),
    GoalCounterPid = erlang:list_to_pid("<0.0.2>"),

    meck:expect(supervisor, start_child, [features_counter_sup, '_'], meck:raise(error, should_not_hit_this)),

    ?MUT:register_counter(global_counter, GlobalCounterPid),
    ?MUT:register_counter(NonGoalFeature, NonGoalCounterPid),

    ?MUT:add_goal(GoalFeature),

    _Goals0 = ?MUT:goals(), % synchronize call

    ?MUT:register_counter(GoalFeature, GoalCounterPid),

    meck:expect(?COUNTER_MOD, add, ['_', '_'], ok),
    meck:expect(?COUNTER_MOD, add, ['_', '_', GoalCounterPid], ok),
    meck:expect(?COUNTER_MOD, includes_key, [{['_', NonGoalCounterPid], true},
                                             {['_', GlobalCounterPid], true},
                                             {['_', GoalCounterPid], false}]),


    meck:expect(?COUNTER_MOD, count, [{[NonGoalCounterPid], #{count=>1, tag_counts=>#{[] => 1}}},
                                      {[GlobalCounterPid], #{count=>1, tag_counts=>#{[] => 1}}},
                                      {[GoalCounterPid], #{count=>1, tag_counts=>#{[NonGoalFeature] => 1}}}]),
    _Goals1 = ?MUT:goals(), % synchronize call

    ?MUT:add(NonGoalFeature, User),
    ?MUT:add(GoalFeature, User),

    _Goals2 = ?MUT:goals(), % synchronize call

    Counts = ?MUT:counts(),

    ExpectedEvents = [global_counter, NonGoalFeature],
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedEvents, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 2)),
    ?assertEqual(GoalCounterPid, meck:capture(first, ?COUNTER_MOD, add, ['_', '_', '_'], 3)),

    ExpectedCounts = [
        #{count => 1, name => NonGoalFeature, tag_counts => #{[] => 1}},
        #{count => 1, name => global_counter, tag_counts => #{[] => 1}},
        #{count => 1, name => GoalFeature, tag_counts => #{[NonGoalFeature] => 1}}
    ],

    ?assertEqual(lists:sort(ExpectedCounts),
                 lists:sort(Counts)),

    Config.


fa_test_event_no_persistence(Config) ->
    Feature = <<"feature_name">>,
    CounterPid = self(),
    meck:expect(supervisor, start_child, [features_counter_sup, '_'], {ok, CounterPid}),
    User = <<"user_id">>,

    StoreLibState = ?config(store_lib_state, Config),

    meck:expect(features_store_lib, get, [StoreLibState], {not_supported, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(undefined),
    Config1 = [{pid, Pid}|Config],

    meck:wait(features_store_lib, get, '_', 1000),

    ?MUT:add(Feature, User),

    Spec = spec_for_feature(Feature, undefined),

    ?assertEqual(Spec, meck:capture(first, supervisor, start_child, ['_', Spec], 2)),
    ?assertEqual(User, meck:capture(first, ?COUNTER_MOD, add, '_', 1)),
    ?assertEqual(CounterPid, meck:capture(first, ?COUNTER_MOD, add, '_', 2)),

    Config1.

expected_stored_data(Data) ->
    #{
        counters => maps:get(counters, Data, []),
        goals => maps:get(goals, Data, [])
    }.

spec_for_feature(Feature) ->
    spec_for_feature(Feature, ?STORE_LIB).

spec_for_feature(Feature, StoreLibMod) ->
    #{id => {features_counter, Feature},
      start => {features_counter, start_link, [StoreLibMod, Feature]}}.

counts(C) ->
    Default = #{count => 0,
                single_tag_counts => #{},
                tag_counts => #{}},
    maps:merge(Default, C).
