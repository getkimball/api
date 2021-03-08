-module(features_bayesian_predictor_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_bayesian_predictor).

load() ->
    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, add, ['_', '_'], ok),

    ok.

unload(_) ->
    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ok.

goal_counters_test_() ->
    {foreach, fun load/0, fun unload/1, [fun goal_predictions/0, fun goal_predictions_namespace/0]}.

for_events_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun for_events_empty/0,
        fun for_events_one_match/0,
        fun for_events_that_is_the_goal/0,
        fun for_events_that_includes_the_goal/0,
        fun for_events_not_in_namespace/0,
        fun for_events_multiple/0
    ]}.

%%%%
%   Get goals from router
%%%%

goal_predictions() ->
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(FeatureName),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(GoalName),

    GlobalCounterID = features_counter_id:global_counter_id(<<"default">>),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [<<"default">>], CountMap),

    ExpectedPredictions = #{
        <<"goal_1">> => #{<<"feature_1">> => 0.5}
    },

    Predictions = ?MUT:for_goal_counts(<<"default">>),

    ?assertEqual(ExpectedPredictions, Predictions).

goal_predictions_namespace() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{
        <<"goal_1">> => #{<<"feature_1">> => 0.5}
    },

    Predictions = ?MUT:for_goal_counts(Namespace),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_empty() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{},

    Predictions = ?MUT:for_events(Namespace, []),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_one_match() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{
        GoalName => #{yes => 0.25, no => 0.25, likelihood => 1.0}
    },

    Predictions = ?MUT:for_events(Namespace, [FeatureName]),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_that_is_the_goal() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{
        GoalName => #{note => <<"Submitted events included this goal, no prediction made">>}
    },

    Predictions = ?MUT:for_events(Namespace, [GoalName]),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_that_includes_the_goal() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{
        GoalName => #{note => <<"Submitted events included this goal, no prediction made">>}
    },

    Predictions = ?MUT:for_events(Namespace, [FeatureName, GoalName]),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_not_in_namespace() ->
    Namespace = <<"test namespace">>,
    FeatureName = <<"feature_1">>,
    FeatureID = features_counter_id:create(Namespace, FeatureName, named),

    GoalName = <<"goal_1">>,
    GoalID = features_counter_id:create(Namespace, GoalName, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        FeatureID => #{count => 2, single_tag_counts => #{}},
        GoalID => #{count => 4, single_tag_counts => #{FeatureName => 1}},
        GlobalCounterID => #{count => 6, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    UnknownEvent = <<"No Match">>,
    ExpectedThrow = {unknown_event, UnknownEvent},

    ?assertThrow(ExpectedThrow, ?MUT:for_events(Namespace, [UnknownEvent])).

for_events_multiple() ->
    Namespace = <<"test namespace">>,

    ERed = <<"color:red">>,
    ERedID = features_counter_id:create(Namespace, ERed, named),
    EYellow = <<"color:yellow">>,
    EYellowID = features_counter_id:create(Namespace, EYellow, named),

    ESports = <<"type:sports">>,
    ESportsID = features_counter_id:create(Namespace, ESports, named),
    ESUV = <<"type:suv">>,
    ESUVID = features_counter_id:create(Namespace, ESUV, named),

    EDomestic = <<"type:domestic">>,
    EDomesticID = features_counter_id:create(Namespace, EDomestic, named),
    EImported = <<"type:imported">>,
    EImportedID = features_counter_id:create(Namespace, EImported, named),

    GoalStolen = <<"was_stolen">>,
    GoalID = features_counter_id:create(Namespace, GoalStolen, named),

    GlobalCounterID = features_counter_id:global_counter_id(Namespace),

    CountMap = #{
        ERedID => #{count => 5, single_tag_counts => #{}},
        EYellowID => #{count => 5, single_tag_counts => #{}},
        ESportsID => #{count => 6, single_tag_counts => #{}},
        ESUVID => #{count => 4, single_tag_counts => #{}},
        EDomesticID => #{count => 5, single_tag_counts => #{}},
        EImportedID => #{count => 5, single_tag_counts => #{}},
        GoalID => #{
            count => 5,
            single_tag_counts => #{
                ERed => 3,
                EYellow => 2,
                ESports => 4,
                ESUV => 1,
                EDomestic => 2,
                EImported => 3
            }
        },
        GlobalCounterID => #{count => 10, single_tag_counts => #{}}
    },
    ok = meck:expect(features_count_router, count_map, [Namespace], CountMap),

    ExpectedPredictions = #{
        GoalStolen => #{yes => 0.048, no => 0.144, likelihood => 0.048 / 0.144}
    },

    Predictions = ?MUT:for_events(Namespace, [ERed, ESUV, EDomestic]),

    ?assertEqual(ExpectedPredictions, Predictions).
