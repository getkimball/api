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
        fun for_events_no_match/0,
        fun for_events_one_match_one_no_match/0
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
        <<"goal_1">> => 0.5
    },

    Predictions = ?MUT:for_events(Namespace, [FeatureName]),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_no_match() ->
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

    Predictions = ?MUT:for_events(Namespace, [<<"No match">>]),

    ?assertEqual(ExpectedPredictions, Predictions).

for_events_one_match_one_no_match() ->
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
        <<"goal_1">> => 0.5
    },

    ?assertEqual(ExpectedPredictions, ?MUT:for_events(Namespace, [FeatureName, <<"No Match">>])),
    ?assertEqual(ExpectedPredictions, ?MUT:for_events(Namespace, [<<"No Match">>, FeatureName])).
