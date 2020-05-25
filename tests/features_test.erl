-module(features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features).

collapse_to_boolean_with_boolean_test() ->
    load(),

    FalseSpec = test_utils:defaulted_feature_spec(
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        #{boolean => true}),

    ?assertEqual(false, ?MUT:collapse_to_boolean(FalseSpec, 0)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(TrueSpec, 0)),

    unload().

collapse_to_boolean_with_rollout_test() ->
    load(),
    Now = erlang:system_time(seconds),

    BeforeSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now + 10,
          rollout_end => Now + 60}),

    AfterSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now - 60,
          rollout_end => Now - 10}),

    ?assertEqual(false, ?MUT:collapse_to_boolean(BeforeSpec, Now)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(AfterSpec, Now)),

    unload().

collapse_features_map_test() ->
    load(),
    FalseSpec = test_utils:defaulted_feature_spec(
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        #{boolean => true}),

    Spec = #{
        <<"false">> => FalseSpec,
        <<"true">> => TrueSpec
    },

    Expected = #{
        <<"false">> => false,
        <<"true">> => true
    },

    Value = ?MUT:collapse_features_map(Spec),

    ?assertEqual(Expected, Value),

    unload().


load() ->
    ok.

unload() ->
    ok.
