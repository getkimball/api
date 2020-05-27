-module(features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features).

collapse_to_boolean_with_boolean_test() ->
    load(),

    FalseSpec = test_utils:defaulted_feature_spec(
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        #{boolean => true}),

    ?assertEqual(false, ?MUT:collapse_to_boolean(FalseSpec, 0, 0)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(TrueSpec, 0, 0)),

    unload().

collapse_to_boolean_with_rollout_test() ->
    load(),
    Now = erlang:system_time(seconds),
    Rand = 0.5,

    % Before the start of the rollout
    BeforeSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now + 10,
          rollout_end => Now + 60}),

    % Not yet passed 50% of the rollout
    NotReachedSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now -5,
          rollout_end => Now + 10}),

    % Passed 50% of the rollout
    ReachedSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now - 10,
          rollout_end => Now - 5}),

    % After the end of the rollout
    AfterSpec = test_utils:defaulted_feature_spec(
        #{rollout_start => Now - 60,
          rollout_end => Now - 10}),

    ?assertEqual(false, ?MUT:collapse_to_boolean(BeforeSpec, Now, Rand)),
    ?assertEqual(false, ?MUT:collapse_to_boolean(NotReachedSpec, Now, Rand)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(ReachedSpec, Now, Rand)),
    ?assertEqual(true, ?MUT:collapse_to_boolean(AfterSpec, Now, Rand)),

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
    meck:new(rand, [unstick]),
    meck:expect(rand, uniform, [], 0.5),
    ok.

unload() ->
    true = meck:validate(rand),
    meck:unload(rand),
    ok.
