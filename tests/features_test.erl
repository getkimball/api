-module(features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features).

collapse_to_boolean_with_boolean_test() ->
    load(),
    Name = <<"example_name">>,
    FalseSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => true}),

    ?assertEqual({Name, false}, ?MUT:collapse_to_boolean(FalseSpec, #{}, 0, 0)),
    ?assertEqual({Name, true}, ?MUT:collapse_to_boolean(TrueSpec, #{}, 0, 0)),

    unload().

collapse_to_boolean_with_rollout_test() ->
    load(),
    Name = <<"example_name">>,
    Now = erlang:system_time(seconds),
    Rand = 0.5,

    % Before the start of the rollout
    BeforeSpec = test_utils:defaulted_feature_spec(
        Name,
        #{rollout_start => Now + 10,
          rollout_end => Now + 60}),

    % Not yet passed 50% of the rollout
    NotReachedSpec = test_utils:defaulted_feature_spec(
        Name,
        #{rollout_start => Now -5,
          rollout_end => Now + 10}),

    % Passed 50% of the rollout
    ReachedSpec = test_utils:defaulted_feature_spec(
        Name,
        #{rollout_start => Now - 10,
          rollout_end => Now - 5}),

    % After the end of the rollout
    AfterSpec = test_utils:defaulted_feature_spec(
        Name,
        #{rollout_start => Now - 60,
          rollout_end => Now - 10}),

    ?assertEqual({Name, false}, ?MUT:collapse_to_boolean(BeforeSpec, #{}, Now, Rand)),
    ?assertEqual({Name, false}, ?MUT:collapse_to_boolean(NotReachedSpec, #{}, Now, Rand)),
    ?assertEqual({Name, true}, ?MUT:collapse_to_boolean(ReachedSpec, #{}, Now, Rand)),
    ?assertEqual({Name, true}, ?MUT:collapse_to_boolean(AfterSpec, #{}, Now, Rand)),

    unload().

collapse_to_boolean_with_user_id_test() ->
    load(),
    Name = <<"example_name">>,
    TrueSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => false,
          user => [{<<"user_id">>, '=', 42}]}),
    FalseSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => false,
          user => [{<<"user_id">>, '=', 24}]}),
    LongTrueSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => false,
          user => [ {<<"user_id">>, '=', 24},
                   {<<"user_id">>, '=', 42}]}),
    LongFalseSpec = test_utils:defaulted_feature_spec(
        Name,
        #{boolean => false,
          user => [ {<<"user_id">>, '=', 1},
                   {<<"user_id">>, '=', 2}]}),

    User = #{<<"user_id">> => 42},

    ?assertEqual({Name, false}, ?MUT:collapse_to_boolean(FalseSpec, User, 0, 0)),
    ?assertEqual({Name, false}, ?MUT:collapse_to_boolean(LongFalseSpec, User, 0, 0)),
    ?assertEqual({Name, true}, ?MUT:collapse_to_boolean(TrueSpec, User, 0, 0)),
    ?assertEqual({Name, true}, ?MUT:collapse_to_boolean(LongTrueSpec, User, 0, 0)),

    unload().

collapse_features_to_map_test() ->
    load(),
    FalseSpec = test_utils:defaulted_feature_spec(
        <<"false">>,
        #{boolean => false}),
    TrueSpec = test_utils:defaulted_feature_spec(
        <<"true">>,
        #{boolean => true}),

    Spec = [FalseSpec, TrueSpec],

    Expected = #{
        <<"false">> => false,
        <<"true">> => true
    },

    Value = ?MUT:collapse_features_to_map(Spec, #{}),

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
