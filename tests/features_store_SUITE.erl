-module(features_store_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_store).
-define(STORE_LIB, fake_store_lib).


all() -> [{group, test_ets}].

groups() -> [{test_ets, [
                aa_write_read,
                ab_invalid_feature_missing_rollout_end,
                ac_invalid_feature_before_after_end,
                ad_user_spec_write_read,
                ae_user_spec_write_read_binary,
                ba_external_store_init,
                bb_external_store_store_data,
                bc_external_store_not_supporting_store,
                ca_write_read_rollout
              ]}
            ].

init_per_testcase(ba_external_store_init, Config) ->
    StoreLibState = make_ref(),

    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', "features_store"], StoreLibState),

    meck:expect(features_store_lib, get, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[], Ref}
    end),

    meck:expect(features_store_lib, store, fun(_, Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {ok, Ref}
    end),

    [{store_lib_state, StoreLibState}|Config];

init_per_testcase(_, Config) ->
    StoreLibState = make_ref(),

    meck:new(features_store_lib),
    meck:expect(features_store_lib, init, ['_', "features_store"], StoreLibState),

    meck:expect(features_store_lib, get, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[], Ref}
    end),

    meck:expect(features_store_lib, store, fun(_, Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {ok, Ref}
    end),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),

    [{store_lib_state, StoreLibState},
     {pid, Pid} |Config].

end_per_testcase(_, _Config) ->
    ?assert(meck:validate(features_store_lib)),
    meck:unload(features_store_lib),
    ok.

aa_write_read(Config) ->
    Name = <<"feature">>,
    Boolean = true,

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, undefined}),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(Name,
      #{boolean => Boolean})],
    ?assertEqual(Expected, Resp),

    Config.

ab_invalid_feature_missing_rollout_end(Config) ->
    Throw = {invalid_feature, "Rollout start must have an end"},
    Name = <<"name">>,
    Boolean = {boolean, undefined},
    Rollout = {rollout, 1, undefined},
    User = {user, []},
    ?assertThrow(Throw, ?MUT:set_feature(Name, Boolean, Rollout, User)),

    Config.

ac_invalid_feature_before_after_end(Config) ->
    Throw = {invalid_feature, "Rollout start cannot be after the end"},
    Name = <<"name">>,
    Boolean = {boolean, undefined},
    Rollout = {rollout, 2, 1},
    User = {user, []},
    ?assertThrow(Throw, ?MUT:set_feature(Name, Boolean, Rollout, User)),

    Config.

ad_user_spec_write_read(Config) ->
    Name = <<"feature">>,
    Boolean = true,
    UserSpec = [[<<"user_id">>, '=', 42]],

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, UserSpec}),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(Name,
      #{boolean => Boolean,
        user => UserSpec})],
    ?assertEqual(Expected, Resp),

    Config.

ae_user_spec_write_read_binary(Config) ->
    Name = <<"feature">>,
    Boolean = true,
    UserSpec = [[<<"user_id">>, <<"=">>, 42]],
    ExpectedUserSpec = [[<<"user_id">>, '=', 42]],

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, UserSpec}),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(Name,
      #{boolean => Boolean,
        user => ExpectedUserSpec})],
    ?assertEqual(Expected, Resp),

    Config.

ba_external_store_init(Config) ->
    NameA = <<"featureA">>,
    BooleanA = true,
    NameB = <<"featureB">>,
    BooleanB = false,

    StoreLibState = ?config(store_lib_state, Config),

    All = [
      test_utils:defaulted_feature_spec(NameA, #{boolean=>BooleanA}),
      replace_keys_with_binary(test_utils:defaulted_feature_spec(NameB, #{boolean=>BooleanB,
                                                                          rollout_start=><<"undefined">>,
                                                                          rollout_end=>1}))
    ],
    ok = meck:expect(features_store_lib, get, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {All, Ref}
    end),

    %% TODO: ^ test with binary keys

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    meck:wait(features_store_lib, get, '_', 1000),

    % There isn't a synchronization point as getting features reads from ets,
    % some sync point should be added instead of sleeping
    timer:sleep(100),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(NameA, #{boolean => BooleanA}),
                test_utils:defaulted_feature_spec(NameB, #{boolean => BooleanB,
                                                           rollout_end => 1})],

    ?assertEqual(?STORE_LIB, meck:capture(first, features_store_lib, init, '_', 1)),
    ?assertEqual(Expected, Resp),

    [{pid, Pid}|Config].

bb_external_store_store_data(Config) ->
    Name = <<"feature">>,
    Boolean = true,
    UserSpecs = [[<<"user_id">>, '=', <<"42">>]],

    StoreLibState = ?config(store_lib_state, Config),

    ok = meck:expect(features_store_lib, store, fun(_Data, Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {ok, Ref}
    end),

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, UserSpecs}),

    Expected = [test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean,
                                                          user=>UserSpecs})],
    ?assertEqual(Expected, meck:capture(first, features_store_lib, store, '_', 1)),

    Config.

bc_external_store_not_supporting_store(Config) ->
    Name = <<"feature">>,
    Status = <<"enabled">>,

    StoreLibState = make_ref(),

    ok = meck:expect(features_store_lib, store, ['_', '_'], {not_suported, StoreLibState}),


    Resp = features_store:set_feature(Name, {boolean, Status},
                                            {rollout, undefined, undefined},
                                            {user, []}),
    ?assertEqual(not_suported, Resp),

    Config.


ca_write_read_rollout(Config) ->
    Name = <<"feature">>,
    Start = erlang:system_time(seconds),
    End = erlang:system_time(seconds) + 100,

    ok = features_store:set_feature(Name, {boolean, false},
                                          {rollout, Start, End},
                                          {user, []}),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(Name,
      #{rollout_start=>Start, rollout_end=>End})],
    ?assertEqual(Expected, Resp),

    Config.

replace_keys_with_binary(Map) ->
    Fun = fun(K,V, AccIn) ->
        Kb = erlang:atom_to_binary(K, utf8),
        maps:put(Kb, V, AccIn)
    end,

    maps:fold(Fun, #{}, Map).
