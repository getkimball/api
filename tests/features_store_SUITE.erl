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
                ba_external_store_init,
                bb_external_store_store_data,
                bc_external_store_not_supporting_store,
                ca_write_read_rollout
              ]}
            ].

aa_write_read(_Config) ->
    {ok, Pid} = ?MUT:start_link(),
    Name = <<"feature">>,
    Boolean = true,

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, undefined}),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(Name,
      #{boolean => Boolean})],
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    ok.

ab_invalid_feature_missing_rollout_end(_Config) ->
    Throw = {invalid_feature, "Rollout start must have an end"},
    Name = <<"name">>,
    Boolean = {boolean, undefined},
    Rollout = {rollout, 1, undefined},
    User = {user, []},
    ?assertThrow(Throw, ?MUT:set_feature(Name, Boolean, Rollout, User)).

ac_invalid_feature_before_after_end(_Config) ->
    Throw = {invalid_feature, "Rollout start cannot be after the end"},
    Name = <<"name">>,
    Boolean = {boolean, undefined},
    Rollout = {rollout, 2, 1},
    User = {user, []},
    ?assertThrow(Throw, ?MUT:set_feature(Name, Boolean, Rollout, User)).

ad_user_spec_write_read(_Config) ->
    {ok, Pid} = ?MUT:start_link(),
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

    exit(Pid, normal),
    ok.

ba_external_store_init(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    NameA = <<"featureA">>,
    BooleanA = true,
    NameB = <<"featureB">>,
    BooleanB = false,

    StoreLibState = make_ref(),

    ok = meck:expect(?STORE_LIB, init, fun() ->
        StoreLibState
    end),
    All = [
      test_utils:defaulted_feature_spec(NameA, #{boolean=>BooleanA}),
      replace_keys_with_binary(test_utils:defaulted_feature_spec(NameB, #{boolean=>BooleanB,
                                                                          rollout_start=><<"undefined">>,
                                                                          rollout_end=>1}))
    ],
    ok = meck:expect(?STORE_LIB, get_all, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {All, Ref}
    end),

    %% TODO: ^ test with binary keys

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    meck:wait(?STORE_LIB, get_all, '_', 1000),
    Resp = features_store:get_features(),

    Expected = [test_utils:defaulted_feature_spec(NameA, #{boolean => BooleanA}),
                test_utils:defaulted_feature_spec(NameB, #{boolean => BooleanB,
                                                           rollout_end => 1})],
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.

bb_external_store_store_data(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    Name = <<"feature">>,
    Boolean = true,
    UserSpecs = [[<<"user_id">>, '=', <<"42">>]],

    StoreLibState = make_ref(),

    ok = meck:expect(?STORE_LIB, init, fun() ->
        StoreLibState
    end),
    ok = meck:expect(?STORE_LIB, get_all, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[], Ref}
    end),
    ok = meck:expect(?STORE_LIB, store, fun(_Data, Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {ok, Ref}
    end),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),

    ok = features_store:set_feature(Name, {boolean, Boolean},
                                          {rollout, undefined, undefined},
                                          {user, UserSpecs}),

    Expected = [test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean,
                                                          user=>UserSpecs})],
    ?assertEqual(Expected, meck:capture(first, ?STORE_LIB, store, '_', 1)),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.

bc_external_store_not_supporting_store(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    Name = <<"feature">>,
    Status = <<"enabled">>,

    StoreLibState = make_ref(),

    ok = meck:expect(?STORE_LIB, init, fun() ->
        StoreLibState
    end),
    ok = meck:expect(?STORE_LIB, get_all, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[], Ref}
    end),
    ok = meck:expect(?STORE_LIB, store, ['_', '_'],
                                        {not_suported, StoreLibState}),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),

    Resp = features_store:set_feature(Name, {boolean, Status},
                                            {rollout, undefined, undefined},
                                            {user, []}),
    ?assertEqual(not_suported, Resp),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.


ca_write_read_rollout(_Config) ->
    {ok, Pid} = ?MUT:start_link(),
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

    exit(Pid, normal),
    ok.


replace_keys_with_binary(Map) ->
    Fun = fun(K,V, AccIn) ->
        Kb = erlang:atom_to_binary(K, utf8),
        maps:put(Kb, V, AccIn)
    end,

    maps:fold(Fun, #{}, Map).
