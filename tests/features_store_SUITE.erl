-module(features_store_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").


-define(MUT, features_store).
-define(STORE_LIB, fake_store_lib).


all() -> [{group, test_ets}].

groups() -> [{test_ets, [
                aa_write_read,
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

    ok = features_store:set_feature(Name, boolean, Boolean),
    Resp = features_store:get_features(),

    Expected = #{Name => test_utils:defaulted_feature_spec(
      #{boolean => Boolean})},
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    ok.

ba_external_store_init(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    Name = <<"feature">>,
    Boolean = true,

    StoreLibState = make_ref(),

    ok = meck:expect(?STORE_LIB, init, fun() ->
        StoreLibState
    end),
    ok = meck:expect(?STORE_LIB, get_all, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[#{name=>Name, boolean=>Boolean}], Ref}
    end),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    meck:wait(?STORE_LIB, get_all, '_', 1000),
    Resp = features_store:get_features(),

    Expected = #{Name => test_utils:defaulted_feature_spec(
                         #{boolean => Boolean})},
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.

bb_external_store_store_data(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    Name = <<"feature">>,
    Boolean = true,

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

    ok = features_store:set_feature(Name, boolean, Boolean),

    Expected = [test_utils:defaulted_feature_spec(#{name=>Name,
                                                    boolean=>Boolean})],
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

    Resp = features_store:set_feature(Name, boolean, Status),

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

    ok = features_store:set_feature(Name, rollout, Start, End),
    Resp = features_store:get_features(),

    Expected = #{Name => test_utils:defaulted_feature_spec(
      #{rollout_start=>Start, rollout_end=>End})},
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    ok.
