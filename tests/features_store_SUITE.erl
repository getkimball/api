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
                bb_external_store_store_data
              ]}
            ].

aa_write_read(_Config) ->
    {ok, Pid} = ?MUT:start_link(),
    Name = <<"feature">>,
    Status = <<"enabled">>,

    ok = features_store:set_binary_feature(Name, Status),
    Resp = features_store:get_binary_features(),

    Expected = #{Name => enabled},
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    ok.

ba_external_store_init(_Config) ->
    ok = meck:new(?STORE_LIB, [non_strict]),

    Name = <<"feature">>,
    Status = <<"enabled">>,

    StoreLibState = make_ref(),

    ok = meck:expect(?STORE_LIB, init, fun() ->
        StoreLibState
    end),
    ok = meck:expect(?STORE_LIB, get_all, fun(Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {[#{name=>Name, status=>Status}], Ref}
    end),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),
    meck:wait(?STORE_LIB, get_all, '_', 1000),
    Resp = features_store:get_binary_features(),

    Expected = #{Name => enabled},
    ?assertEqual(Expected, Resp),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.

bb_external_store_store_data(_Config) ->
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
    ok = meck:expect(?STORE_LIB, store, fun(_Data, Ref) ->
        ?assertEqual(StoreLibState, Ref),
        {ok, Ref}
    end),

    {ok, Pid} = ?MUT:start_link(?STORE_LIB),

    ok = features_store:set_binary_feature(Name, Status),

    Expected = [#{name => Name, status => Status}],
    ?assertEqual(Expected, meck:capture(first, ?STORE_LIB, store, '_', 1)),

    exit(Pid, normal),
    true = meck:validate(?STORE_LIB),
    ok = meck:unload(?STORE_LIB),
    ok.
