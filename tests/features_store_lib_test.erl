-module(features_store_lib_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_store_lib).
-define(LIB_MOD, fake_store_lib_mod).


init_test() ->
    load(),
    Name = "test",
    LibState = make_ref(),
    meck:expect(?LIB_MOD, init, ['_'], LibState),

    _State = ?MUT:init(?LIB_MOD, "test"),

    ?assertEqual(Name, meck:capture(first, ?LIB_MOD, init, ['_'], 1)),

    unload().

get_test() ->
    load(),
    Name = "test",
    LibState = make_ref(),
    Data = make_ref(),
    meck:expect(?LIB_MOD, init, ['_'], LibState),
    meck:expect(?LIB_MOD, get_all, ['_'], {Data, LibState}),

    State = ?MUT:init(?LIB_MOD, Name),
    {Data, State} = ?MUT:get(State),

    ?assertEqual(LibState, meck:capture(first, ?LIB_MOD, get_all, ['_'], 1)),

    unload().

store_test() ->
    load(),
    Name = "test",
    LibState = {"lib_state", make_ref()},
    Data = {"data", make_ref()},
    meck:expect(?LIB_MOD, init, ['_'], LibState),
    meck:expect(?LIB_MOD, store, ['_', '_'], {ok, LibState}),

    State = ?MUT:init(?LIB_MOD, Name),
    {ok, State} = ?MUT:store(Data, State),

    ?assertEqual(Data, meck:capture(first, ?LIB_MOD, store, ['_', '_'], 1)),
    ?assertEqual(LibState, meck:capture(first, ?LIB_MOD, store, ['_', '_'], 2)),

    unload().

load() ->
    ok = meck:new(?LIB_MOD, [non_strict]).

unload() ->
    true = meck:validate(?LIB_MOD),
    ok = meck:unload(?LIB_MOD).
