-module(features_store_lib_configmap_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_store_lib_configmap).


init_test() ->
    ok = meck:new(kuberlnetes),
    ok = meck:new(swaggerl),

    API = make_ref(),

    Operations = [
        <<"createCoreV1NamespacedConfigMap">>,
        <<"readCoreV1NamespacedConfigMap">>,
        <<"replaceCoreV1NamespacedConfigMap">>
    ],

    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),

    _State = ?MUT:init(),

    Expected = [{operations, Operations}],
    ?assertEqual(Expected, meck:capture(first, kuberlnetes, load, '_', 1)),



    true = meck:validate(kuberlnetes),
    true = meck:validate(swaggerl),
    ok = meck:unload(kuberlnetes),
    ok = meck:unload(swaggerl),
    ok.

write_read_test() ->
    ok = meck:new(kuberlnetes),
    ok = meck:new(swaggerl),

    API = make_ref(),

    Operations = [
        <<"createCoreV1NamespacedConfigMap">>,
        <<"readCoreV1NamespacedConfigMap">>,
        <<"replaceCoreV1NamespacedConfigMap">>
    ],

    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),
    ok = meck:expect(swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], #{<<"code">>=>200}),

    State = ?MUT:init(),

    Expected = [{operations, Operations}],
    ?assertEqual(Expected, meck:capture(first, kuberlnetes, load, '_', 1)),

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    State = ?MUT:store(Data, State),

    StoreOps = meck:capture(first, swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], 3),
    ConfigMapDoc = proplists:get_value(<<"body">>, StoreOps),

    ok = meck:expect(swaggerl, op, [API, "readCoreV1NamespacedConfigMap", '_'], ConfigMapDoc),

    {Features, State} = ?MUT:get_all(State),

    ?assertEqual(Data, Features),


    true = meck:validate(kuberlnetes),
    true = meck:validate(swaggerl),
    ok = meck:unload(kuberlnetes),
    ok = meck:unload(swaggerl),
    ok.
