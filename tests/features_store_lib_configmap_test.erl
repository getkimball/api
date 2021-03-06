-module(features_store_lib_configmap_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_store_lib_configmap).

init_test() ->
    load(),

    API = make_ref(),
    Name = "test",

    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),

    _State = ?MUT:init(Name),
    ok = assert_kubernerlnetes_loaded(),

    unload(),
    ok.

write_read_test() ->
    load(),

    API = make_ref(),
    Name = "test",

    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),
    ok = meck:expect(swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], #{
        <<"code">> => 200
    }),

    State = ?MUT:init(Name),
    ok = assert_kubernerlnetes_loaded(),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    io:format("here!~n"),
    {ok, State} = ?MUT:store(Data, State),
    io:format("here!~n"),

    StoreOps = meck:capture(first, swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], 3),
    ConfigMapDoc = proplists:get_value(<<"body">>, StoreOps),

    ok = meck:expect(swaggerl, op, [API, "readCoreV1NamespacedConfigMap", '_'], ConfigMapDoc),

    {Features, State} = ?MUT:get_all(State),

    ?assertEqual(Data, Features),

    unload(),
    ok.

first_write_test() ->
    load(),
    API = make_ref(),
    Name = "test",

    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),
    ok = meck:expect(swaggerl, op, [
        {[API, "replaceCoreV1NamespacedConfigMap", '_'], #{<<"code">> => 404}},
        {[API, "createCoreV1NamespacedConfigMap", '_'], #{<<"code">> => 200}}
    ]),

    State = ?MUT:init(Name),
    ok = assert_kubernerlnetes_loaded(),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    io:format("here!~n"),
    {ok, State} = ?MUT:store(Data, State),
    io:format("here!~n"),

    ReplaceOps = meck:capture(
        first,
        swaggerl,
        op,
        [API, "replaceCoreV1NamespacedConfigMap", '_'],
        3
    ),
    CreateOps = meck:capture(first, swaggerl, op, [API, "createCoreV1NamespacedConfigMap", '_'], 3),
    ReplaceName = proplists:get_value(<<"name">>, ReplaceOps),
    ReplaceConfigMapDoc = proplists:get_value(<<"body">>, ReplaceOps),
    CreateConfigMapDoc = proplists:get_value(<<"body">>, CreateOps),

    ?assertEqual(<<"features-state-store">>, ReplaceName),
    ?assertEqual(ReplaceConfigMapDoc, CreateConfigMapDoc),

    unload(),
    ok.

push_to_namespaces_test() ->
    load(),

    API = make_ref(),
    Name = "test",
    Namespace = <<"test_namespace">>,

    ok = meck:expect(application, get_env, [
        {[features, namespaces], {ok, [Namespace]}},
        {[features, namespace], {ok, <<"getkimball">>}}
    ]),
    ok = meck:expect(kuberlnetes, load, ['_'], API),
    ok = meck:expect(swaggerl, operations, ['_'], []),
    ok = meck:expect(swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], #{
        <<"code">> => 200
    }),

    State = ?MUT:init(Name),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    {ok, State} = ?MUT:store(Data, State),

    StoreOps = meck:capture(last, swaggerl, op, [API, "replaceCoreV1NamespacedConfigMap", '_'], 3),
    ConfigMapNamespace = proplists:get_value(<<"namespace">>, StoreOps),
    ConfigMapName = proplists:get_value(<<"name">>, StoreOps),

    ?assertEqual(Namespace, ConfigMapNamespace),
    ?assertEqual(<<"getkimball-features">>, ConfigMapName),

    unload(),
    ok.

assert_kubernerlnetes_loaded() ->
    Operations = [
        <<"createCoreV1NamespacedConfigMap">>,
        <<"readCoreV1NamespacedConfigMap">>,
        <<"replaceCoreV1NamespacedConfigMap">>
    ],

    Expected = [{operations, Operations}],
    ?assertEqual(Expected, meck:capture(first, kuberlnetes, load, '_', 1)),
    ok.

load() ->
    ok = meck:new(application, [unstick]),
    ok = meck:new(kuberlnetes),
    ok = meck:new(swaggerl),

    ok = meck:expect(application, get_env, [
        {[features, namespaces], {ok, []}},
        {[features, namespace], {ok, <<"getkimball">>}}
    ]),
    ok.

unload() ->
    true = meck:validate(kuberlnetes),
    true = meck:validate(swaggerl),
    true = meck:validate(application),
    ok = meck:unload(kuberlnetes),
    ok = meck:unload(swaggerl),
    ok = meck:unload(application),
    ok.
