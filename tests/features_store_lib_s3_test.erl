-module(features_store_lib_s3_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include("../include/counter_names.hrl").

-define(MUT, features_store_lib_s3).
-define(BUCKET, "test_bucket").
-define(BASE_PATH, "test_base_path").
-define(AWS_CONFIG, #aws_config{}).

init_test_() ->
    {foreach,
     fun load/0,
     fun unload/1,
     [fun basic_init/0,
      fun init_custom_s3/0]}.

basic_init() ->
    _State = ?MUT:init("test").

init_custom_s3() ->
    CustomS3Host = "test.host.example.com",
    ok = meck:expect(application, get_env, [{[features, s3_bucket], {ok, ?BUCKET}},
                                            {[features, s3_base_path], {ok, ?BASE_PATH}},
                                            {[features, s3_host], {ok, CustomS3Host}}]),

    State = ?MUT:init("test"),
    AWSConfig = ?MUT:aws_config(State),
    io:format("record ~p~n", [State]),
    ?assertEqual(CustomS3Host, AWSConfig#aws_config.s3_host).

read_test() ->
    load(),
    Name = "test",
    ExpectedPath = ?BASE_PATH ++ "/" ++ Name,

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = erlang:term_to_binary(Data),
    Obj = [{content, DataBin}],
    ok = meck:expect(erlcloud_s3, get_object, ['_', '_', '_'], Obj),

    State = ?MUT:init(Name),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 2)),

    ?assertEqual(Data, ReturnedData),

    unload().

read_counter_name_weekly_test() ->
    load(),
    Name = "test",
    CNW = #counter_name_weekly{name=Name, year=2020, week=1},
    ExpectedPath = ?BASE_PATH ++ "/" ++ Name ++ "/2020/1",

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = erlang:term_to_binary(Data),
    Obj = [{content, DataBin}],
    ok = meck:expect(erlcloud_s3, get_object, ['_', '_', '_'], Obj),

    State = ?MUT:init(CNW),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 2)),

    ?assertEqual(Data, ReturnedData),

    unload().

read_with_type_test() ->
    load(),
    Name = "test",
    Type = "type",
    ExpectedPath = ?BASE_PATH ++ "/" ++ Type ++ "/" ++ Name,

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = erlang:term_to_binary(Data),
    Obj = [{content, DataBin}],
    ok = meck:expect(erlcloud_s3, get_object, ['_', '_', '_'], Obj),

    State = ?MUT:init({Type, Name}),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 2)),

    ?assertEqual(Data, ReturnedData),

    unload().

read_counter_name_weekly_with_type_test() ->
    load(),
    Name = "test",
    Type = "type",
    CNW = #counter_name_weekly{name=Name, year=2020, week=1},
    ExpectedPath = ?BASE_PATH ++ "/" ++ Type ++ "/" ++ Name ++ "/2020/1",

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = erlang:term_to_binary(Data),
    Obj = [{content, DataBin}],
    ok = meck:expect(erlcloud_s3, get_object, ['_', '_', '_'], Obj),

    State = ?MUT:init({Type, CNW}),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 2)),

    ?assertEqual(Data, ReturnedData),

    unload().

read_404_test() ->
    load(),
    Name = "test",
    ExpectedPath = ?BASE_PATH ++ "/" ++ Name,

    Error = {aws_error, {http_error, 404, "TestMSG", "TestDoc"}},
    ok = meck:expect(erlcloud_s3, get_object, ['_', '_', '_'], meck:raise(error, Error)),

    State = ?MUT:init(Name),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, get_object, ['_', '_', '_'], 2)),

    ?assertEqual(#{}, ReturnedData),

    unload().

store_test() ->
    load(),
    Name = "test",
    ExpectedPath = ?BASE_PATH ++ "/" ++ Name,

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    ok = meck:expect(erlcloud_s3, put_object,  ['_', '_', '_', '_'], {}),

    % API = make_ref(),
    State = ?MUT:init("test"),

    {ok, _State} = ?MUT:store(Data, State),


    ?assertEqual(?BUCKET, meck:capture(first, erlcloud_s3, put_object, ['_', '_', '_', '_'], 1)),
    ?assertEqual(ExpectedPath, meck:capture(first, erlcloud_s3, put_object, ['_', '_', '_', '_'], 2)),

    StoredData = meck:capture(first, erlcloud_s3, put_object, ['_', '_', '_', '_'], 3),
    DecodedStoredData = erlang:binary_to_term(StoredData),
    ?assertEqual(Data, DecodedStoredData),


    unload(),
    ok.

load() ->
    ok = meck:new(erlcloud_aws),
    ok = meck:new(erlcloud_s3),
    ok = meck:new(application, [unstick]),

    ok = meck:expect(erlcloud_aws, auto_config, [], {ok, ?AWS_CONFIG}),

    ok = meck:expect(application, get_env, [{[features, s3_bucket], {ok, ?BUCKET}},
                                            {[features, s3_base_path], {ok, ?BASE_PATH}},
                                            {[features, s3_host], {ok, ""}}]),
    ok.

unload() ->
    true = meck:validate(erlcloud_aws),
    true = meck:validate(erlcloud_s3),
    true = meck:validate(application),
    ok = meck:unload(erlcloud_aws),
    ok = meck:unload(erlcloud_s3),
    ok = meck:unload(application),
    ok.

unload(_) ->
    unload().
