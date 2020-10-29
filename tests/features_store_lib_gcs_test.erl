-module(features_store_lib_gcs_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("enenra/include/enenra.hrl").

-define(MUT, features_store_lib_gcs).
-define(BUCKET, <<"test_bucket">>).
-define(BASE_PATH, "test_base_path").
-define(CREDENTIALS_PATH, "test_credentials_path").
-define(ENENRA_CREDENTIALS, "test_enenra_credentials").

init_test() ->
    load(),
    _State = ?MUT:init(<<"test">>),
    unload().

read_test() ->
    load(),
    Name = <<"test">>,
    ExpectedPath = binary:list_to_bin(?BASE_PATH ++ "/" ++ binary_to_list(Name)),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    DataBin = erlang:term_to_binary(Data),
    ok = meck:expect(enenra, get_object_contents, ['_', '_', '_'], {ok, DataBin}),

    State = ?MUT:init(Name),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(
        1,
        meck:num_calls(enenra, get_object_contents, [?BUCKET, ExpectedPath, ?ENENRA_CREDENTIALS])
    ),
    ?assertEqual(Data, ReturnedData),

    unload().

read_with_type_test() ->
    load(),
    Name = <<"test">>,
    Type = "type",
    ExpectedPath = binary:list_to_bin(?BASE_PATH ++ "/" ++ Type ++ "/" ++ binary_to_list(Name)),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    DataBin = erlang:term_to_binary(Data),
    ok = meck:expect(enenra, get_object_contents, ['_', '_', '_'], {ok, DataBin}),

    State = ?MUT:init({Type, Name}),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(
        1,
        meck:num_calls(enenra, get_object_contents, [?BUCKET, ExpectedPath, ?ENENRA_CREDENTIALS])
    ),
    ?assertEqual(Data, ReturnedData),

    unload().

read_404_test() ->
    load(),
    Name = <<"test">>,
    ExpectedPath = binary:list_to_bin(?BASE_PATH ++ "/" ++ binary_to_list(Name)),

    ok = meck:expect(enenra, get_object_contents, ['_', '_', '_'], {error, not_found}),

    State = ?MUT:init(Name),
    {ReturnedData, State} = ?MUT:get_all(State),

    ?assertEqual(
        1,
        meck:num_calls(enenra, get_object_contents, [?BUCKET, ExpectedPath, ?ENENRA_CREDENTIALS])
    ),

    ?assertEqual(#{}, ReturnedData),

    unload().

read_unknown_error_test() ->
    load(),
    Name = <<"test">>,

    ok = meck:expect(enenra, get_object_contents, ['_', '_', '_'], {error, other}),
    Throw = {enenra, other},
    State = ?MUT:init(Name),
    ?assertThrow(Throw, ?MUT:get_all(State)),

    unload().

store_test() ->
    load(),
    Name = <<"test">>,
    ExpectedPath = binary:list_to_bin(?BASE_PATH ++ "/" ++ binary_to_list(Name)),

    Data = [#{<<"name">> => <<"name">>, <<"status">> => <<"status">>}],
    EncodedData = erlang:term_to_binary(Data),
    Hash = base64:encode(erlang:md5(EncodedData)),

    EnenraObj = #object{
        bucket = ?BUCKET,
        contentType = <<"application/erlang">>,
        id = ExpectedPath,
        name = ExpectedPath,
        md5Hash = Hash,
        size = size(EncodedData),
        storageClass = <<"STANDARD">>,
        timeCreated = <<"1970-01-01:00:00.00Z">>,
        updated = <<"1970-01-01:00:00.00Z">>
    },

    State = ?MUT:init(<<"test">>),
    {ok, _State} = ?MUT:store(Data, State),

    ?assertEqual(
        1,
        meck:num_calls(enenra, upload_data, [EncodedData, EnenraObj, ?ENENRA_CREDENTIALS])
    ),
    unload().

load() ->
    ok = meck:new(enenra),
    ok = meck:new(application, [unstick]),

    ok = meck:expect(enenra, load_credentials, ['_'], {ok, ?ENENRA_CREDENTIALS}),
    ok = meck:expect(enenra, upload_data, ['_', '_', '_'], {ok, ?ENENRA_CREDENTIALS}),

    ok = meck:expect(application, get_env, [
        {[features, gcs_bucket], {ok, ?BUCKET}},
        {[features, gcs_base_path], {ok, ?BASE_PATH}},
        {[features, gcs_credentials_path], {ok, ?CREDENTIALS_PATH}}
    ]),
    ok.

unload() ->
    true = meck:validate(enenra),
    true = meck:validate(application),
    ok = meck:unload(enenra),
    ok = meck:unload(application),
    ok.
