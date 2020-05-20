-module(features_store_lib_file_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_store_lib_file).


init_test() ->
    load(),
    ok = meck:expect(application, get_env, [features, file_store_path], {ok, []}),

    _State = ?MUT:init(),

    unload(),
    ok.

read_test() ->
    load(),
    Path = <<"/test_path">>,
    ok = meck:expect(application, get_env, [features, file_store_path], {ok, Path}),

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = jsx:encode(Data),
    ok = meck:expect(file, read_file, ['_'], {ok, DataBin}),

    % API = make_ref(),
    State = ?MUT:init(),
    {Features, State} = ?MUT:get_all(State),

    ReadFilePath = meck:capture(first, file, read_file, ['_'], 1),

    ?assertEqual(Path, ReadFilePath),
    ?assertEqual(Data, Features),

    unload(),
    ok.

store_test() ->
    load(),
    ok = meck:expect(application, get_env, [features, file_store_path], {ok, []}),

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = jsx:encode(Data),
    ok = meck:expect(file, read_file, ['_'], {ok, DataBin}),

    % API = make_ref(),
    State = ?MUT:init(),

    Resp = ?MUT:store(Data, State),

    ?assertEqual({not_suported, State}, Resp),

    unload(),
    ok.

load() ->
    ok = meck:new(file, [unstick]),
    ok = meck:new(application, [unstick]),
    ok.

unload() ->
    true = meck:validate(file),
    true = meck:validate(application),
    ok = meck:unload(file),
    ok = meck:unload(application),
    ok.
