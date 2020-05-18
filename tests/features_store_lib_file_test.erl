-module(features_store_lib_file_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_store_lib_file).


init_test() ->
    load(),

    _State = ?MUT:init(),

    unload(),
    ok.

read_test() ->
    load(),

    Data = [#{<<"name">>=><<"name">>, <<"status">>=><<"status">> }],
    DataBin = jsx:encode(Data),
    ok = meck:expect(file, read_file, ['_'], {ok, DataBin}),

    % API = make_ref(),
    State = ?MUT:init(),

    {Features, State} = ?MUT:get_all(State),

    ?assertEqual(Data, Features),

    unload(),
    ok.


load() ->
    ok = meck:new(file, [unstick]),
    ok.

unload() ->
    true = meck:validate(file),
    ok = meck:unload(file),
    ok.
