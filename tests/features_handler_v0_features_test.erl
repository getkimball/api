-module(features_handler_v0_features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_features).

setup_test() ->
    [Trail] = ?MUT:trails(),
    #{ path_match := Path } = Trail,
    ?assertEqual("/v0/features", Path),
    ok.

ok_test() ->
    ok = cowboy_test_helpers:setup(),
    ok = meck:new(features_store),
    ok = meck:expect(features_store, get_binary_features, fun() -> [] end),

    Req = cowboy_test_helpers:req(),
    Opts = [],

    CowResp = cowboy_test_helpers:init(?MUT, Req, Opts),
    {response, Code, _Headers, Body} = cowboy_test_helpers:read_reply(CowResp),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(200, Code),
    ?assertEqual(#{<<"features">> => []}, Data),

    ok = meck:unload(features_store),
    ok = cowboy_test_helpers:cleanup(),

    ok.


create_feature_test() ->
    Name = <<"feature_name">>,
    Enabled = true,
    Doc = #{
        name => Name,
        enabled => Enabled
    },

    ok = meck:new(features_store),
    ok = meck:expect(features_store, set_binary_feature, fun(_, _) -> ok end),
    ok = cowboy_test_helpers:setup(),
    ok = meck:expect(features_store, get_binary_features, fun() ->
            #{Name => Enabled}
    end),
    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),

    ?assertEqual(204, PostCode),
    ?assertEqual(<<"{}">>, PostBody),

    GetReq = cowboy_test_helpers:req(),
    CowGetResp = cowboy_test_helpers:init(?MUT, GetReq, Opts),
    {response, GetCode, _GetHeaders, GetBody} = cowboy_test_helpers:read_reply(CowGetResp),
    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, GetCode),

    ?assertEqual(200, GetCode),
    Data = jsx:decode(GetBody, [return_maps]),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, Data),

    ExpectedData = #{
        <<"features">> => #{
            Name => true
    }},
    ?assertEqual(ExpectedData, Data),

    ?assert(meck:validate(features_store)),
    ok = meck:unload(features_store),
    ok = cowboy_test_helpers:cleanup(),
    ok.
