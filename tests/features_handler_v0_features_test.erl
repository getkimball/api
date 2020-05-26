-module(features_handler_v0_features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_features).

load() ->
    ok = cowboy_test_helpers:setup(),
    ok = meck:new(features_store),
    ok = meck:expect(features_store, get_features, fun() -> #{} end),
    ok = meck:expect(features_store, set_feature, fun(_, boolean, _) -> ok end),
    ok = meck:expect(features_store, set_feature, fun(_, rollout, _, _) -> ok end),
    ok.

unload() ->
    ?assert(meck:validate(features_store)),
    ok = meck:unload(features_store),
    ok = cowboy_test_helpers:cleanup(),
    ok.

setup_test() ->
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/features", Path),
    ok.

ok_test() ->
    load(),

    Req = cowboy_test_helpers:req(),
    Opts = [],

    CowResp = cowboy_test_helpers:init(?MUT, Req, Opts),
    {response, Code, _Headers, Body} = cowboy_test_helpers:read_reply(CowResp),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(200, Code),
    ?assertEqual(#{<<"features">> => #{}}, Data),
    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, Code),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, Data),

    unload().

get_boolean_features_test() ->
    load(),
    FeatureName = <<"feature_foo">>,
    Features = #{
        FeatureName => #{
            boolean => false,
            rollout_start => undefined,
            rollout_end => undefined
    }},

    ok = meck:expect(features_store, get_features, fun() -> Features end),

    Req = cowboy_test_helpers:req(),
    Opts = [],

    CowResp = cowboy_test_helpers:init(?MUT, Req, Opts),
    {response, Code, _Headers, Body} = cowboy_test_helpers:read_reply(CowResp),
    Data = jsx:decode(Body, [return_maps]),

    ?assertEqual(200, Code),
    ?assertEqual(#{<<"features">>=>#{FeatureName=>false}}, Data),
    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, Code),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, Data),

    unload().

create_feature_boolean_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,
    Doc = #{
        name => Name,
        enabled => Boolean
    },

    ok = meck:expect(features_store, get_features, fun() ->
            #{Name => test_utils:defaulted_feature_spec(#{boolean=>Boolean})}
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

    unload().

create_feature_rollout_test() ->
    load(),
    Name = <<"feature_name">>,
    Now = erlang:system_time(seconds),
    Later = Now + 100,
    Doc = #{
        name => Name,
        rollout_start => binary:list_to_bin(calendar:system_time_to_rfc3339(Now)),
        rollout_end => binary:list_to_bin(calendar:system_time_to_rfc3339(Later))
    },

    ok = meck:expect(features_store, get_features, fun() ->
            #{Name => test_utils:defaulted_feature_spec(
                #{rollout_start => Now,
                  rollout_end => Later})}
    end),
    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),

    ?assertEqual(Now, meck:capture(first, features_store, set_feature, '_', 3)),
    ?assertEqual(Later, meck:capture(first, features_store, set_feature, '_', 4)),

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
            Name => false
    }},
    ?assertEqual(ExpectedData, Data),

    unload().

create_feature_missing_required_name_test() ->
    cowboy_test_helpers:setup(),
    Boolean = true,
    Doc = #{
        enabled => Boolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, _PostBody} = cowboy_test_helpers:read_reply(CowPostResp),

    ?assertEqual(400, PostCode),


    ok = cowboy_test_helpers:cleanup(),
    ok.

create_feature_incorrect_boolean_type_test() ->
    cowboy_test_helpers:setup(),
    Name = <<"feature_name">>,
    BadBoolean = <<"true">>,
    Doc = #{
        name => Name,
        enabled => BadBoolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data = jsx:decode(PostBody, [return_maps]),

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"boolean">>,
                             <<"value">> => <<"true">>,
                             <<"what">> => "Incorrect type"}},
    ?assertEqual(400, PostCode),
    ?assertEqual(ExpectedResponse, Data),


    ok = cowboy_test_helpers:cleanup(),
    ok.

create_feature_incorrect_string_type_test() ->
    cowboy_test_helpers:setup(),
    BadName = 4,
    Boolean = true,
    Doc = #{
        name => BadName,
        enabled => Boolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data = jsx:decode(PostBody, [return_maps]),

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"string">>,
                             <<"value">> => 4,
                             <<"what">> => "Incorrect type"}},
    ?assertEqual(400, PostCode),
    ?assertEqual(ExpectedResponse, Data),


    ok = cowboy_test_helpers:cleanup(),
    ok.
