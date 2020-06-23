-module(features_handler_v0_features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_features).

load() ->
    ok = cowboy_test_helpers:setup(),
    ok = meck:new(features_store),
    ok = meck:expect(features_store, get_features, fun() -> [] end),
    ok = meck:expect(features_store, set_feature, fun(_, _, _, _) -> ok end),
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
    {response, Code, Headers, Body} = cowboy_test_helpers:read_reply(CowResp),
    Data = jsx:decode(Body, [return_maps]),

    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, Code),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, Headers, Data),

    ?assertEqual(200, Code),
    ?assertEqual(#{<<"features">> => #{}}, Data),
    unload().

%%%%
%   Boolean Tests
%%%%


get_boolean_features_test() ->
    load(),
    FeatureName = <<"feature_foo">>,
    Features = [test_utils:defaulted_feature_spec(FeatureName, #{boolean=>false})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    Req = cowboy_test_helpers:req(),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{FeatureName=>false}}, Data),

    unload().

get_feature_boolean_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,

    ok = meck:expect(features_store, get_features, fun() ->
            [test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean})]
    end),


    GetReq = cowboy_test_helpers:req(),
    Data = http_get(GetReq, 200),

    ExpectedData = #{
        <<"features">> => #{
            Name => true
    }},
    ?assertEqual(ExpectedData, Data),

    unload().

%%%%
%   Rollout Tests
%%%%

get_feature_rollout_test() ->
    load(),
    Name = <<"feature_name">>,
    Now = erlang:system_time(seconds),
    Later = Now + 100,

    ok = meck:expect(features_store, get_features, fun() ->
            [test_utils:defaulted_feature_spec(Name,
                #{rollout_start => Now,
                  rollout_end => Later})]
    end),

    GetReq = cowboy_test_helpers:req(),
    Data = http_get(GetReq, 200),
    ExpectedData = #{
        <<"features">> => #{
            Name => false
    }},
    ?assertEqual(ExpectedData, Data),

    unload().


%%%%
%   Get user features tests
%%%%

get_user_features_string_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = '=',
    Value = <<"42">>,

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => Value},

    Features = [test_utils:defaulted_feature_spec(Name, #{user=>UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{Name=>true}}, Data),
    unload().

get_user_features_integer_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = '=',
    Value = 42,

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => Value},

    Features = [test_utils:defaulted_feature_spec(Name, #{user=>UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{Name=>true}}, Data),
    unload().

get_user_features_membership_integer_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = 'in',
    Value = [40, 41, 42],

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => 42},

    Features = [test_utils:defaulted_feature_spec(Name, #{user=>UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{Name=>true}}, Data),
    unload().

get_user_features_membership_string_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = 'in',
    Value = [<<"40">>, <<"41">>, <<"42">>],

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => <<"42">>},

    Features = [test_utils:defaulted_feature_spec(Name, #{user=>UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{Name=>true}}, Data),
    unload().

get_user_features_invalid_user_json_test() ->
    load(),
    UserQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 400),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => <<"user_obj">>}},
    ?assertEqual(Expected, Data),
    unload().

get_user_features_invalid_user_base64_test() ->
    load(),
    UserQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 400),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => UserQuery}},
    ?assertEqual(Expected, Data),
    unload().

get_user_features_invalid_context_json_test() ->
    load(),
    ContextQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),
    Data = http_get(Req, 400),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => <<"context_obj">>}},
    ?assertEqual(Expected, Data),
    unload().

get_user_features_invalid_context_base64_test() ->
    load(),
    ContextQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),
    Data = http_get(Req, 400),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => ContextQuery}},
    ?assertEqual(Expected, Data),

    unload().

%%%%
%   Test helpers
%%%%

http_get(Req, ExpectedCode) ->
    CowGetResp = cowboy_test_helpers:init(?MUT, Req, []),
    {response, GetCode, GetHeaders, GetBody} = cowboy_test_helpers:read_reply(CowGetResp),
    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, GetCode),

    ?assertEqual(ExpectedCode, GetCode),
    Data = jsx:decode(GetBody, [return_maps]),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, GetHeaders, Data),
    Data.

