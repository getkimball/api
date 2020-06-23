-module(features_handler_v0_features_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_features).
-define(CTH, cowboy_test_helpers).

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
    ExpectedCode = 200,
    ExpectedData = #{<<"features">> => #{}},

    ?CTH:http_get(?MUT, Req, ExpectedCode, ExpectedData),

    unload().

%%%%
%   Boolean Tests
%%%%


get_boolean_features_test() ->
    load(),
    FeatureName = <<"feature_foo">>,
    Features = [test_utils:defaulted_feature_spec(FeatureName, #{boolean=>false})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    ExpectedData = #{<<"features">>=>#{FeatureName=>false}},

    Req = cowboy_test_helpers:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

    unload().

get_feature_boolean_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,

    ok = meck:expect(features_store, get_features, fun() ->
            [test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean})]
    end),

    ExpectedData = #{
        <<"features">> => #{
            Name => true
    }},

    GetReq = cowboy_test_helpers:req(),
    ?CTH:http_get(?MUT, GetReq, 200, ExpectedData),

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
    ExpectedData = #{
        <<"features">> => #{
            Name => false
    }},
    ?CTH:http_get(?MUT, GetReq, 200, ExpectedData),

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
    ExpectedData = #{<<"features">>=>#{Name=>true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),
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
    ExpectedData = #{<<"features">>=>#{Name=>true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),
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
    ExpectedData = #{<<"features">>=>#{Name=>true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),
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
    ExpectedData = #{<<"features">>=>#{Name=>true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),
    unload().

get_user_features_invalid_user_json_test() ->
    load(),
    UserQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => <<"user_obj">>}},
    ?CTH:http_get(?MUT, Req, 400, Expected),
    unload().

get_user_features_invalid_user_base64_test() ->
    load(),
    UserQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => UserQuery}},
    ?CTH:http_get(?MUT, Req, 400, Expected),
    unload().

get_user_features_invalid_context_json_test() ->
    load(),
    ContextQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => <<"context_obj">>}},
    ?CTH:http_get(?MUT, Req, 400, Expected),
    unload().

get_user_features_invalid_context_base64_test() ->
    load(),
    ContextQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => ContextQuery}},
    ?CTH:http_get(?MUT, Req, 400, Expected),

    unload().
