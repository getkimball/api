-module(features_handler_v0_features_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_features).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = cowboy_test_helpers:setup(),
    ok = meck:new(features_store),
    ok = meck:expect(features_store, get_features, fun() -> [] end),
    ok = meck:expect(features_store, set_feature, fun(_, _, _, _) -> ok end),

    ok = meck:new(application, [unstick]),
    ok = meck:expect(
        application,
        get_env,
        [features, analytics_event_mod],
        {ok, features_count_router}
    ),

    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, add, ['_', '_'], ok),

    ok = meck:new(features_count_relay),
    ok = meck:expect(features_count_relay, add, ['_', '_'], ok),

    ok.

unload(_) ->
    unload().

unload() ->
    ?assert(meck:validate(features_store)),
    ok = meck:unload(features_store),

    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ?assert(meck:validate(application)),
    ok = meck:unload(application),

    ?assert(meck:validate(features_count_relay)),
    ok = meck:unload(features_count_relay),

    ok = cowboy_test_helpers:cleanup(),
    ok.

general_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun handler_metadata/0,
        fun simple_get/0,
        fun post_fails/0
    ]}.

handler_metadata() ->
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/features", Path).

simple_get() ->
    Req = cowboy_test_helpers:req(),
    ExpectedCode = 200,
    ExpectedData = #{<<"features">> => #{}},

    ?CTH:http_get(
        ?MUT,
        #{analytics_event_mod => features_count_router},
        Req,
        ExpectedCode,
        ExpectedData
    ).

post_fails() ->
    Req = cowboy_test_helpers:req(post, json, #{}),
    ExpectedCode = 405,
    ExpectedData = #{
        <<"error">> => #{
            <<"method">> => <<"post">>,
            <<"what">> => <<"Method not allowed">>
        }
    },

    ?CTH:http_post(?MUT, Req, ExpectedCode, ExpectedData).

%%%%
%   Boolean Tests
%%%%

boolean_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_boolean_features/0,
        fun get_feature_boolean/0
    ]}.

get_boolean_features() ->
    FeatureName = <<"feature_foo">>,
    Features = [test_utils:defaulted_feature_spec(FeatureName, #{boolean => false})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    ExpectedData = #{<<"features">> => #{FeatureName => false}},

    Req = cowboy_test_helpers:req(),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, ExpectedData).

get_feature_boolean() ->
    Name = <<"feature_name">>,
    Boolean = true,

    ok = meck:expect(features_store, get_features, fun() ->
        [test_utils:defaulted_feature_spec(Name, #{boolean => Boolean})]
    end),

    ExpectedData = #{
        <<"features">> => #{
            Name => true
        }
    },

    GetReq = cowboy_test_helpers:req(),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, GetReq, 200, ExpectedData).

%%%%
%   Rollout Tests
%%%%

rollout_test_() ->
    {foreach, fun load/0, fun unload/1, [fun get_feature_rollout/0]}.

get_feature_rollout() ->
    Name = <<"feature_name">>,
    Now = erlang:system_time(seconds),
    Later = Now + 100,

    ok = meck:expect(features_store, get_features, fun() ->
        [
            test_utils:defaulted_feature_spec(
                Name,
                #{
                    rollout_start => Now,
                    rollout_end => Later
                }
            )
        ]
    end),

    GetReq = cowboy_test_helpers:req(),
    ExpectedData = #{
        <<"features">> => #{
            Name => false
        }
    },
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, GetReq, 200, ExpectedData).

%%%%
%   Get user features tests
%%%%

user_features_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_user_features_string/0,
        fun get_user_features_integer/0,
        fun get_user_features_membership_integer/0,
        fun get_user_features_membership_string/0,
        fun get_user_features_invalid_user_json/0,
        fun get_user_features_invalid_user_base64/0,
        fun get_user_features_invalid_context_json/0,
        fun get_user_features_invalid_context_base64/0
    ]}.

get_user_features_string() ->
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = '=',
    Value = <<"42">>,

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => Value},

    Features = [test_utils:defaulted_feature_spec(Name, #{user => UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),
    ExpectedData = #{<<"features">> => #{Name => true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, ExpectedData).

get_user_features_integer() ->
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = '=',
    Value = 42,

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => Value},

    Features = [test_utils:defaulted_feature_spec(Name, #{user => UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),
    ExpectedData = #{<<"features">> => #{Name => true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, ExpectedData).

get_user_features_membership_integer() ->
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = 'in',
    Value = [40, 41, 42],

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => 42},

    Features = [test_utils:defaulted_feature_spec(Name, #{user => UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),
    ExpectedData = #{<<"features">> => #{Name => true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, ExpectedData).

get_user_features_membership_string() ->
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = 'in',
    Value = [<<"40">>, <<"41">>, <<"42">>],

    UserSpec = [[UserProp, Comparator, Value]],

    UserObj = #{UserProp => <<"42">>},

    Features = [test_utils:defaulted_feature_spec(Name, #{user => UserSpec})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    UserQuery = base64:encode(jsx:encode(UserObj)),
    ExpectedData = #{<<"features">> => #{Name => true}},

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, ExpectedData).

get_user_features_invalid_user_json() ->
    UserQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{
        <<"error">> => #{
            <<"what">> => Msg,
            <<"object">> => <<"user_obj">>
        }
    },
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 400, Expected).

get_user_features_invalid_user_base64() ->
    UserQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{
        <<"error">> => #{
            <<"what">> => Msg,
            <<"object">> => UserQuery
        }
    },
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 400, Expected).

get_user_features_invalid_context_json() ->
    ContextQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{
        <<"error">> => #{
            <<"what">> => Msg,
            <<"object">> => <<"context_obj">>
        }
    },
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 400, Expected).

get_user_features_invalid_context_base64() ->
    ContextQuery = <<"b'badb64">>,

    Req = cowboy_test_helpers:req(get, [{<<"context_obj">>, ContextQuery}]),

    Msg = <<"The object cannot be base64 decoded">>,
    Expected = #{
        <<"error">> => #{
            <<"what">> => Msg,
            <<"object">> => ContextQuery
        }
    },
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 400, Expected).

%%%%
%   Record context tests
%%%%

features_api_mode_test_() ->
    {foreach, fun load/0, fun unload/1, [fun get_features_api_mode_with_context_and_user/0]}.

get_features_api_mode_with_context_and_user() ->
    UserIDProp = <<"user_id">>,
    UserID = 42,
    FeatureName = <<"feature_name">>,
    UserObj = #{UserIDProp => UserID},
    UserQuery = base64:encode(jsx:encode(UserObj)),

    ContextQuery = base64:encode(jsx:encode(#{feature => FeatureName})),

    Req = cowboy_test_helpers:req(get, [
        {<<"context_obj">>, ContextQuery},
        {<<"user_obj">>, UserQuery}
    ]),

    Expected = #{<<"features">> => #{}},
    ?CTH:http_get(?MUT, #{analytics_event_mod => features_count_router}, Req, 200, Expected),

    ?assertEqual(FeatureName, meck:capture(first, features_count_router, add, '_', 1)),
    ?assertEqual(UserID, meck:capture(first, features_count_router, add, '_', 2)).
