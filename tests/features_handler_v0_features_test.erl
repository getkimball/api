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
    Features = [test_utils:defaulted_feature_spec(FeatureName, #{boolean=>false})],
    ok = meck:expect(features_store, get_features, fun() -> Features end),

    Req = cowboy_test_helpers:req(),
    Data = http_get(Req, 200),
    ?assertEqual(#{<<"features">>=>#{FeatureName=>false}}, Data),

    unload().

create_feature_boolean_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,
    Doc = #{
        name => Name,
        boolean => Boolean
    },

    ok = meck:expect(features_store, get_features, fun() ->
            [test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean})]
    end),
    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Opts = [],

    CowPostResp = cowboy_test_helpers:init(?MUT, PostReq, Opts),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),

    ?assertEqual(204, PostCode),
    ?assertEqual(<<"{}">>, PostBody),

    GetReq = cowboy_test_helpers:req(),
    Data = http_get(GetReq, 200),

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
            [test_utils:defaulted_feature_spec(Name,
                #{rollout_start => Now,
                  rollout_end => Later})]
    end),
    PostReq = cowboy_test_helpers:req(post, json, Doc),
    PostBody = http_post(PostReq, 204),
    ?assertEqual({rollout, Now, Later},
                 meck:capture(first, features_store, set_feature, '_', 3)),

    ?assertEqual(#{}, PostBody),

    GetReq = cowboy_test_helpers:req(),
    Data = http_get(GetReq, 200),
    ExpectedData = #{
        <<"features">> => #{
            Name => false
    }},
    ?assertEqual(ExpectedData, Data),

    unload().


create_feature_invalid_json_test() ->
    load(),
    Data = <<"{:not valid json">>,

    PostReq = cowboy_test_helpers:req(post, binary, Data),
    Body = http_post(PostReq, 400),
    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{
                    <<"what">> => Msg,
                    <<"object">> => <<"post_body">>}},
    ?assertEqual(Expected, Body),

    unload().

create_feature_missing_required_name_test() ->
    load(),
    Boolean = true,
    Doc = #{
        boolean => Boolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Body = http_post(PostReq, 400),

    Expected = #{<<"error">> => #{
                        <<"key">> => <<"name">>,
                        <<"what">> => <<"Missing required element">>}},
    ?assertEqual(Expected, Body),

    unload().

create_feature_incorrect_boolean_type_test() ->
    load(),
    Name = <<"feature_name">>,
    BadBoolean = <<"true">>,
    Doc = #{
        name => Name,
        boolean => BadBoolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Data = http_post(PostReq, 400),

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"boolean">>,
                             <<"value">> => <<"true">>,
                             <<"what">> => <<"Incorrect type">>}},
    ?assertEqual(ExpectedResponse, Data),

    unload().

create_feature_incorrect_string_type_test() ->
    load(),
    BadName = 4,
    Boolean = true,
    Doc = #{
        name => BadName,
        boolean => Boolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Data = http_post(PostReq, 400),

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"string">>,
                             <<"value">> => 4,
                             <<"what">> => <<"Incorrect type">>}},
    ?assertEqual(ExpectedResponse, Data),

    unload().

create_feature_rollout_missing_end_test() ->
    load(),
    Name = <<"feature_name">>,
    Now = erlang:system_time(seconds),
    Doc = #{
        name => Name,
        rollout_start => binary:list_to_bin(calendar:system_time_to_rfc3339(Now))
    },
    ErrorMessage = <<"Rollout start requires a rollout end">>,

    ok = meck:expect(features_store, set_feature, ['_', '_', '_', '_'],
                     meck:raise(throw, {invalid_feature, ErrorMessage})),

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Data = http_post(PostReq, 400),

    ExpectedResponse = #{<<"error">> =>
                           #{<<"what">> => <<"Invalid feature">>,
                             <<"description">> => ErrorMessage}},
    ?assertEqual(ExpectedResponse, Data),
    unload().

create_feature_rollout_invalid_date_format_test() ->
    load(),
    Doc = #{
        name => <<"feature_name">>,
        rollout_start => <<"2020">>
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Data = http_post(PostReq, 400),

    ErrorMessage = <<"Date doesn't appear to be the right format">>,
    ExpectedResponse = #{<<"error">> =>
                           #{<<"what">> => ErrorMessage,
                             <<"value">> => <<"2020">>}},
    ?assertEqual(ExpectedResponse, Data),
    unload().

%%%%
%   Create feature with user spec
%%%%

create_feature_user_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"=">>,
    Value = <<"42">>,

    Doc = #{
        name => Name,
        user => [#{property => UserProp,
                   comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    PostBody = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, '=', Value]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

    ?assertEqual(#{}, PostBody),

    unload().

create_feature_user_missing_required_property_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"invalid comparator">>,
    Value = <<"42">>,

    Doc = #{
        name => Name,
        user => [#{property=> UserProp,
                   comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Body = http_post(PostReq, 400),

    Expected = #{<<"error">> => #{
                        <<"value">> => Comparator,
                        <<"choices">> => [<<"=">>],
                        <<"what">> => <<"Value not in enum">>}},
    ?assertEqual(Expected, Body),

    unload().

create_feature_user_value_not_in_enum_test() ->
    load(),
    Name = <<"feature_name">>,

    Comparator = <<"=">>,
    Value = <<"42">>,

    Doc = #{
        name => Name,
        user => [#{comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    Body = http_post(PostReq, 400),

    Expected = #{<<"error">> => #{
                        <<"key">> => <<"property">>,
                        <<"what">> => <<"Missing required element">>}},
    ?assertEqual(Expected, Body),

    unload().

%%%%
%   Get user features tests
%%%%

get_user_features_test() ->
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

get_user_features_invalid_json_test() ->
    load(),
    UserQuery = base64:encode(<<"{ not valid json ]">>),

    Req = cowboy_test_helpers:req(get, [{<<"user_obj">>, UserQuery}]),
    Data = http_get(Req, 400),

    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{<<"what">> => Msg,
                                  <<"object">> => <<"user_obj">>}},
    ?assertEqual(Expected, Data),

    unload().


%%%%
%   Test helpers
%%%%

http_get(Req, ExpectedCode) ->
    CowGetResp = cowboy_test_helpers:init(?MUT, Req, []),
    {response, GetCode, _GetHeaders, GetBody} = cowboy_test_helpers:read_reply(CowGetResp),
    GetSpec = swagger_specified_handler:response_spec(?MUT, <<"get">>, GetCode),

    ?assertEqual(ExpectedCode, GetCode),
    Data = jsx:decode(GetBody, [return_maps]),
    ok = cowboy_test_helpers:validate_response_against_spec(GetSpec, Data),
    Data.

http_post(Req, ExpectedCode) ->
    CowPostResp = cowboy_test_helpers:init(?MUT, Req, []),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data = jsx:decode(PostBody, [return_maps]),

    ?assertEqual(ExpectedCode, PostCode),
    Data.
