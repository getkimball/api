-module(features_handler_v0_feature_specs_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_feature_specs).
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
    ?assertEqual("/v0/featureSpecs", Path),
    ok.

create_feature_invalid_content_type_test() ->
    load(),
    ContentType = <<"invalid">>,
    RequestOpts = #{
        has_body => true,
        headers => #{
            <<"content-type">> => ContentType
        }
    },

    PostReq = cowboy_test_helpers:req(post, raw, RequestOpts),
    Msg = <<"The content-type is invalid">>,
    Expected = #{<<"error">> => #{
                    <<"what">> => Msg,
                    <<"expected_types">> => [<<"application/json">>],
                    <<"type">> => ContentType}},
    http_post(PostReq, 400, Expected),

    unload().

create_feature_invalid_json_test() ->
    load(),
    Data = <<"{:not valid json">>,

    PostReq = cowboy_test_helpers:req(post, binary, Data),
    Msg = <<"The object is not valid JSON">>,
    Expected = #{<<"error">> => #{
                    <<"what">> => Msg,
                    <<"object">> => <<"post_body">>}},
    http_post(PostReq, 400, Expected),

    unload().

create_feature_missing_required_name_test() ->
    load(),
    Boolean = true,
    Doc = #{
        boolean => Boolean
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),

    Expected = #{<<"error">> => #{
                        <<"key">> => <<"name">>,
                        <<"what">> => <<"Missing required element">>}},
    http_post(PostReq, 400, Expected),

    unload().

%%%%
%   Boolean Tests
%%%%

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
    ?assertEqual(<<"">>, PostBody),

    unload().

get_feature_boolean_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,
    FeatureSpec = test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean}),
    ok = meck:expect(features_store, get_features, [], [FeatureSpec]),
    Req = cowboy_test_helpers:req(),

    Expected = ?CTH:json_roundtrip(#{<<"featureSpecs">> => [
        #{<<"name">> => Name,
          <<"boolean">> => Boolean,
          <<"user">> => []}]}),

    ok = ?CTH:http_get(?MUT, Req, 200, Expected),

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

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"boolean">>,
                             <<"value">> => <<"true">>,
                             <<"what">> => <<"Incorrect type">>}},
    http_post(PostReq, 400, ExpectedResponse),

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

    ExpectedResponse = #{<<"error">> =>
                           #{<<"type_expected">> => <<"string">>,
                             <<"value">> => 4,
                             <<"what">> => <<"Incorrect type">>}},
    http_post(PostReq, 400, ExpectedResponse),

    unload().

%%%%
%   Rollout Tests
%%%%

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

    ok = http_post(PostReq, 204),
    ?assertEqual({rollout, Now, Later},
                 meck:capture(first, features_store, set_feature, '_', 3)),

    unload().

get_feature_rollout_test() ->
    load(),
    Name = <<"feature_name">>,
    Boolean = true,
    Now = erlang:system_time(seconds),
    Later = Now + 100,
    RolloutStart = binary:list_to_bin(calendar:system_time_to_rfc3339(Now, [{offset, "z"}])),
    RolloutEnd = binary:list_to_bin(calendar:system_time_to_rfc3339(Later, [{offset, "z"}])),

    InternalFeatureSpec = test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean,
                                                                    rollout_start=>Now,
                                                                    rollout_end=>Later}),
    ExternalFeatureSpec = test_utils:defaulted_feature_spec(Name, #{boolean=>Boolean,
                                                                    rollout_start=>RolloutStart,
                                                                    rollout_end=>RolloutEnd}),
    ok = meck:expect(features_store, get_features, [], [InternalFeatureSpec]),
    Req = cowboy_test_helpers:req(),

    Expected = ?CTH:json_roundtrip(#{<<"featureSpecs">> => [ExternalFeatureSpec]}),

    ok = ?CTH:http_get(?MUT, Req, 200, Expected),

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

    ExpectedResponse = #{<<"error">> =>
                           #{<<"what">> => <<"Invalid feature">>,
                             <<"description">> => ErrorMessage}},
    http_post(PostReq, 400, ExpectedResponse),
    unload().

create_feature_rollout_invalid_date_format_test() ->
    load(),
    Doc = #{
        name => <<"feature_name">>,
        rollout_start => <<"2020">>
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),

    ErrorMessage = <<"Date doesn't appear to be the right format">>,
    ExpectedResponse = #{<<"error">> =>
                           #{<<"what">> => ErrorMessage,
                             <<"value">> => <<"2020">>}},
    http_post(PostReq, 400, ExpectedResponse),
    unload().

%%%%
%   User Tests
%%%%

create_feature_user_multiple_conditions_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"=">>,
    Value1 = <<"42">>,
    Value2 = <<"42">>,

    Doc = #{
        name => Name,
        user => [#{property => UserProp,
                   comparator => Comparator,
                   value => Value1},
                 #{property => UserProp,
                   comparator => Comparator,
                   value => Value2}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    ok = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, '=', Value1],
                         [UserProp, '=', Value2]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

    unload().

create_feature_user_string_test() ->
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
    ok = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, '=', Value]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

    unload().

get_feature_user_string_test() ->
    load(),
    Name = <<"feature_name">>,
    UserProp = <<"user_id">>,
    Comparator = '=',
    Value = <<"42">>,

    UserSpec = [[UserProp, Comparator, Value]],

    FeatureSpec = test_utils:defaulted_feature_spec(Name, #{user=>UserSpec}),
    ok = meck:expect(features_store, get_features, [], [FeatureSpec]),
    Req = cowboy_test_helpers:req(),

    ExpectedUserSpec = [#{
        <<"property">> => UserProp,
        <<"comparator">> => <<"=">>,
        <<"value">> => Value
    }],

    Expected = ?CTH:json_roundtrip(#{<<"featureSpecs">> => [#{
        <<"name">> => Name,
        <<"boolean">> => false,
        <<"user">> => ExpectedUserSpec}]}),

    ok = ?CTH:http_get(?MUT, Req, 200, Expected),

    unload().

create_feature_user_integer_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"=">>,
    Value = 42,

    Doc = #{
        name => Name,
        user => [#{property => UserProp,
                   comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    ok = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, '=', Value]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

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

    #{<<"error">> := #{<<"what">>:= _What,
                       <<"object">>:= _Obj,
                       <<"why">>:= Whys}} = http_post(PostReq, 400),

    ExpectedError = [<<"invalid_enum">>, Comparator, [<<"=">>]],
    ?assert(lists:member(ExpectedError, Whys)),
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

     #{<<"error">> := #{<<"what">>:= _What,
                        <<"object">>:= _Obj,
                        <<"why">>:= Whys}} =
        http_post(PostReq, 400),
    ExpectedError = [<<"missing_required_key">>, <<"property">>],
    ?assert(lists:member(ExpectedError, Whys)),
    unload().

create_feature_user_membership_integer_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"in">>,
    Value = [40, 41, 42],

    Doc = #{
        name => Name,
        user => [#{property => UserProp,
                   comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    ok = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, 'in', Value]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

    unload().

create_feature_user_membership_string_test() ->
    load(),
    Name = <<"feature_name">>,

    UserProp = <<"user_id">>,
    Comparator = <<"in">>,
    Value = [<<"40">>, <<"41">>, <<"42">>],

    Doc = #{
        name => Name,
        user => [#{property => UserProp,
                   comparator => Comparator,
                   value => Value}]
    },

    PostReq = cowboy_test_helpers:req(post, json, Doc),
    ok = http_post(PostReq, 204),
    ?assertEqual({user, [[UserProp, 'in', Value]]},
                 meck:capture(first, features_store, set_feature, '_', 4)),

    unload().

%%%%
%   Test helpers
%%%%

http_post(Req, 204) ->
    http_post(Req, 204, no_body);
http_post(Req, ExpectedCode) ->
    CowPostResp = cowboy_test_helpers:init(?MUT, Req, []),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data = jsx:decode(PostBody, [return_maps]),

    ?assertEqual(ExpectedCode, PostCode),
    Data.

http_post(Req, ExpectedCode, ExpectedBody) ->
    CowPostResp = cowboy_test_helpers:init(?MUT, Req, []),
    {response, PostCode, _PostHeaders, PostBody} = cowboy_test_helpers:read_reply(CowPostResp),
    Data = case PostBody of
        <<"">> -> no_body;
        Content -> jsx:decode(Content, [return_maps])
    end,

    ?assertEqual({ExpectedCode, ExpectedBody}, {PostCode, Data}),
    ok.
