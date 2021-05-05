-module(features_handler_v0_analytics_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_analytics).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(application, [unstick]),
    ok = meck:expect(
        application,
        get_env,
        [features, analytics_event_mod],
        {ok, features_count_router}
    ),

    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, counts, ['_'], #{}),
    ok = meck:expect(features_count_router, add, ['_'], ok),
    ok = meck:expect(features_count_router, add, ['_', '_', '_', '_'], ok),

    ok.

unload(_) ->
    unload().

unload() ->
    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ?assert(meck:validate(application)),
    ok = meck:unload(application),

    ok = ?CTH:cleanup(),
    ok.

setup_test() ->
    load(),
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/analytics", Path),
    unload().

%%%%
%   Get analytics from router
%%%%
%%%
get_analytics_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_basic_analytics/0,
        fun get_namespaced_analytics/0,
        fun get_basic_analytics_in_sidecar_mode_404s/0,
        fun get_basic_tag_counts_analytics/0,
        fun get_date_cohort_tag_counts_analytics/0,
        fun get_tag_counts_analytics/0
    ]}.

get_basic_analytics() ->
    Feature = <<"feature">>,
    ID = features_counter_id:create(Feature),
    Count = 4,
    ok = meck:expect(features_count_router, counts, [<<"default">>], [
        #{
            id => ID,
            count => Count,
            single_tag_counts => #{},
            value => #{},
            tag_counts => #{}
        }
    ]),

    ExpectedData = #{
        <<"counts">> => [
            #{
                <<"name">> => Feature,
                <<"count">> => Count,
                <<"single_event_counts">> => [],
                <<"type">> => <<"named">>,
                <<"value">> => #{},
                <<"event_counts">> => []
            }
        ]
    },

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_namespaced_analytics() ->
    Feature = <<"feature">>,
    ID = features_counter_id:create(Feature),
    Namespace = <<"not default">>,
    Count = 4,
    ok = meck:expect(features_count_router, counts, [Namespace], [
        #{
            id => ID,
            count => Count,
            single_tag_counts => #{},
            value => #{},
            tag_counts => #{}
        }
    ]),

    ExpectedData = #{
        <<"counts">> => [
            #{
                <<"name">> => Feature,
                <<"count">> => Count,
                <<"single_event_counts">> => [],
                <<"type">> => <<"named">>,
                <<"value">> => #{},
                <<"event_counts">> => []
            }
        ]
    },

    Req = ?CTH:req(get, [{<<"namespace">>, Namespace}]),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData),

    test_utils:assertNCalls(1, features_count_router, counts, [Namespace]).

get_basic_analytics_in_sidecar_mode_404s() ->
    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_relay},
    ExpectedData = #{<<"error">> => #{<<"what">> => <<"Daemonset cannot GET analytics">>}},

    ?CTH:http_get(?MUT, State, Req, 404, ExpectedData).

get_basic_tag_counts_analytics() ->
    Feature = <<"feature">>,
    ID = features_counter_id:create(Feature),
    Count = 4,
    TagCount = 1,
    TagCounts = #{[] => TagCount},
    ok = meck:expect(features_count_router, counts, ['_'], [
        #{
            id => ID,
            count => Count,
            single_tag_counts => #{},
            value => #{},
            tag_counts => TagCounts
        }
    ]),

    ExpectedData = #{
        <<"counts">> => [
            #{
                <<"name">> => Feature,
                <<"count">> => Count,
                <<"single_event_counts">> => [],
                <<"type">> => <<"named">>,
                <<"value">> => #{},
                <<"event_counts">> => [
                    #{
                        <<"events">> => [],
                        <<"count">> => TagCount
                    }
                ]
            }
        ]
    },

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_date_cohort_tag_counts_analytics() ->
    Name = <<"feature">>,
    ID = features_counter_id:create(<<"default">>, Name, weekly, {2020, 1}),
    Count = 4,
    TagCount = 1,
    TagCounts = #{[] => TagCount},
    ok = meck:expect(features_count_router, counts, ['_'], [
        #{
            id => ID,
            count => Count,
            single_tag_counts => #{},
            value => #{},
            tag_counts => TagCounts
        }
    ]),

    ExpectedData = #{
        <<"counts">> => [
            #{
                <<"name">> => <<"feature 20201">>,
                <<"count">> => Count,
                <<"single_event_counts">> => [],
                <<"type">> => <<"weekly">>,
                <<"value">> => #{},
                <<"event_counts">> => [
                    #{
                        <<"events">> => [],
                        <<"count">> => TagCount
                    }
                ]
            }
        ]
    },

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

get_tag_counts_analytics() ->
    Feature = <<"feature">>,
    ID = features_counter_id:create(Feature),
    Count = 4,
    TagCounts = #{
        [] => 0,
        [<<"1">>] => 1,
        [<<"1">>, <<"2">>] => 2
    },
    STC = #{
        <<"1">> => 3,
        <<"2">> => 2
    },
    ok = meck:expect(features_count_router, counts, ['_'], [
        #{
            id => ID,
            count => Count,
            single_tag_counts => STC,
            value => #{},
            tag_counts => TagCounts
        }
    ]),

    ExpectedTagCounts = [
        #{<<"count">> => 2, <<"events">> => [<<"1">>, <<"2">>]},
        #{<<"count">> => 1, <<"events">> => [<<"1">>]},
        #{<<"count">> => 0, <<"events">> => []}
    ],
    ExpectedSTC = [
        #{<<"count">> => 2, <<"name">> => <<"2">>},
        #{<<"count">> => 3, <<"name">> => <<"1">>}
    ],

    ExpectedData = #{
        <<"counts">> => [
            #{
                <<"name">> => Feature,
                <<"count">> => Count,
                <<"single_event_counts">> => ExpectedSTC,
                <<"type">> => <<"named">>,
                <<"value">> => #{},
                <<"event_counts">> => ExpectedTagCounts
            }
        ]
    },

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).

%%%%
%   Save analytic event
%%%%

save_analytic_event_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun save_analytic_event/0,
        fun save_analytic_event_with_namespace/0,
        fun save_analytic_event_with_datetime/0,
        fun save_analytic_event_with_invalid_datetime/0,
        fun save_analytic_event_dont_ensure_goal/0,
        fun save_analytic_event_ensure_goal/0,
        fun save_multiple_analytic_events/0,
        fun save_multiple_analytic_events_with_namespaces/0,
        fun save_list_multiple_analytic_events/0,
        fun save_list_multiple_analytic_events_with_namespaces/0
    ]}.

save_analytic_event() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    % features_count_router:add(feature, userid);

    ?assertEqual(
        1,
        meck:num_calls(features_count_router, add, [
            <<"default">>,
            EventName,
            UserID,
            #{ensure_goal => false}
        ])
    ).

save_analytic_event_with_namespace() ->
    EventName = <<"event_name">>,
    Namespace = <<"test namespace">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        namespace => Namespace,
        user_id => UserID
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    % features_count_router:add(feature, userid);

    ?assertEqual(
        1,
        meck:num_calls(features_count_router, add, [
            Namespace,
            EventName,
            UserID,
            #{ensure_goal => false}
        ])
    ).

save_analytic_event_with_datetime() ->
    Namespace = <<"default">>,
    EventName = <<"event_name">>,
    Now = erlang:system_time(seconds),
    NowRFC = binary:list_to_bin(calendar:system_time_to_rfc3339(Now)),
    {YMD, _Time} = calendar:system_time_to_universal_time(Now, seconds),
    UserID = <<"user_id">>,
    Doc = #{
        datetime => NowRFC,
        event_name => EventName,
        user_id => UserID
    },
    io:format("NowRFC ~p~n", [{NowRFC, YMD}]),

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    Expected = [
        Namespace,
        EventName,
        UserID,
        #{
            ensure_goal => false,
            date => YMD,
            value => undefined
        }
    ],

    test_utils:assertNCalls(1, features_count_router, add, Expected).

save_analytic_event_with_invalid_datetime() ->
    EventName = <<"event_name">>,
    Namespace = <<"test namespace">>,
    UserID = <<"user_id">>,
    InvalidDateTime = <<"invalid datetime">>,
    Doc = #{
        datetime => InvalidDateTime,
        event_name => EventName,
        namespace => Namespace,
        user_id => UserID
    },

    PostReq = ?CTH:req(post, json, Doc),

    ExpectedErrObj = ?CTH:json_roundtrip(Doc),

    ErrorMessage = <<"Object does not match any of the oneOf specifications">>,
    State = #{analytics_event_mod => features_count_router},
    #{
        <<"error">> := #{
            <<"what">> := ErrWhat,
            <<"object">> := ErrObj,
            <<"why">> := Whys
        }
    } = ?CTH:http_post(?MUT, State, PostReq, 400),

    ExpectedError = [<<"invalid_date">>,<<"invalid datetime">>],
    ?assertEqual(ErrorMessage, ErrWhat),
    ?assertEqual(ExpectedErrObj, ErrObj),

    io:format("Whys ~p~n", [Whys]),
    ?assert(lists:member(ExpectedError, Whys)).

save_analytic_event_dont_ensure_goal() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        ensure_goal => false
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    ?assertEqual(
        1,
        meck:num_calls(features_count_router, add, [
            <<"default">>,
            EventName,
            UserID,
            #{ensure_goal => false}
        ])
    ).

save_analytic_event_ensure_goal() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        ensure_goal => true
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    ?assertEqual(
        1,
        meck:num_calls(features_count_router, add, [
            <<"default">>,
            EventName,
            UserID,
            #{ensure_goal => true}
        ])
    ).

save_multiple_analytic_events() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        events => [
            #{event_name => EventName, user_id => UserID},
            #{event_name => EventName, user_id => UserID, ensure_goal => false},
            #{event_name => EventName, user_id => UserID, ensure_goal => true}
        ]
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    Expected = [
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"default">>, EventName, UserID, #{ensure_goal => true}}
    ],

    ?assertEqual(1, meck:num_calls(features_count_router, add, [Expected])).

save_multiple_analytic_events_with_namespaces() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        events => [
            #{namespace => <<"ns1">>, event_name => EventName, user_id => UserID},
            #{event_name => EventName, user_id => UserID, ensure_goal => false},
            #{
                namespace => <<"ns2">>,
                event_name => EventName,
                user_id => UserID,
                ensure_goal => true
            }
        ]
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    Expected = [
        {<<"ns1">>, EventName, UserID, #{ensure_goal => false}},
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"ns2">>, EventName, UserID, #{ensure_goal => true}}
    ],

    test_utils:assertNCalls(1, features_count_router, add, [Expected]).

save_list_multiple_analytic_events() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = [
        #{event_name => EventName, user_id => UserID},
        #{event_name => EventName, user_id => UserID, ensure_goal => false},
        #{event_name => EventName, user_id => UserID, ensure_goal => true}
    ],

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    Expected = [
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"default">>, EventName, UserID, #{ensure_goal => true}}
    ],

    ?assertEqual(1, meck:num_calls(features_count_router, add, [Expected])).

save_list_multiple_analytic_events_with_namespaces() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = [
        #{event_name => EventName, user_id => UserID},
        #{namespace => <<"ns1">>, event_name => EventName, user_id => UserID, ensure_goal => false},
        #{namespace => <<"ns2">>, event_name => EventName, user_id => UserID, ensure_goal => true}
    ],

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    Expected = [
        {<<"default">>, EventName, UserID, #{ensure_goal => false}},
        {<<"ns1">>, EventName, UserID, #{ensure_goal => false}},
        {<<"ns2">>, EventName, UserID, #{ensure_goal => true}}
    ],

    test_utils:assertNCalls(1, features_count_router, add, [Expected]).

value_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun save_analytic_event_int_value/0,
        fun save_analytic_event_float_value/0,
        fun save_analytic_event_not_number_value/0
    ]}.

save_analytic_event_int_value() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Value = 1,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        value => Value
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    assert_added(EventName, UserID, #{ensure_goal => false, value => Value}, 1).

save_analytic_event_float_value() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Value = 1.1,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        value => Value
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod => features_count_router}, PostReq, 204, no_body),

    assert_added(EventName, UserID, #{ensure_goal => false, value => Value}, 1).

save_analytic_event_not_number_value() ->
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Value = <<"foo">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        value => Value
    },

    PostReq = ?CTH:req(post, json, Doc),

    ExpectedErrObj = ?CTH:json_roundtrip(Doc),

    ErrorMessage = <<"Object does not match any of the oneOf specifications">>,
    State = #{analytics_event_mod => features_count_router},
    #{
        <<"error">> := #{
            <<"what">> := ErrWhat,
            <<"object">> := ErrObj,
            <<"why">> := Whys
        }
    } = ?CTH:http_post(?MUT, State, PostReq, 400),

    ExpectedError = [<<"incorrect_type">>, Value, <<"number">>],
    ?assertEqual(ErrorMessage, ErrWhat),
    ?assertEqual(ExpectedErrObj, ErrObj),

    ?assert(lists:member(ExpectedError, Whys)),

    ?assertEqual(0, meck:num_calls(features_count_router, add, '_')).

invalid_combination_of_single_and_multiple_events_test() ->
    load(),
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        events => [
            #{event_name => EventName, user_id => UserID}
        ]
    },

    PostReq = ?CTH:req(post, json, Doc),

    ErrObj = ?CTH:json_roundtrip(Doc),

    ErrorMessage = <<"Object matches more than oneOf specifications">>,
    ExpectedResponse = #{
        <<"error">> => #{
            <<"what">> => ErrorMessage,
            <<"object">> => ErrObj
        }
    },

    ?CTH:http_post(
        ?MUT,
        #{analytics_event_mod => features_count_router},
        PostReq,
        400,
        ExpectedResponse
    ),

    ?assertEqual(0, meck:num_calls(features_count_router, add, '_')),

    unload().

non_matching_event_of_multiple_events_test() ->
    load(),
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        events => [
            #{event_name => EventName, user_id => UserID},
            #{event => EventName, user_id => UserID}
        ]
    },

    PostReq = ?CTH:req(post, json, Doc),

    ExpectedErrObj = ?CTH:json_roundtrip(Doc),

    ErrorMessage = <<"Object does not match any of the oneOf specifications">>,

    State = #{analytics_event_mod => features_count_router},

    #{
        <<"error">> := #{
            <<"what">> := ErrWhat,
            <<"object">> := ErrObj,
            <<"why">> := Whys
        }
    } = ?CTH:http_post(?MUT, State, PostReq, 400),

    ExpectedError = [<<"missing_required_key">>, <<"event_name">>],
    ?assertEqual(ErrorMessage, ErrWhat),
    ?assertEqual(ExpectedErrObj, ErrObj),
    ?assert(lists:member(ExpectedError, Whys)),

    unload().

assert_added(Event, UserID, Opts, Num) ->
    io:format("features_count_router history: ~p~n", [meck:history(features_count_router)]),

    ?assertEqual(
        Num,
        meck:num_calls(features_count_router, add, [<<"default">>, Event, UserID, Opts])
    ).
