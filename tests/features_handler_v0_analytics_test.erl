-module(features_handler_v0_analytics_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_analytics).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(application, [unstick]),
    ok = meck:expect(application, get_env,
                     [features, analytics_event_mod],
                     {ok, features_count_router}),

    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, counts, [], #{}),
    ok = meck:expect(features_count_router, add, ['_'], ok),
    ok = meck:expect(features_count_router, add, ['_', '_', '_'], ok),

    ok.

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

get_basic_analytics_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    ok = meck:expect(features_count_router, counts, [], [#{name => Feature,
                                                           count => Count,
                                                           single_tag_counts => #{},
                                                           tag_counts => #{}}]),

    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"single_event_counts">> => [],
                                      <<"event_counts">> => []}]},

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData),

    unload().

get_basic_analytics_in_sidecar_mode_404s_test() ->
    load(),
    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_relay},
    ExpectedData = #{<<"error">>=>#{<<"what">> => <<"Daemonset cannot GET analytics">>}},

    ?CTH:http_get(?MUT, State, Req, 404, ExpectedData),

    unload().

get_basic_tag_counts_analytics_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    TagCount = 1,
    TagCounts = #{[] => TagCount},
    ok = meck:expect(features_count_router, counts, [], [#{name => Feature,
                                                           count => Count,
                                                           single_tag_counts => #{},
                                                           tag_counts => TagCounts}]),

    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"single_event_counts">> => [],
                                      <<"event_counts">> => [#{<<"events">> => [],
                                                             <<"count">> => TagCount}]}]},

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData),

    unload().

get_tag_counts_analytics_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    TagCounts = #{[] => 0,
                  [<<"1">>] => 1,
                  [<<"1">>, <<"2">>] => 2},
    STC = #{<<"1">> => 3,
            <<"2">> => 2},
    ok = meck:expect(features_count_router, counts, [], [#{name => Feature,
                                                           count => Count,
                                                           single_tag_counts => STC,
                                                           tag_counts => TagCounts}]),

    ExpectedTagCounts = [
        #{<<"count">> => 2, <<"events">> => [<<"1">>, <<"2">>]},
        #{<<"count">> => 1, <<"events">> => [<<"1">>]},
        #{<<"count">> => 0, <<"events">> => []}
    ],
    ExpectedSTC = [
        #{<<"count">> => 2, <<"event">> => <<"2">>},
        #{<<"count">> => 3, <<"event">> => <<"1">>}
    ],

    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"single_event_counts">> => ExpectedSTC,
                                      <<"event_counts">> => ExpectedTagCounts}]},

    Req = ?CTH:req(),
    State = #{analytics_event_mod => features_count_router},
    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData),

    unload().

%%%%
%   Save analytic event
%%%%

save_analytic_event_test() ->
    load(),
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod=>features_count_router}, PostReq, 204, no_body),

    % features_count_router:add(feature, userid);

    ?assertEqual(1, meck:num_calls(features_count_router, add, [EventName, UserID, #{ensure_goal=>false}])),

    unload().

save_analytic_event_dont_ensure_goal_test() ->
    load(),
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        ensure_goal => false
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod=>features_count_router}, PostReq, 204, no_body),

    ?assertEqual(1, meck:num_calls(features_count_router, add, [EventName, UserID, #{ensure_goal=>false}])),

    unload().

save_analytic_event_ensure_goal_test() ->
    load(),
    EventName = <<"event_name">>,
    UserID = <<"user_id">>,
    Doc = #{
        event_name => EventName,
        user_id => UserID,
        ensure_goal => true
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{analytics_event_mod=>features_count_router}, PostReq, 204, no_body),

    ?assertEqual(1, meck:num_calls(features_count_router, add, [EventName, UserID, #{ensure_goal=>true}])),

    unload().

save_multiple_analytic_events_test() ->
    load(),
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

    ?CTH:http_post(?MUT, #{analytics_event_mod=>features_count_router}, PostReq, 204, no_body),

    Expected = [
      {EventName, UserID, #{ensure_goal => false}},
      {EventName, UserID, #{ensure_goal => false}},
      {EventName, UserID, #{ensure_goal => true}}
    ],

    ?assertEqual(1, meck:num_calls(features_count_router, add, [Expected])),

    unload().

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
    ExpectedResponse = #{<<"error">> =>
                           #{<<"what">> => ErrorMessage,
                             <<"object">> => ErrObj}},

    ?CTH:http_post(?MUT, #{analytics_event_mod=>features_count_router}, PostReq, 400, ExpectedResponse),

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

    State = #{analytics_event_mod=>features_count_router},

    #{<<"error">> := #{<<"what">>:= ErrWhat,
                       <<"object">>:= ErrObj,
                       <<"why">>:= Whys}} = ?CTH:http_post(?MUT, State, PostReq, 400),


    ExpectedError = [<<"missing_required_key">>,<<"event_name">>],
    ?assertEqual(ErrorMessage, ErrWhat),
    ?assertEqual(ExpectedErrObj, ErrObj),
    ?assert(lists:member(ExpectedError, Whys)),

    unload().
