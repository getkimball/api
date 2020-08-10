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
    ok = meck:expect(features_count_router, add, ['_', '_'], ok),

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
                                                           tag_counts => #{}}]),

    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"event_counts">> => []}]},

    Req = ?CTH:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

    unload().

get_basic_tag_counts_analytics_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    TagCount = 1,
    TagCounts = #{[] => TagCount},
    ok = meck:expect(features_count_router, counts, [], [#{name => Feature,
                                                           count => Count,
                                                           tag_counts => TagCounts}]),

    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"event_counts">> => [#{<<"events">> => [],
                                                             <<"count">> => TagCount}]}]},

    Req = ?CTH:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

    unload().

get_tag_counts_analytics_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    TagCounts = #{[] => 0,
                  [<<"1">>] => 1,
                  [<<"1">>, <<"2">>] => 2},
    ok = meck:expect(features_count_router, counts, [], [#{name => Feature,
                                                           count => Count,
                                                           tag_counts => TagCounts}]),

    ExpectedTagCounts = [
        #{<<"count">> => 2, <<"events">> => [<<"1">>, <<"2">>]},
        #{<<"count">> => 1, <<"events">> => [<<"1">>]},
        #{<<"count">> => 0, <<"events">> => []}
    ],
    ExpectedData = #{<<"counts">>=>[#{<<"name">> => Feature,
                                      <<"count">> => Count,
                                      <<"event_counts">> => ExpectedTagCounts}]},

    Req = ?CTH:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

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

    % features_count_router:add(Feature, UserId);

    ?assertEqual(EventName, meck:capture(first, features_count_router, add, '_', 1)),
    ?assertEqual(UserID, meck:capture(first, features_count_router, add, '_', 2)),

    unload().
