-module(features_count_relay_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_count_relay).

load() ->
    ok = meck:new(hackney),
    ok.

unload() ->
    ?assert(meck:validate(hackney)),
    ok = meck:unload(hackney),
    ok.

basic_case_test() ->
    load(),

    URL = <<"test_url">>,
    EventName = <<"testEventName">>,
    UserId = <<"testUserId">>,

    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    ok = ?MUT:add(EventName, UserId),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{
        ensure_goal => false,
        event_name => EventName,
        namespace => <<"default">>,
        user_id => UserId
    }),
    ExpectedOpts = [{timeout, 1000}],

    meck:wait(hackney, request, '_', 1000),

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

multiple_counts_test() ->
    load(),

    URL = <<"test_url">>,
    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    Adds = [
        {<<"default">>, <<"event_1">>, <<"user_1">>, #{ensure_goal => false}},
        {<<"ns1">>, <<"event_2">>, <<"user_2">>, #{ensure_goal => true}}
    ],

    ok = ?MUT:add(Adds),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{
        events => [
            #{
                <<"event_name">> => <<"event_1">>,
                <<"namespace">> => <<"default">>,
                <<"user_id">> => <<"user_1">>,
                <<"ensure_goal">> => false
            },
            #{
                <<"event_name">> => <<"event_2">>,
                <<"namespace">> => <<"ns1">>,
                <<"user_id">> => <<"user_2">>,
                <<"ensure_goal">> => true
            }
        ]
    }),
    ExpectedOpts = [{timeout, 1000}],

    meck:wait(hackney, request, '_', 1000),

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

ensure_goal_case_test() ->
    load(),

    URL = <<"test_url">>,
    EventName = <<"testEventName">>,
    UserId = <<"testUserId">>,
    EnsureGoal = true,

    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    ok = ?MUT:add(<<"default">>, EventName, UserId, #{ensure_goal => EnsureGoal}),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{
        ensure_goal => true,
        event_name => EventName,
        namespace => <<"default">>,
        user_id => UserId
    }),
    ExpectedOpts = [{timeout, 1000}],

    meck:wait(hackney, request, '_', 1000),

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

int_user_test() ->
    load(),

    URL = <<"test_url">>,
    EventName = <<"testEventName">>,
    UserId = 42,

    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    ok = ?MUT:add(EventName, UserId),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{
        ensure_goal => false,
        event_name => EventName,
        namespace => <<"default">>,
        user_id => <<"42">>
    }),
    ExpectedOpts = [{timeout, 1000}],

    meck:wait(hackney, request, '_', 1000),

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

no_analytics_url_test() ->
    load(),

    EventName = <<"testEventName">>,
    UserId = 42,

    persistent_term:put({features, analytics_url}, undefined),

    ok = ?MUT:add(EventName, UserId),

    unload().

no_analytics_url_multiple_events_test() ->
    load(),

    persistent_term:put({features, analytics_url}, undefined),

    ok = ?MUT:add([]),

    unload().
