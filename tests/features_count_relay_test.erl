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
    FeatureName = <<"testFeatureName">>,
    UserId = <<"testUserId">>,

    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    ok = ?MUT:add(FeatureName, UserId),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{feature_name => FeatureName,
                                user_id => UserId}),
    ExpectedOpts = [{timeout, 1000}],

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

int_user_test() ->
    load(),

    URL = <<"test_url">>,
    FeatureName = <<"testFeatureName">>,
    UserId = 42,

    ClientRef = make_ref(),

    persistent_term:put({features, analytics_url}, URL),
    meck:expect(hackney, request, ['_', '_', '_', '_', '_'], {ok, 204, [], ClientRef}),
    meck:expect(hackney, body, [ClientRef], {ok, <<"body">>}),

    ok = ?MUT:add(FeatureName, UserId),

    ExpectedHeaders = [{<<"content-type">>, <<"application/json">>}],
    ExpectedBody = jsx:encode(#{feature_name => FeatureName,
                                user_id => <<"42">>}),
    ExpectedOpts = [{timeout, 1000}],

    ?assertEqual(post, meck:capture(first, hackney, request, '_', 1)),
    ?assertEqual(URL, meck:capture(first, hackney, request, '_', 2)),
    ?assertEqual(ExpectedHeaders, meck:capture(first, hackney, request, '_', 3)),
    ?assertEqual(ExpectedBody, meck:capture(first, hackney, request, '_', 4)),
    ?assertEqual(ExpectedOpts, meck:capture(first, hackney, request, '_', 5)),

    unload().

no_analytics_url_test() ->
    load(),

    FeatureName = <<"testFeatureName">>,
    UserId = 42,

    persistent_term:put({features, analytics_url}, undefined),

    ok = ?MUT:add(FeatureName, UserId),

    unload().
