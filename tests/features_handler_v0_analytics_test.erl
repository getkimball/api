-module(features_handler_v0_analytics_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_analytics).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),
    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, counts, [], #{}),

    ok.

unload() ->
    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ok = ?CTH:cleanup(),
    ok.

setup_test() ->
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/analytics", Path),
    ok.

%%%%
%   Get analytics from router
%%%%


get_boolean_features_test() ->
    load(),
    Feature = <<"feature">>,
    Count = 4,
    ok = meck:expect(features_count_router, counts, [], #{Feature => Count}),

    ExpectedData = #{<<"counts">>=>#{Feature=>Count}},

    Req = cowboy_test_helpers:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

    unload().
