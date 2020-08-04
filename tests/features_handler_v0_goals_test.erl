-module(features_handler_v0_goals_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_goals).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, counts, [], #{}),
    ok = meck:expect(features_count_router, add, ['_', '_'], ok),
    ok = meck:expect(features_count_router, add_goal, ['_'], ok),

    ok.

unload() ->
    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ok = ?CTH:cleanup(),
    ok.

setup_test() ->
    load(),
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/goals", Path),
    unload().

%%%%
%   Get goals from router
%%%%

get_goals_test() ->
    load(),
    Goal = <<"test_goal">>,
    ok = meck:expect(features_count_router, goals, [], [Goal]),

    ExpectedData = #{<<"goals">>=>[Goal]},

    Req = ?CTH:req(),
    ?CTH:http_get(?MUT, Req, 200, ExpectedData),

    unload().

%%%%
%   Save goal event
%%%%

save_goal_event_test() ->
    load(),
    EventName = <<"event_name">>,
    Doc = #{
        goal_name => EventName
    },

    PostReq = ?CTH:req(post, json, Doc),

    ?CTH:http_post(?MUT, #{}, PostReq, 204, no_body),

    ?assertEqual(EventName, meck:capture(first, features_count_router, add_goal, '_', 1)),

    unload().
