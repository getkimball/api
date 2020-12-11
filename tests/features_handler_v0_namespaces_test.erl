-module(features_handler_v0_namespaces_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, features_handler_v0_namespaces).
-define(CTH, cowboy_test_helpers).

load() ->
    ok = ?CTH:setup(),

    ok = meck:new(features_count_router),
    ok = meck:expect(features_count_router, namespaces, [], []),

    ok.

unload(_) ->
    unload().

unload() ->
    ?assert(meck:validate(features_count_router)),
    ok = meck:unload(features_count_router),

    ok = ?CTH:cleanup(),
    ok.

setup_test() ->
    load(),
    [Trail] = ?MUT:trails(),
    Path = trails:path_match(Trail),
    ?assertEqual("/v0/namespaces", Path),
    unload().

%%%%
%   Get goals from router
%%%%

get_namespaces_test_() ->
    {foreach, fun load/0, fun unload/1, [
        fun get_namespaces/0
    ]}.

get_namespaces() ->
    Namespace = <<"test namespace">>,
    ok = meck:expect(features_count_router, namespaces, [], [Namespace]),

    ExpectedData = #{<<"namespaces">> => [Namespace]},

    Req = ?CTH:req(),
    State = #{},

    ?CTH:http_get(?MUT, State, Req, 200, ExpectedData).
